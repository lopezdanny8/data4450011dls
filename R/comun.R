# =============================================================================
# comun.R - datos, particiones y modelos compartidos por los scripts 04 en adelante
# =============================================================================
suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(sf)
  library(recipes); library(rsample); library(yardstick); library(spatialsample)
  library(ranger); library(xgboost); library(gstat); library(spacetime); library(sp); library(FNN)
})

MANEJO <- c("superficie_media_ha", "prop_riego", "prop_semilla_cert", "prop_papa_nativa", "prop_abono",
            "prop_fertilizante", "prop_plaguicida", "prop_asistencia", "prop_capacitacion", "prop_asociatividad")
CLIMA  <- c("pp_campana", "pp_siembra", "pp_crecimiento", "anom_pp_campana", "meses_secos",
            "tmin_crecimiento", "tmax_crecimiento", "amplitud_termica", "tmin_minima", "meses_helada")
PRED   <- c(MANEJO, CLIMA)          # 20 predictores comunes a todos los modelos
ANIOS  <- 2015:2025                 # eje temporal (2020 sin ENA)
K_VEC  <- 5                         # vecinos para Random Forest espacial (RFSI)

# ---------------------------------------------------------------- datos
# base completa (scripts 00-03, requiere la ENA original) o base ligera incluida en el repositorio
ruta_db <- function(raiz) {
  completa <- file.path(raiz, "data", "ena_papa.duckdb")
  ligera   <- file.path(raiz, "data", "ena_papa_ligera.duckdb")
  if (Sys.getenv("USAR_BASE_LIGERA") == "1" || !file.exists(completa)) ligera else completa
}
cargar_base <- function(raiz, min_productores = 5) {
  con <- dbConnect(duckdb(), ruta_db(raiz), read_only = TRUE)
  on.exit(dbDisconnect(con, shutdown = TRUE))
  d <- dbGetQuery(con, sprintf("SELECT * FROM analisis.base WHERE region = 'Sierra' AND n_productores >= %d", min_productores))
  p <- st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE), 32718)
  xy <- st_coordinates(p) / 1000                 # km, UTM 18S
  d$x <- xy[, 1]; d$y <- xy[, 2]
  d$y_obs <- d$rendimiento_t_ha
  d[order(d$anio, d$ubigeo), ]
}

# ---------------------------------------------------------------- particiones
# cada partición: lista de pliegues con índices de entrenamiento y prueba sobre `d`
particion_temporal <- function(d, anios_prueba = c(2019, 2021, 2022, 2023, 2024)) {
  lapply(anios_prueba, function(a) list(id = paste0("T", a), train = which(d$anio < a), test = which(d$anio == a)))
}
particion_espacial <- function(d, v = 5, semilla = 2026) {
  dist <- d |> distinct(ubigeo, x, y)
  s <- st_as_sf(dist, coords = c("x", "y"), crs = NA)
  set.seed(semilla)
  cv <- spatial_clustering_cv(s, v = v)            # k-medias sobre coordenadas: bloques de distritos vecinos
  lapply(seq_len(v), function(k) {
    ub_test <- dist$ubigeo[complement(cv$splits[[k]])]
    list(id = paste0("E", k), train = which(!d$ubigeo %in% ub_test), test = which(d$ubigeo %in% ub_test))
  })
}
particion_futuro <- function(d) list(list(id = "F2025", train = which(d$anio <= 2024), test = which(d$anio == 2025)))

# ---------------------------------------------------------------- preprocesamiento
# transformación de potencia (Yeo-Johnson) + estandarización, como el PowerTransformer del artículo base
receta <- function(train, vars) {
  recipe(as.formula(paste("y_obs ~", paste(vars, collapse = " + "))), data = train) |>
    step_impute_median(all_predictors()) |>
    # las coordenadas UTM (miles de km) no se transforman: Yeo-Johnson las vuelve constantes
    step_YeoJohnson(all_predictors(), -any_of(c("x", "y"))) |>
    step_normalize(all_predictors()) |>
    prep(training = train)
}
matriz <- function(rec, df) as.data.frame(bake(rec, new_data = df, all_predictors()))

# selección de las K mejores (equivalente a SelectKBest con f_regression: |correlación| con y)
k_mejores <- function(X, y, k) names(sort(abs(cor(X, y)[, 1]), decreasing = TRUE))[seq_len(min(k, ncol(X)))]

# ---------------------------------------------------------------- vecinos (RFSI)
# para cada fila: rendimiento y distancia de los K distritos de entrenamiento más cercanos (excluido el propio),
# en el mismo año si está en entrenamiento; si no, en el año de entrenamiento más cercano anterior
vecinos <- function(train, objetivo, k = K_VEC) {
  out <- matrix(NA_real_, nrow(objetivo), 2 * k, dimnames = list(NULL, c(paste0("nn", 1:k), paste0("dnn", 1:k))))
  anios_tr <- sort(unique(train$anio))
  for (a in unique(objetivo$anio)) {
    a_ref <- if (a %in% anios_tr) a else max(c(anios_tr[anios_tr < a], min(anios_tr)))
    pool <- train[train$anio == a_ref, ]
    filas <- which(objetivo$anio == a)
    nn <- get.knnx(as.matrix(pool[, c("x", "y")]), as.matrix(objetivo[filas, c("x", "y")]), k = k + 1)
    for (j in seq_along(filas)) {
      idx <- nn$nn.index[j, ]; dis <- nn$nn.dist[j, ]
      propio <- pool$ubigeo[idx] == objetivo$ubigeo[filas[j]]
      idx <- idx[!propio][1:k]; dis <- dis[!propio][1:k]
      out[filas[j], ] <- c(pool$y_obs[idx], dis)
    }
  }
  out
}

# ---------------------------------------------------------------- kriging espacio-temporal de residuos
# datos en STSDF (distritos x años 2015-2025); variograma separable ajustado a los residuos de entrenamiento
st_obj <- function(df, valores) {
  dist <- df |> distinct(ubigeo, x, y)
  sp_pts <- SpatialPoints(as.matrix(dist[, c("x", "y")]))
  tiempo <- as.POSIXct(paste0(ANIOS, "-07-01"), tz = "UTC")
  idx <- cbind(match(df$ubigeo, dist$ubigeo), match(df$anio, ANIOS))
  STSDF(sp_pts, tiempo, data.frame(r = valores), index = idx)
}
ajustar_vario <- function(st_tr) {
  # rezagos temporales hasta 4 años, sin pasar el rango de años con datos (p. ej. 2015-2018 en el pliegue T2019)
  t_max <- min(4, diff(range(st_tr@index[, 2])))
  ev <- variogramST(r ~ 1, data = st_tr, tlags = 0:t_max, width = 25, cutoff = 250, progress = FALSE)
  s2 <- var(st_tr@data$r)
  # gstat mide los rezagos temporales en días: rango temporal inicial 2 años = 730 días
  ini <- vgmST("separable", space = vgm(0.8, "Exp", 60, 0.2), time = vgm(0.8, "Exp", 730, 0.2), sill = s2)
  fit <- fit.StVariogram(ev, ini, method = "L-BFGS-B",
                         lower = c(5, 0, 30, 0, 0.01 * s2), upper = c(500, 1, 20 * 365, 1, 3 * s2))
  list(emp = ev, modelo = fit, sse = attr(fit, "optim.output")$value)
}
krigear <- function(st_tr, vario, test) {
  nuevo <- STIDF(SpatialPoints(as.matrix(test[, c("x", "y")])),
                 as.POSIXct(paste0(test$anio, "-07-01"), tz = "UTC"), data.frame(id = seq_len(nrow(test))))
  rs <- vario$modelo$space$range[2]; rt <- vario$modelo$time$range[2]
  k <- krigeST(r ~ 1, data = st_tr, newdata = nuevo, modelList = vario$modelo,
               nmax = 50, stAni = rs / rt, computeVar = TRUE, progress = FALSE)
  list(pred = k@data$var1.pred, var = k@data$var1.var)
}

# ---------------------------------------------------------------- modelos
# cada modelo recibe (train, test, hp) y devuelve list(pred, var = NULL)
modelos <- list(
  # réplica del artículo base: potencia + K mejores + Random Forest
  base_rf = function(train, test, hp) {
    rec <- receta(train, PRED); Xtr <- matriz(rec, train); Xte <- matriz(rec, test)
    sel <- k_mejores(Xtr, train$y_obs, hp$k)
    m <- ranger(x = Xtr[, sel, drop = FALSE], y = train$y_obs, num.trees = 500, mtry = min(hp$mtry, length(sel)),
                min.node.size = hp$nodo, seed = 1, num.threads = 8)
    list(pred = predict(m, Xte[, sel, drop = FALSE])$predictions)
  },
  # réplica del artículo base: potencia + K mejores + gradient boosting
  base_gb = function(train, test, hp) {
    rec <- receta(train, PRED); Xtr <- matriz(rec, train); Xte <- matriz(rec, test)
    sel <- k_mejores(Xtr, train$y_obs, hp$k)
    m <- xgboost(data = as.matrix(Xtr[, sel]), label = train$y_obs, nrounds = hp$n, max_depth = hp$prof, eta = 0.05,
                 subsample = 0.8, colsample_bytree = 0.8, objective = "reg:squarederror", verbose = 0, nthread = 8)
    list(pred = predict(m, as.matrix(Xte[, sel])))
  },
  # XGBoost con coordenadas
  xgb = function(train, test, hp) {
    rec <- receta(train, c(PRED, "x", "y")); Xtr <- matriz(rec, train); Xte <- matriz(rec, test)
    m <- xgboost(data = as.matrix(Xtr), label = train$y_obs, nrounds = hp$n, max_depth = hp$prof, eta = 0.05,
                 subsample = 0.8, colsample_bytree = 0.8, objective = "reg:squarederror", verbose = 0, nthread = 8)
    list(pred = predict(m, as.matrix(Xte)))
  },
  # Random Forest espacial: coordenadas + rendimiento y distancia de vecinos (RFSI, Sekulić et al. 2020)
  rf_esp = function(train, test, hp) {
    rec <- receta(train, c(PRED, "x", "y")); Xtr <- matriz(rec, train); Xte <- matriz(rec, test)
    Xtr <- cbind(Xtr, vecinos(train, train)); Xte <- cbind(Xte, vecinos(train, test))
    m <- ranger(x = Xtr, y = train$y_obs, num.trees = 500, mtry = hp$mtry, min.node.size = hp$nodo, seed = 1, num.threads = 8)
    list(pred = predict(m, Xte)$predictions)
  },
  # kriging espacio-temporal con regresión: tendencia lineal + kriging de residuos
  rk_st = function(train, test, hp) {
    rec <- receta(train, PRED); Xtr <- matriz(rec, train); Xte <- matriz(rec, test)
    lmf <- lm(train$y_obs ~ ., data = Xtr)
    st_tr <- st_obj(train, residuals(lmf)); vg <- ajustar_vario(st_tr); kr <- krigear(st_tr, vg, test)
    list(pred = predict(lmf, Xte) + kr$pred, var = kr$var, vario = vg)
  },
  # híbrido: Random Forest (tendencia) + kriging espacio-temporal de sus residuos fuera de bolsa
  rf_rkst = function(train, test, hp) {
    rec <- receta(train, PRED); Xtr <- matriz(rec, train); Xte <- matriz(rec, test)
    m <- ranger(x = Xtr, y = train$y_obs, num.trees = 500, mtry = hp$mtry, min.node.size = hp$nodo, seed = 1, num.threads = 8)
    st_tr <- st_obj(train, train$y_obs - m$predictions); vg <- ajustar_vario(st_tr); kr <- krigear(st_tr, vg, test)
    list(pred = predict(m, Xte)$predictions + kr$pred, var = kr$var, vario = vg)
  }
)

metricas <- function(obs, pred) {
  tibble(rmse = sqrt(mean((obs - pred)^2)), mae = mean(abs(obs - pred)),
         r2 = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2), n = length(obs))
}
