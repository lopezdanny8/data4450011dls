# =============================================================================
# 09 - Validación robusta
# 1. Validación cruzada ANIDADA: en cada pliegue externo (temporal y espacial) los
#    hiperparámetros se eligen con una validación temporal interna que usa solo los datos
#    de entrenamiento de ese pliegue. El pliegue de prueba nunca participa en el ajuste.
# 2. Bootstrap por distrito (2 000 réplicas): IC 95 % del RMSE, del R2 y de la mejora
#    del mejor modelo frente a la réplica RF, en cada esquema.
# 3. Índice de Moran de los residuos de 2025 (5 vecinos más cercanos, prueba por permutación).
# =============================================================================
raiz <- local({  # raíz del proyecto (carpeta con R/comun.R): Rscript, Positron, RStudio y Quarto
  a <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (d in c(dirname(a), getwd())) {
    d <- normalizePath(d, winslash = "/", mustWork = FALSE)
    while (!file.exists(file.path(d, "R", "comun.R")) && dirname(d) != d) d <- dirname(d)
    if (file.exists(file.path(d, "R", "comun.R"))) return(d)
  }
  stop("No se encontró la raíz del proyecto (carpeta que contiene R/comun.R)")
})
source(file.path(raiz, "R", "comun.R"))
res <- file.path(raiz, "results")
set.seed(2026)

d   <- cargar_base(raiz)
dev <- d[d$anio <= 2024, ]

# ---------------------------------------------------------------- grillas (las mismas del script 04)
rf_trend <- function(train, test, hp) {
  rec <- receta(train, PRED); Xtr <- matriz(rec, train); Xte <- matriz(rec, test)
  m <- ranger(x = Xtr, y = train$y_obs, num.trees = 500, mtry = hp$mtry, min.node.size = hp$nodo, seed = 1, num.threads = 8)
  list(pred = predict(m, Xte)$predictions)
}
grillas <- list(
  base_rf = expand.grid(k = c(10, 15, 20), mtry = c(3, 6), nodo = c(5, 10)),
  base_gb = expand.grid(k = c(10, 15, 20), n = c(200, 400, 800), prof = c(3, 5)),
  xgb     = expand.grid(n = c(200, 400, 800), prof = c(3, 5)),
  rf_esp  = expand.grid(mtry = c(4, 8, 12), nodo = c(5, 10)),
  rf_rkst = expand.grid(mtry = c(4, 8, 12), nodo = c(5, 10)))

# pliegues temporales internos: los (hasta) 3 últimos años del entrenamiento con >= 2 años previos
internos <- function(train) {
  anios <- sort(unique(train$anio))
  cand <- anios[sapply(anios, function(a) sum(anios < a) >= 2)]
  lapply(tail(cand, 3), function(a) list(train = which(train$anio < a), test = which(train$anio == a)))
}
afinar <- function(m, train) {
  if (m == "rk_st") return(list())
  g <- grillas[[m]]; f <- if (m == "rf_rkst") rf_trend else modelos[[m]]
  pl <- internos(train)
  err <- sapply(seq_len(nrow(g)), function(i) {
    hp <- as.list(g[i, , drop = FALSE])
    mean(sapply(pl, function(p) { r <- f(train[p$train, ], train[p$test, ], hp); sqrt(mean((train$y_obs[p$test] - r$pred)^2)) }))
  })
  as.list(g[which.min(err), , drop = FALSE])
}

# ---------------------------------------------------------------- 1. validación anidada
externos <- list(temporal = particion_temporal(dev), espacial = particion_espacial(dev))
t0 <- Sys.time(); preds <- list(); hp_log <- list()
for (e in names(externos)) for (p in externos[[e]]) {
  tr <- dev[p$train, ]; te <- dev[p$test, ]
  for (m in names(modelos)) {
    hp <- afinar(m, tr)
    r <- modelos[[m]](tr, te, hp)
    preds[[length(preds) + 1]] <- data.frame(esquema = e, pliegue = p$id, modelo = m, ubigeo = te$ubigeo, anio = te$anio,
                                             obs = te$y_obs, pred = r$pred)
    hp_log[[length(hp_log) + 1]] <- data.frame(esquema = e, pliegue = p$id, modelo = m,
                                               hp = if (length(hp)) paste(names(hp), unlist(hp), sep = "=", collapse = ", ") else "-")
  }
  message(sprintf("%-8s %-6s listo (%.1f min)", e, p$id, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
PA <- bind_rows(preds)
write.csv(PA, file.path(res, "predicciones_anidada.csv"), row.names = FALSE)
write.csv(bind_rows(hp_log), file.path(res, "hiperparametros_anidada.csv"), row.names = FALSE)

# el esquema futuro no cambia: sus hiperparámetros ya se eligieron solo con 2015-2024
PF <- read.csv(file.path(res, "predicciones.csv"), colClasses = c(ubigeo = "character")) |> filter(esquema == "futuro") |>
  select(esquema, pliegue, modelo, ubigeo, anio, obs, pred)
PT <- bind_rows(PA, PF)

pp <- PT |> group_by(esquema, pliegue, modelo) |> group_modify(~ metricas(.x$obs, .x$pred)) |> ungroup()
resumen <- pp |> group_by(esquema, modelo) |>
  summarise(rmse_de = sd(rmse), r2_de = sd(r2), rmse = mean(rmse), mae = mean(mae), r2 = mean(r2), .groups = "drop")
write.csv(resumen, file.path(res, "metricas_resumen_anidada.csv"), row.names = FALSE)

# ---------------------------------------------------------------- 2. bootstrap por distrito
mejor <- c(temporal = "rf_esp", espacial = "rf_rkst", futuro = "rf_esp")
B <- 2000
boot <- lapply(names(mejor), function(e) {
  a <- PT |> filter(esquema == e, modelo == mejor[[e]]) |> arrange(pliegue, ubigeo, anio)
  b <- PT |> filter(esquema == e, modelo == "base_rf") |> arrange(pliegue, ubigeo, anio)
  stopifnot(all(a$ubigeo == b$ubigeo), all(a$anio == b$anio))
  grupos <- split(seq_len(nrow(a)), a$ubigeo)              # remuestreo por distrito (conglomerado)
  r <- replicate(B, {
    i <- unlist(sample(grupos, length(grupos), replace = TRUE), use.names = FALSE)
    rm_a <- sqrt(mean((a$obs[i] - a$pred[i])^2)); rm_b <- sqrt(mean((b$obs[i] - b$pred[i])^2))
    c(rmse = rm_a, r2 = 1 - sum((a$obs[i] - a$pred[i])^2) / sum((a$obs[i] - mean(a$obs[i]))^2), mejora = 100 * (rm_b - rm_a) / rm_b)
  })
  est <- c(rmse = sqrt(mean((a$obs - a$pred)^2)), r2 = 1 - sum((a$obs - a$pred)^2) / sum((a$obs - mean(a$obs))^2),
           mejora = 100 * (sqrt(mean((b$obs - b$pred)^2)) - sqrt(mean((a$obs - a$pred)^2))) / sqrt(mean((b$obs - b$pred)^2)))
  w <- wilcox.test(abs(a$obs - a$pred), abs(b$obs - b$pred), paired = TRUE)
  data.frame(esquema = e, modelo = mejor[[e]], metrica = names(est), estimado = est,
             ic_inf = apply(r, 1, quantile, 0.025), ic_sup = apply(r, 1, quantile, 0.975), p_wilcoxon = w$p.value, n = nrow(a))
})
boot <- bind_rows(boot)
write.csv(boot, file.path(res, "bootstrap_ic.csv"), row.names = FALSE)

# ---------------------------------------------------------------- 3. Moran de los residuos de 2025
moran <- function(r, xy, k = 5, perm = 999) {
  nn <- get.knn(xy, k = k)$nn.index
  z <- r - mean(r)
  I <- function(z) (length(z) / (length(z) * k)) * sum(z * rowSums(matrix(z[nn], ncol = k))) / sum(z^2) * 1
  obs <- I(z)
  sim <- replicate(perm, I(sample(z)))
  c(I = obs, p = (sum(sim >= obs) + 1) / (perm + 1))
}
f25 <- d[d$anio == 2025, c("ubigeo", "x", "y")]
mor <- bind_rows(lapply(c("rf_esp", "base_rf", "rf_rkst"), function(m) {
  a <- PF |> filter(modelo == m) |> inner_join(f25, by = "ubigeo")
  v <- moran(a$obs - a$pred, as.matrix(a[, c("x", "y")]))
  data.frame(modelo = m, moran_I = v[["I"]], p_valor = v[["p"]], n = nrow(a))
}))
write.csv(mor, file.path(res, "moran_residuos_2025.csv"), row.names = FALSE)

# ---------------------------------------------------------------- comparación con la validación anterior
ant <- read.csv(file.path(res, "metricas_resumen.csv"))
cmp <- resumen |> left_join(ant |> select(esquema, modelo, rmse_previo = rmse, r2_previo = r2), by = c("esquema", "modelo")) |>
  arrange(esquema, rmse)
options(width = 150)
cat("\n== Anidada vs anterior\n"); print(as.data.frame(cmp), digits = 3)
cat("\n== Bootstrap (IC 95 %)\n"); print(boot, digits = 3, row.names = FALSE)
cat("\n== Moran de residuos 2025\n"); print(mor, digits = 3)
message(sprintf("Total: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
