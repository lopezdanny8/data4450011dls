# =============================================================================
# 04 - Comparación de modelos bajo tres esquemas de validación
# Datos: distritos de la sierra con >= 5 productores de papa (analisis.base), 2015-2025.
# Modelos (ver comun.R):
#   base_rf, base_gb : réplica de Tamayo-Vera et al. (2025): potencia + K mejores + RF / gradient boosting
#   xgb              : XGBoost con coordenadas
#   rf_esp           : Random Forest espacial (coordenadas + vecinos, RFSI)
#   rk_st            : kriging espacio-temporal con regresión (tendencia lineal + kriging de residuos)
#   rf_rkst          : híbrido Random Forest + kriging espacio-temporal de residuos
# Validación:
#   temporal : 5 pliegues pasado -> futuro (prueba 2019, 2021, 2022, 2023, 2024), como el artículo base
#   espacial : 5 bloques de distritos vecinos (k-medias sobre coordenadas), 2015-2024
#   futuro   : entrenamiento 2015-2024, prueba 2025 (ronda no usada en ningún ajuste)
# Hiperparámetros: búsqueda en grilla con la validación temporal sobre 2015-2024 (igual para todos).
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
dir.create(file.path(raiz, "results"), showWarnings = FALSE)

d   <- cargar_base(raiz)
dev <- d[d$anio <= 2024, ]
esquemas <- list(temporal = list(datos = dev, pl = particion_temporal(dev)),
                 espacial = list(datos = dev, pl = particion_espacial(dev)),
                 futuro   = list(datos = d,   pl = particion_futuro(d)))
message(sprintf("Base: %d distrito-año, %d distritos, %d predictores", nrow(d), length(unique(d$ubigeo)), length(PRED)))

# ---------------------------------------------------------------- 1. ajuste de hiperparámetros
rf_trend <- function(train, test, hp) {          # tendencia RF del híbrido, para ajustar sin kriging
  rec <- receta(train, PRED); Xtr <- matriz(rec, train); Xte <- matriz(rec, test)
  m <- ranger(x = Xtr, y = train$y_obs, num.trees = 500, mtry = hp$mtry, min.node.size = hp$nodo, seed = 1, num.threads = 8)
  list(pred = predict(m, Xte)$predictions)
}
grillas <- list(
  base_rf = expand.grid(k = c(10, 15, 20), mtry = c(3, 6), nodo = c(5, 10)),
  base_gb = expand.grid(k = c(10, 15, 20), n = c(200, 400, 800), prof = c(3, 5)),
  xgb     = expand.grid(n = c(200, 400, 800), prof = c(3, 5)),
  rf_esp  = expand.grid(mtry = c(4, 8, 12), nodo = c(5, 10)),
  rf_rkst = expand.grid(mtry = c(4, 8, 12), nodo = c(5, 10)),
  rk_st   = data.frame(ninguno = 0))
cv_rmse <- function(f, hp) {
  pl <- esquemas$temporal$pl
  mean(sapply(pl, function(p) { r <- f(dev[p$train, ], dev[p$test, ], hp); sqrt(mean((dev$y_obs[p$test] - r$pred)^2)) }))
}
t0 <- Sys.time()
hps <- list(); tabla_hp <- list()
for (m in names(grillas)) {
  g <- grillas[[m]]
  if (m == "rk_st") { hps[[m]] <- list(); next }
  f <- if (m == "rf_rkst") rf_trend else modelos[[m]]
  g$rmse_cv <- sapply(seq_len(nrow(g)), function(i) cv_rmse(f, as.list(g[i, , drop = FALSE])))
  hps[[m]] <- as.list(g[which.min(g$rmse_cv), setdiff(names(g), "rmse_cv"), drop = FALSE])
  tabla_hp[[m]] <- cbind(modelo = m, g)
  message(sprintf("%-8s mejor: %s (RMSE CV %.3f)", m, paste(names(hps[[m]]), unlist(hps[[m]]), sep = "=", collapse = ", "), min(g$rmse_cv)))
}
write.csv(bind_rows(tabla_hp), file.path(raiz, "results", "hiperparametros_grilla.csv"), row.names = FALSE)
message(sprintf("Ajuste: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

# ---------------------------------------------------------------- 2. evaluación
preds <- list(); varios <- list()
for (e in names(esquemas)) {
  D <- esquemas[[e]]$datos
  for (p in esquemas[[e]]$pl) {
    tr <- D[p$train, ]; te <- D[p$test, ]
    for (m in names(modelos)) {
      r <- modelos[[m]](tr, te, hps[[m]])
      preds[[length(preds) + 1]] <- data.frame(esquema = e, pliegue = p$id, modelo = m, ubigeo = te$ubigeo, anio = te$anio,
                                               departamento = te$departamento, n_productores = te$n_productores,
                                               obs = te$y_obs, pred = r$pred, var_kriging = if (is.null(r$var)) NA else r$var)
      if (!is.null(r$vario)) varios[[paste(e, p$id, m, sep = "_")]] <- r$vario
    }
    message(sprintf("%-8s %-6s listo", e, p$id))
  }
}
P <- bind_rows(preds)
write.csv(P, file.path(raiz, "results", "predicciones.csv"), row.names = FALSE)
saveRDS(list(variogramas = varios, hiperparametros = hps), file.path(raiz, "results", "modelos_aux.rds"))

# ---------------------------------------------------------------- 3. métricas
por_pliegue <- P |> group_by(esquema, pliegue, modelo) |> group_modify(~ metricas(.x$obs, .x$pred)) |> ungroup()
resumen <- por_pliegue |> group_by(esquema, modelo) |>
  summarise(rmse_de = sd(rmse), r2_de = sd(r2), rmse = mean(rmse), mae = mean(mae), r2 = mean(r2), pliegues = n(), .groups = "drop") |>
  arrange(esquema, rmse)
sequia <- por_pliegue |> filter(pliegue == "T2023") |> select(modelo, rmse_2023 = rmse, mae_2023 = mae, r2_2023 = r2)
write.csv(por_pliegue, file.path(raiz, "results", "metricas_pliegue.csv"), row.names = FALSE)
write.csv(resumen, file.path(raiz, "results", "metricas_resumen.csv"), row.names = FALSE)
write.csv(sequia, file.path(raiz, "results", "metricas_sequia_2023.csv"), row.names = FALSE)
options(width = 140)
cat("\n== Resumen (media de pliegues)\n"); print(as.data.frame(resumen), digits = 3)
cat("\n== Año de sequía 2023 (pliegue temporal T2023)\n"); print(as.data.frame(sequia), digits = 3)
message(sprintf("Total: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
