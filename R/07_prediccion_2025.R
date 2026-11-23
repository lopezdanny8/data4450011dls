# =============================================================================
# 07 - Predicción de 2025 con el mejor modelo e intervalos de predicción al 95 %
# 1. Mejor modelo = menor RMSE en la validación temporal 2015-2024 (no se usa 2025 para elegirlo).
# 2. Intervalos: regresión cuantílica conformalizada (CQR; Romano et al., 2019) sobre un
#    Quantile Regression Forest con los mismos predictores del RF espacial.
#    - Calibración: pliegues temporales de 2015-2024 (entrenar con años < t, predecir t);
#      puntaje de conformidad E = max(q_inf - y, y - q_sup).
#    - Ajuste final con 2015-2024 y predicción de 2025: [q_inf - c, q_sup + c],
#      con c = cuantil ceil((n+1)(1-a))/n de E.
# 3. Cobertura empírica en los 585 distritos de 2025 y tabla de 12 distritos:
#    los 6 con más productores fuera de Puno y los 6 con más productores de Puno.
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
ALFA <- 0.05

d   <- cargar_base(raiz)
dev <- d[d$anio <= 2024, ]
fut <- d[d$anio == 2025, ]

# ---------------------------------------------------------------- 1. mejor modelo
resumen <- read.csv(file.path(res, "metricas_resumen.csv"))
temp <- resumen[resumen$esquema == "temporal", c("modelo", "rmse", "r2")]
temp <- temp[order(temp$rmse), ]
mejor <- temp$modelo[1]
cat("Comparación (validación temporal):\n"); print(temp, row.names = FALSE)
cat("Mejor modelo:", mejor, "\n")
stopifnot(mejor == "rf_esp")
hp <- readRDS(file.path(res, "modelos_aux.rds"))$hiperparametros$rf_esp

# ---------------------------------------------------------------- 2. QRF con los predictores del RF espacial
matriz_esp <- function(train, objetivo) {
  rec <- receta(train, c(PRED, "x", "y"))
  cbind(matriz(rec, objetivo), vecinos(train, objetivo))
}
qrf <- function(train, test) {
  Xtr <- matriz_esp(train, train); Xte <- matriz_esp(train, test)
  m <- ranger(x = Xtr, y = train$y_obs, num.trees = 500, mtry = hp$mtry, min.node.size = hp$nodo,
              quantreg = TRUE, seed = 1, num.threads = 8)
  q <- predict(m, Xte, type = "quantiles", quantiles = c(ALFA / 2, 1 - ALFA / 2))$predictions
  data.frame(pred = predict(m, Xte)$predictions, q_inf = q[, 1], q_sup = q[, 2])
}

# calibración con los pliegues temporales
cal <- do.call(rbind, lapply(particion_temporal(dev), function(p) {
  r <- qrf(dev[p$train, ], dev[p$test, ])
  data.frame(anio = dev$anio[p$test], obs = dev$y_obs[p$test], r)
}))
cal$E <- pmax(cal$q_inf - cal$obs, cal$obs - cal$q_sup)
n <- nrow(cal)
c_hat <- unname(quantile(cal$E, probs = min(1, ceiling((n + 1) * (1 - ALFA)) / n), type = 1))
cat(sprintf("\nCalibración: n = %d, cobertura del QRF sin conformalizar = %.1f %%, corrección c = %.2f t/ha\n",
            n, 100 * mean(cal$E <= 0), c_hat))

# ---------------------------------------------------------------- 3. predicción de 2025
r25 <- qrf(dev, fut)
P <- data.frame(ubigeo = fut$ubigeo, departamento = fut$departamento, provincia = fut$provincia,
                distrito = fut$distrito, n_productores = fut$n_productores, obs = fut$y_obs,
                pred = r25$pred, qrf_inf = r25$q_inf, qrf_sup = r25$q_sup,
                li = pmax(0, r25$q_inf - c_hat), ls = r25$q_sup + c_hat)
P$dentro <- P$obs >= P$li & P$obs <= P$ls
P$dentro_qrf <- P$obs >= P$qrf_inf & P$obs <= P$qrf_sup
write.csv(P, file.path(res, "prediccion_2025.csv"), row.names = FALSE)

cob <- data.frame(
  intervalo = c("QRF sin conformalizar", "CQR (conformal)"),
  cobertura_pct = 100 * c(mean(P$dentro_qrf), mean(P$dentro)),
  ancho_medio = c(mean(P$qrf_sup - P$qrf_inf), mean(P$ls - P$li)),
  ancho_mediano = c(median(P$qrf_sup - P$qrf_inf), median(P$ls - P$li)),
  n = nrow(P))
write.csv(cob, file.path(res, "cobertura_2025.csv"), row.names = FALSE)
cat("\nCobertura en 2025 (nominal 95 %):\n"); print(cob, digits = 3, row.names = FALSE)
cat(sprintf("RMSE de la predicción puntual 2025: %.2f t/ha; R2 = %.3f\n",
            sqrt(mean((P$obs - P$pred)^2)), 1 - sum((P$obs - P$pred)^2) / sum((P$obs - mean(P$obs))^2)))

# ---------------------------------------------------------------- 4. tabla de 12 distritos
puno <- substr(P$ubigeo, 1, 2) == "21"
sel <- rbind(head(P[!puno, ][order(-P$n_productores[!puno]), ], 6),
             head(P[puno, ][order(-P$n_productores[puno]), ], 6))
write.csv(sel, file.path(res, "tabla_prediccion_12_distritos.csv"), row.names = FALSE)
cat("\nTabla de 12 distritos:\n")
print(transform(sel[, c("departamento", "distrito", "n_productores", "obs", "pred", "li", "ls", "dentro")],
                obs = round(obs, 2), pred = round(pred, 2), li = round(li, 2), ls = round(ls, 2)), row.names = FALSE)
