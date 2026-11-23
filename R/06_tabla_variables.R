# =============================================================================
# 06 - Estadística descriptiva de las variables del modelo (base de modelado:
# distritos de la sierra con >= 5 productores, 2015-2025) para la tabla de variables
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
d <- cargar_base(raiz)
d$anom_pp_campana <- 100 * d$anom_pp_campana          # en porcentaje
vars <- c("rendimiento_t_ha", PRED)
est <- data.frame(variable = vars,
                  media = sapply(vars, function(v) mean(d[[v]], na.rm = TRUE)),
                  de    = sapply(vars, function(v) sd(d[[v]], na.rm = TRUE)),
                  min   = sapply(vars, function(v) min(d[[v]], na.rm = TRUE)),
                  max   = sapply(vars, function(v) max(d[[v]], na.rm = TRUE)),
                  pct_completo = sapply(vars, function(v) 100 * mean(!is.na(d[[v]]))))
write.csv(est, file.path(raiz, "results", "estadisticos_variables.csv"), row.names = FALSE)
print(est, digits = 3, row.names = FALSE)
