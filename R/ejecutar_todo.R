# =============================================================================
# ejecutar_todo.R - corre la cadena completa en orden y renderiza el artículo
#
# Modo completo (requiere la ENA original, ver README):   Rscript R/ejecutar_todo.R completo
# Modo ligero (solo con lo incluido en el repositorio):    Rscript R/ejecutar_todo.R
#   El modo ligero usa data/ena_papa_ligera.duckdb y rehace modelos, validación,
#   predicción 2025, figuras y el PDF. Los pasos 00-03, 08 y 10 necesitan la ENA original.
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
completo <- "completo" %in% commandArgs(TRUE)
if (!completo) Sys.setenv(USAR_BASE_LIGERA = "1")

pasos <- c(
  if (completo) c("00_extraer_ena.R",      # ENA .sav -> data/ena_papa.duckdb
                  "01_base_papa.R",        # registros de papa y rendimiento distrital
                  "02_clima_distrito.R",   # PISCOp y TerraClimate por distrito (~17 min)
                  "03_base_integrada.R"),  # base distrito-año (data/base_integrada.csv)
  "04_modelos.R",                          # 6 modelos x 3 esquemas de validación
  "06_tabla_variables.R",                  # estadísticos de la Tabla II
  "07_prediccion_2025.R",                  # mejor modelo + intervalos conformales
  if (completo) "08_inventario_datos.R",   # volumen de datos (Tabla I)
  "09_validacion_robusta.R",               # CV anidada, bootstrap, Moran (~23 min)
  "05_figuras.R",                          # figuras 1-5
  if (completo) "10_base_ligera.R")        # copia ligera para el repositorio

rscript <- file.path(R.home("bin"), "Rscript")
for (p in pasos) {
  message(sprintf("\n==== %s  (%s)", p, format(Sys.time(), "%H:%M")))
  st <- system2(rscript, shQuote(file.path(raiz, "R", p)))
  if (st != 0) stop("Falló ", p)
}

# artículo: Quarto renderiza paper/paper.qmd -> paper/paper_quarto.pdf
Sys.setenv(REGENERAR_FIGURAS = "0")   # las figuras ya se generaron arriba
quarto <- Sys.which("quarto")
if (!nzchar(quarto)) quarto <- file.path(Sys.getenv("ProgramFiles"), "RStudio/resources/app/bin/quarto/bin/quarto.exe")
st <- system2(quarto, c("render", shQuote(file.path(raiz, "paper", "paper.qmd"))))
if (st != 0) stop("Falló quarto render")
message("Listo: ", file.path(raiz, "paper", "paper_quarto.pdf"))
