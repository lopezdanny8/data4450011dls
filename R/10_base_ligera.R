# =============================================================================
# 10 - Base ligera para el repositorio
# La base completa (data/ena_papa.duckdb, ~240 MB) guarda las columnas crudas de la ENA y
# supera el límite de GitHub. Esta copia ligera conserva solo las tablas procesadas que
# usan los scripts 04-09 y el artículo en Quarto, para reproducir los resultados sin
# descargar la ENA original (14 GB).
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
suppressPackageStartupMessages({ library(DBI); library(duckdb) })
completa <- file.path(raiz, "data", "ena_papa.duckdb")
ligera   <- file.path(raiz, "data", "ena_papa_ligera.duckdb")
stopifnot(file.exists(completa))
if (file.exists(ligera)) file.remove(ligera)

con <- dbConnect(duckdb(), ligera)
dbExecute(con, sprintf("ATTACH '%s' AS c (READ_ONLY)", normalizePath(completa, winslash = "/")))
tablas <- c("ena.papa_valido", "ena.papa_distrito", "clima.distrito_campana", "clima.distrito_geo", "analisis.base")
for (s in unique(sub("\\..*", "", tablas))) dbExecute(con, sprintf("CREATE SCHEMA %s", s))
for (t in tablas) dbExecute(con, sprintf("CREATE TABLE %s AS SELECT * FROM c.%s", t, t))
dbExecute(con, "DETACH c")
print(dbGetQuery(con, "SELECT schema_name, table_name, estimated_size AS filas FROM duckdb_tables() ORDER BY 1, 2"))
dbDisconnect(con, shutdown = TRUE)
message(sprintf("Base ligera: %s (%.1f MB)", ligera, file.size(ligera) / 2^20))
