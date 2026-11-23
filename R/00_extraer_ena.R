# =============================================================================
# 00 - Extracción de la ENA a la base propia del artículo (DuckDB)
# Lee de los archivos originales del INEI (sin copiarlos) solo las columnas que usa
# el estudio y las guarda en data/ena_papa.duckdb, esquema raw:
#   raw.h_2014_2024 : ENA_2014_2024.sav (formato largo, todos los capítulos)
#   raw.e25_<cap>   : módulos de la ENA 2025 (un archivo por capítulo)
#   raw.diccionario : nombre y etiqueta de cada variable extraída
# =============================================================================
suppressPackageStartupMessages({ library(haven); library(DBI); library(duckdb) })

raiz <- local({  # raíz del proyecto (carpeta con R/comun.R): Rscript, Positron, RStudio y Quarto
  a <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (d in c(dirname(a), getwd())) {
    d <- normalizePath(d, winslash = "/", mustWork = FALSE)
    while (!file.exists(file.path(d, "R", "comun.R")) && dirname(d) != d) d <- dirname(d)
    if (file.exists(file.path(d, "R", "comun.R"))) return(d)
  }
  stop("No se encontró la raíz del proyecto (carpeta que contiene R/comun.R)")
})
# carpeta con los archivos del INEI: variable ENA_DIR, o la carpeta superior al proyecto, o data/ena
datos <- Sys.getenv("ENA_DIR", unset = if (file.exists(file.path(raiz, "..", "ENA_2014_2024.sav")))
                 normalizePath(file.path(raiz, "..")) else file.path(raiz, "data", "ena"))
db    <- file.path(raiz, "data", "ena_papa.duckdb")
if (file.exists(db)) file.remove(db)
con <- dbConnect(duckdb(), dbdir = db)
dbExecute(con, "CREATE SCHEMA raw")

# ---------------------------------------------------------------- variables
LLAVE <- c("ANIO", "CCDD", "NOMBREDD", "CCPP", "NOMBREPV", "CCDI", "NOMBREDI", "CONGLOMERADO", "NSELUA",
           "NSEGM", "ID_PROD", "UA", "REGION", "ESTRATO", "FACTOR", "FACTOR_PRODUCTOR", "FACTOR_SUPERFICIE",
           "LATITUD", "LONGITUD", "ALTITUD")
# parcela: su superficie en la unidad declarada y su equivalencia en hectáreas permiten convertir
# la superficie cosechada a hectáreas en los años en que el INEI no publicó P217_SUP_ha (2015, 2018)
PARCELA <- c("P105_N", "P105", "P105_SUP_1", "P105_SUP_2", "P105_EQUIV_1", "P105_EQUIV_2", "P105_SUP_ha")
CULTIVO <- c("P203", "P204_NOM", "P204_COD", "P204_TIPO", "P205_N", "P206_INI_MES", "P206_INI_ANIO",
             "P207_FIN_MES", "P207_FIN_ANIO", "P208", "P209_MES", "P209_ANIO", "P210_SUP_1", "P210_SUP_2",
             "P212", "P213", "P214", "P215", "P217_SUP_1", "P217_SUP_2", "P217_SUP_ha",
             "P219_CANT_1", "P219_CANT_2", "P219_UM", "P219_UM_COD", "P219_EQUIV_KG",
             "P220_1_CANT_1", "P220_1_CANT_2", "P223A", paste0("P223B_", 1:8), paste0("P224E_", 1:8))
INSUMOS <- c("P234_COD", "P236", "P238", "P239", "P240", "P241")
OTROS   <- c("P701", "P704", "P801", "P901", "P902", "P905", "P1102", "P1103", "P1104_A", "P1105", "P1106")
VARS    <- c(LLAVE, PARCELA, CULTIVO, INSUMOS, OTROS)

dic <- list()
cargar <- function(tabla, archivo) {
  t0 <- Sys.time()
  df <- read_sav(archivo, col_select = any_of(VARS), encoding = "latin1")
  lab <- vapply(df, function(v) { l <- attr(v, "label"); if (is.null(l)) "" else paste(l, collapse = " ") }, "")
  dic[[tabla]] <<- data.frame(tabla = tabla, variable = names(df), etiqueta = lab)
  df <- zap_labels(zap_formats(df))
  df[] <- lapply(df, function(v) if (is.character(v)) trimws(v) else as.numeric(v))
  dbWriteTable(con, Id(schema = "raw", table = tabla), df)
  message(sprintf("raw.%-16s %10s filas  %3d columnas  (%.1f min)", tabla, format(nrow(df), big.mark = ","),
                  ncol(df), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

# ---------------------------------------------------------------- 2025 (módulos)
sav25 <- list.files(datos, pattern = "[.]sav$", recursive = TRUE, full.names = TRUE)
sav25 <- sav25[grepl("1036-Modulo", sav25)]
caps  <- c(e25_caratula = "01_CARATULA", e25_cap100a_03 = "01_CAP100A_03", e25_cap200ab = "03_CAP200AB", e25_cap200e = "07_CAP200E",
           e25_cap700 = "15_CAP700", e25_cap800 = "16_CAP800", e25_cap900 = "17_CAP900", e25_cap1100 = "19_CAP1100")
for (k in names(caps)) cargar(k, grep(caps[[k]], sav25, value = TRUE)[1])

# ---------------------------------------------------------------- 2014-2024 (archivo único)
cargar("h_2014_2024", file.path(datos, "ENA_2014_2024.sav"))

dic <- do.call(rbind, dic)
dic$etiqueta <- iconv(dic$etiqueta, "UTF-8", "UTF-8", sub = "")
dbWriteTable(con, Id(schema = "raw", table = "diccionario"), dic)

# ---------------------------------------------------------------- controles
print(dbGetQuery(con, "SELECT CAST(ANIO AS INT) anio, count(*) filas, count(P204_COD) filas_cultivo,
                        count(CASE WHEN P204_COD BETWEEN 2610 AND 2615 THEN 1 END) filas_papa
                         FROM raw.h_2014_2024 GROUP BY 1 ORDER BY 1"))
print(dbGetQuery(con, "SELECT count(*) filas, count(CASE WHEN TRY_CAST(P204_COD AS INT) BETWEEN 2610 AND 2615 THEN 1 END) filas_papa
                         FROM raw.e25_cap200ab"))
dbDisconnect(con, shutdown = TRUE)
message("Base: ", db, sprintf(" (%.0f MB)", file.size(db) / 2^20))
