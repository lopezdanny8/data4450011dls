# =============================================================================
# 08 - Inventario de datos: cuántos datos se procesan en cada fuente y etapa
# Cuenta directamente desde los archivos y la base (no hay cifras escritas a mano).
# Salidas: results/inventario_datos.csv y results/inventario_datos.md
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
suppressPackageStartupMessages({ library(DBI); library(duckdb); library(terra); library(sf) })
fmt <- function(x) formatC(x, format = "d", big.mark = " ")
inv <- list()
add <- function(fuente, etapa, valor, detalle = "") inv[[length(inv) + 1]] <<- data.frame(fuente, etapa, valor, detalle)

# ---------------------------------------------------------------- ENA
con <- dbConnect(duckdb(), file.path(raiz, "data", "ena_papa.duckdb"), read_only = TRUE)
q <- function(sql) dbGetQuery(con, sql)
h <- q("SELECT CAST(ANIO AS INT) anio, count(*) filas, count(P204_COD) cultivo FROM raw.h_2014_2024 GROUP BY 1 ORDER BY 1")
add("ENA", "Filas de ENA_2014_2024.sav (todos los capítulos)", sum(h$filas),
    paste(sprintf("%d: %s", h$anio, fmt(h$filas)), collapse = "; "))
add("ENA", "Filas de cultivos 2014-2024", sum(h$cultivo))
add("ENA", "Filas del módulo de cultivos ENA 2025", q("SELECT count(*) n FROM raw.e25_cap200ab")$n)
add("ENA", "Registros de papa 2015-2025", q("SELECT count(*) n FROM ena.papa_registro")$n)
add("ENA", "Registros de papa válidos (0,1-60 t/ha)", q("SELECT count(*) n FROM ena.papa_valido")$n)
add("ENA", "Distrito-año con papa, todo el Perú", q("SELECT count(*) n FROM analisis.base")$n)
add("ENA", "Distrito-año con papa, sierra", q("SELECT count(*) n FROM analisis.base WHERE region = 'Sierra'")$n)
b <- q("SELECT anio, count(*) n FROM analisis.base WHERE region = 'Sierra' AND n_productores >= 5 GROUP BY 1 ORDER BY 1")
add("Base de modelado", "Distrito-año, sierra, >= 5 productores", sum(b$n), paste(sprintf("%d: %d", b$anio, b$n), collapse = "; "))
add("Base de modelado", "Distritos distintos",
    q("SELECT count(DISTINCT ubigeo) n FROM analisis.base WHERE region = 'Sierra' AND n_productores >= 5")$n)
add("Base de modelado", "Entrenamiento 2015-2024", sum(b$n[b$anio <= 2024]))
add("Base de modelado", "Prueba 2025", sum(b$n[b$anio == 2025]))
add("Base de modelado", "Variables (objetivo + predictores)", 21)
add("Clima", "Distrito-mes por variable", q("SELECT count(*) n FROM clima.distrito_mes")$n)
add("Clima", "Distrito-campaña", q("SELECT count(*) n FROM clima.distrito_campana")$n)
dbDisconnect(con, shutdown = TRUE)

# ---------------------------------------------------------------- clima (grillas)
grilla <- function(archivo, var, nombre, detalle) {
  r <- rast(file.path(raiz, "data", "clima", archivo), subds = var)
  total <- ncell(r) * nlyr(r)
  validos <- sum(global(!is.na(r), "sum")$sum)
  add(nombre, "Valores en el archivo (celda-mes)", total, sprintf("%s celdas x %d meses; %s", fmt(ncell(r)), nlyr(r), detalle))
  add(nombre, "Valores válidos (celdas sobre tierra)", validos)
}
grilla("pisco_prec_v2p1_mensual_2014_2025_peru.nc", "Prec", "PISCOp v2.1", "lluvia, 0,1°")
grilla("terraclimate_tmin_2014_2025_peru.nc", "tmin", "TerraClimate Tmín", "1/30°")
grilla("terraclimate_tmax_2014_2025_peru.nc", "tmax", "TerraClimate Tmáx", "1/30°")

# ---------------------------------------------------------------- mapa
m <- st_read(file.path(raiz, "data", "mapas", "hdx_cod_ab", "per_admin_boundaries.gdb"), layer = "per_admin3", quiet = TRUE)
add("Mapa distrital (IGN)", "Distritos", nrow(m))

inv <- do.call(rbind, inv)
write.csv(inv, file.path(raiz, "results", "inventario_datos.csv"), row.names = FALSE)
md <- c("# Inventario de datos", "", "Generado por `R/08_inventario_datos.R` a partir de los archivos y de `data/ena_papa.duckdb`.", "",
        "| Fuente | Etapa | Cantidad | Detalle |", "|---|---|---:|---|",
        sprintf("| %s | %s | %s | %s |", inv$fuente, inv$etapa, fmt(inv$valor), inv$detalle))
writeLines(md, file.path(raiz, "results", "inventario_datos.md"), useBytes = TRUE)
print(inv[, 1:3], row.names = FALSE)
