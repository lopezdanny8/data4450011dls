# =============================================================================
# 02 - Clima por distrito y campaña agrícola
# Fuentes: PISCOp v2.1 (lluvia, 0.1°) y TerraClimate v1.1 (tmin, tmax, 1/30°), 2014-01 a 2025-12.
#  1. Promedio de las celdas dentro de cada distrito (ponderado por la fracción de celda
#     cubierta), mes a mes -> clima.distrito_mes
#  2. Agregación por campaña agrícola agosto(t-1)-julio(t) -> clima.distrito_campana
#     La ventana de crecimiento (noviembre-marzo) y de riesgo de helada (octubre-abril) sigue
#     el calendario de la papa en la ENA: en la sierra, 62 % siembra en octubre-noviembre y
#     57 % inicia cosecha en abril-mayo (ponderado, 2015-2025).
# =============================================================================
suppressPackageStartupMessages({ library(terra); library(sf); library(DBI); library(duckdb) })

raiz <- local({  # raíz del proyecto (carpeta con R/comun.R): Rscript, Positron, RStudio y Quarto
  a <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (d in c(dirname(a), getwd())) {
    d <- normalizePath(d, winslash = "/", mustWork = FALSE)
    while (!file.exists(file.path(d, "R", "comun.R")) && dirname(d) != d) d <- dirname(d)
    if (file.exists(file.path(d, "R", "comun.R"))) return(d)
  }
  stop("No se encontró la raíz del proyecto (carpeta que contiene R/comun.R)")
})
dclim <- file.path(raiz, "data", "clima")
meses <- seq(as.Date("2014-01-01"), as.Date("2025-12-01"), by = "month")   # 144 capas en las tres fuentes

# ---------------------------------------------------------------- distritos
dist <- st_read(file.path(raiz, "data", "mapas", "hdx_cod_ab", "per_admin_boundaries.gdb"), layer = "per_admin3", quiet = TRUE)
dist$ubigeo <- sub("^PE", "", dist$adm3_pcode)
v <- vect(dist[, "ubigeo"])

# ---------------------------------------------------------------- extracción
leer <- function(archivo, var) {
  r <- rast(file.path(dclim, archivo), subds = var)
  stopifnot(nlyr(r) == length(meses))
  r
}
extraer <- function(r, nombre) {
  m <- extract(r, v, fun = mean, weights = TRUE, na.rm = TRUE, ID = FALSE)
  # distritos más pequeños que una celda: valor de la celda que contiene el centroide
  sin <- which(rowSums(is.na(m)) == ncol(m))
  if (length(sin)) m[sin, ] <- extract(r, centroids(v[sin], inside = TRUE), ID = FALSE)
  data.frame(ubigeo = rep(v$ubigeo, times = length(meses)),
             fecha = rep(meses, each = nrow(v)),
             valor = as.vector(as.matrix(m)), variable = nombre)
}
t0 <- Sys.time()
pp   <- extraer(leer("pisco_prec_v2p1_mensual_2014_2025_peru.nc", "Prec"), "pp")
tmin <- extraer(leer("terraclimate_tmin_2014_2025_peru.nc", "tmin"), "tmin")
tmax <- extraer(leer("terraclimate_tmax_2014_2025_peru.nc", "tmax"), "tmax")
message(sprintf("Extracción: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
mes <- rbind(pp, tmin, tmax)

# ---------------------------------------------------------------- base
con <- dbConnect(duckdb(), dbdir = file.path(raiz, "data", "ena_papa.duckdb"))
x <- function(sql) invisible(dbExecute(con, sql))
q <- function(sql) dbGetQuery(con, sql)
x("DROP SCHEMA IF EXISTS clima CASCADE"); x("CREATE SCHEMA clima")
dbWriteTable(con, Id(schema = "clima", table = "largo"), mes)
x("CREATE TABLE clima.distrito_mes AS
   PIVOT clima.largo ON variable USING any_value(valor) GROUP BY ubigeo, fecha")
x("DROP TABLE clima.largo")

# campaña t = agosto(t-1) .. julio(t); climatología mensual 2014-2025 del distrito para anomalías
x("CREATE TABLE clima.distrito_campana AS
  WITH m AS (
    SELECT ubigeo, fecha, month(fecha) AS mes, pp, tmin, tmax,
           CASE WHEN month(fecha) >= 8 THEN year(fecha) + 1 ELSE year(fecha) END AS campana,
           avg(pp) OVER (PARTITION BY ubigeo, month(fecha)) AS pp_clim
      FROM clima.distrito_mes)
  SELECT ubigeo, campana AS anio,
         sum(pp)                                                       AS pp_campana,
         sum(pp) FILTER (WHERE mes IN (9, 10, 11))                     AS pp_siembra,
         sum(pp) FILTER (WHERE mes IN (11, 12, 1, 2, 3))               AS pp_crecimiento,
         count(*) FILTER (WHERE mes IN (11, 12, 1, 2, 3) AND pp < 0.5 * pp_clim) AS meses_secos,
         avg(tmin) FILTER (WHERE mes IN (11, 12, 1, 2, 3))             AS tmin_crecimiento,
         avg(tmax) FILTER (WHERE mes IN (11, 12, 1, 2, 3))             AS tmax_crecimiento,
         avg(tmax - tmin) FILTER (WHERE mes IN (11, 12, 1, 2, 3))      AS amplitud_termica,
         min(tmin) FILTER (WHERE mes IN (10, 11, 12, 1, 2, 3, 4))      AS tmin_minima,
         count(*) FILTER (WHERE mes IN (10, 11, 12, 1, 2, 3, 4) AND tmin < 0) AS meses_helada,
         count(*) AS n_meses
    FROM m GROUP BY ubigeo, campana HAVING count(*) = 12")
x("ALTER TABLE clima.distrito_campana ADD COLUMN anom_pp_campana DOUBLE")
x("UPDATE clima.distrito_campana c SET anom_pp_campana = (c.pp_campana - s.media) / nullif(s.media, 0)
     FROM (SELECT ubigeo, avg(pp_campana) media FROM clima.distrito_campana GROUP BY 1) s WHERE c.ubigeo = s.ubigeo")

# geografía del distrito (centroide y área) desde el mapa
geo <- st_drop_geometry(dist)[, c("ubigeo", "center_lat", "center_lon", "area_sqkm")]
names(geo) <- c("ubigeo", "lat", "lon", "area_km2")
dbWriteTable(con, Id(schema = "clima", table = "distrito_geo"), geo, overwrite = TRUE)

# ---------------------------------------------------------------- controles
cat("\n== Campañas disponibles\n")
print(q("SELECT anio, count(*) distritos, round(avg(pp_campana)) pp_media, round(avg(tmin_crecimiento), 1) tmin_media,
                round(avg(meses_helada), 2) helada_media FROM clima.distrito_campana GROUP BY 1 ORDER BY 1"))
cat("\n== Faltantes por variable\n")
print(q("SELECT count(*) n, count(pp_campana) pp, count(tmin_crecimiento) tmin, count(tmax_crecimiento) tmax FROM clima.distrito_campana"))
cat("\n== Juliaca (211101) y Puno (210101)\n")
print(q("SELECT ubigeo, anio, round(pp_campana) pp, meses_secos, round(tmin_minima, 1) tmin_min, meses_helada, round(anom_pp_campana, 2) anom
           FROM clima.distrito_campana WHERE ubigeo IN ('211101', '210101') ORDER BY 1, 2"))
dbDisconnect(con, shutdown = TRUE)
