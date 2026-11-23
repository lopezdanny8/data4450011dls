# =============================================================================
# 03 - Base integrada de análisis: ENA (papa, distrito-año) + clima (campaña) + geografía
#   analisis.base : una fila = distrito con papa en la ENA en el año t (2015-2025)
# =============================================================================
suppressPackageStartupMessages({ library(DBI); library(duckdb) })

raiz <- local({  # raíz del proyecto (carpeta con R/comun.R): Rscript, Positron, RStudio y Quarto
  a <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (d in c(dirname(a), getwd())) {
    d <- normalizePath(d, winslash = "/", mustWork = FALSE)
    while (!file.exists(file.path(d, "R", "comun.R")) && dirname(d) != d) d <- dirname(d)
    if (file.exists(file.path(d, "R", "comun.R"))) return(d)
  }
  stop("No se encontró la raíz del proyecto (carpeta que contiene R/comun.R)")
})
con  <- dbConnect(duckdb(), dbdir = file.path(raiz, "data", "ena_papa.duckdb"))
x <- function(sql) invisible(dbExecute(con, sql))
q <- function(sql) dbGetQuery(con, sql)
x("DROP SCHEMA IF EXISTS analisis CASCADE"); x("CREATE SCHEMA analisis")

# nombres: 2018 viene sin nombres de ubicación en el archivo del INEI -> se toman de los otros años
x("CREATE TABLE analisis.base AS
  WITH nom AS (
    SELECT ubigeo,
           any_value(departamento) FILTER (WHERE nullif(departamento, '') IS NOT NULL) AS departamento,
           any_value(provincia)    FILTER (WHERE nullif(provincia, '') IS NOT NULL)    AS provincia,
           any_value(distrito)     FILTER (WHERE nullif(distrito, '') IS NOT NULL)     AS distrito
      FROM ena.papa_distrito GROUP BY 1)
  SELECT e.anio, e.ubigeo, nom.departamento, nom.provincia, nom.distrito,
         CASE e.region_cod WHEN 1 THEN 'Costa' WHEN 2 THEN 'Sierra' WHEN 3 THEN 'Selva' END AS region,
         g.lat, g.lon, g.area_km2,
         e.n_registros, e.n_productores,
         e.rendimiento_t_ha,
         e.superficie_media_ha, e.prop_riego, e.prop_semilla_cert, e.prop_papa_nativa,
         e.prop_abono, e.prop_fertilizante, e.prop_plaguicida, e.prop_asistencia, e.prop_capacitacion,
         e.prop_asociatividad,
         c.pp_campana, c.pp_siembra, c.pp_crecimiento, c.anom_pp_campana, c.meses_secos,
         c.tmin_crecimiento, c.tmax_crecimiento, c.amplitud_termica, c.tmin_minima, c.meses_helada
    FROM ena.papa_distrito e
    JOIN clima.distrito_geo g USING (ubigeo)
    JOIN clima.distrito_campana c USING (ubigeo, anio)
    LEFT JOIN nom USING (ubigeo)")

cat("== Filas por año\n")
print(q("SELECT anio, count(*) distritos, sum(CASE WHEN n_productores >= 5 THEN 1 ELSE 0 END) con_5mas,
                count(DISTINCT departamento) dptos FROM analisis.base GROUP BY 1 ORDER BY 1"))
cat("\n== Filas ENA que no unieron (deben ser solo ubigeo 000000)\n")
print(q("SELECT e.ubigeo, count(*) n FROM ena.papa_distrito e ANTI JOIN analisis.base b USING (ubigeo, anio) GROUP BY 1"))
cat("\n== Completitud (%) y rango de cada variable\n")
vars <- setdiff(dbListFields(con, Id(schema = "analisis", table = "base")),
                c("anio", "ubigeo", "departamento", "provincia", "distrito", "region"))
res <- do.call(rbind, lapply(vars, function(v) q(sprintf(
  "SELECT '%s' AS variable, round(100.0 * count(%s) / count(*), 1) AS pct_completo, round(min(%s), 2) AS minimo,
          round(median(%s), 2) AS mediana, round(max(%s), 2) AS maximo FROM analisis.base", v, v, v, v, v))))
print(res, row.names = FALSE)
cat("\n== Región\n"); print(q("SELECT region, count(*) n, round(median(rendimiento_t_ha), 1) rend_med FROM analisis.base GROUP BY 1"))
cat("\n== Correlación de Spearman con el rendimiento (sierra)\n")
d <- q("SELECT * FROM analisis.base WHERE region = 'Sierra' AND n_productores >= 5")
r <- sapply(setdiff(vars, c("rendimiento_t_ha", "n_registros", "n_productores")),
            function(v) cor(d$rendimiento_t_ha, d[[v]], method = "spearman", use = "complete.obs"))
print(round(sort(r), 3))
write.csv(q("SELECT * FROM analisis.base ORDER BY anio, ubigeo"), file.path(raiz, "data", "base_integrada.csv"), row.names = FALSE)
dbDisconnect(con, shutdown = TRUE)
