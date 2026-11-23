# =============================================================================
# 01 - Base de papa 2015-2025: registros armonizados, rendimiento y agregación
# distrito-año con el factor de expansión. Lee y escribe en data/ena_papa.duckdb.
#   ena.papa_registro : un registro = un cultivo de papa en una parcela de un productor
#   ena.productor     : prácticas a nivel de productor (insumos, asistencia, asociatividad)
#   ena.papa_distrito : rendimiento (estimador de razón ponderado) y covariables por distrito-año
# Notas de armonización
#   - 2014 no tiene producción ni superficie de cultivos en ENA_2014_2024.sav: se excluye.
#   - 2015 y 2018 no tienen P217_SUP_ha: se convierte la superficie declarada con el factor
#     ha/unidad de la misma parcela (validado: 100 % de coincidencia en 2016-2022).
#   - Factor de expansión: FACTOR (2015-2022), FACTOR_PRODUCTOR (2023-2025).
#   - prop_venta_aprox: P220_1_CANT_1 está en la unidad declarada (no en kg), por eso es aproximada
#     y no se usa como predictor hasta verificar su unidad.
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
x("DROP SCHEMA IF EXISTS ena CASCADE"); x("CREATE SCHEMA ena")

# ---------------------------------------------------------------- llaves
x(r"(CREATE OR REPLACE MACRO nk(v) AS coalesce(ltrim(regexp_replace(trim(CAST(v AS VARCHAR)), '\.0$', ''), '0'), ''))")
x(r"(CREATE OR REPLACE MACRO dec3(v) AS coalesce(TRY_CAST(nullif(trim(CAST(v AS VARCHAR)), '') AS DOUBLE), 0) / 1000)")
x("CREATE OR REPLACE MACRO ubigeo(d, p, i) AS lpad(nk(d), 2, '0') || lpad(nk(p), 2, '0') || lpad(nk(i), 2, '0')")
ID_H  <- "concat_ws('|', CAST(CAST(ANIO AS INTEGER) AS VARCHAR), nk(CCDD), nk(CCPP), nk(CCDI), nk(CONGLOMERADO), nk(NSELUA), nk(NSEGM), nk(ID_PROD), nk(UA))"
ID_25 <- "concat_ws('|', CAST(CAST(ANIO AS INTEGER) AS VARCHAR), nk(CCDD), nk(CCPP), nk(CCDI), '', '', nk(NSEGM), nk(ID_PROD), nk(UA))"

# ---------------------------------------------------------------- 1. registros de papa
CAMPOS <- "CAST(ANIO AS INTEGER) AS anio, ubigeo(CCDD, CCPP, CCDI) AS ubigeo,
           NOMBREDD AS departamento, NOMBREPV AS provincia, NOMBREDI AS distrito,
           CAST(REGION AS INTEGER) AS region_cod, TRY_CAST(P204_COD AS INTEGER) AS cod_cultivo, P204_NOM AS cultivo,
           (P219_CANT_1 + dec3(P219_CANT_2)) * P219_EQUIV_KG AS kg,
           P212, P214, P220_1_CANT_1, P223A, P223B_1, P223B_3, P223B_4, P223B_5"
x(sprintf("CREATE TABLE ena.papa_raw AS
  WITH parc AS (  -- factor ha / unidad declarada de cada parcela (para 2015 y 2018)
    SELECT %s AS id_productor, P105_N,
           any_value(coalesce(P105_SUP_ha, P105_EQUIV_1 + coalesce(TRY_CAST(P105_EQUIV_2 AS DOUBLE), 0) / 10000)
                     / nullif(P105_SUP_1 + dec3(P105_SUP_2), 0)) AS factor_ha
      FROM raw.h_2014_2024 WHERE P105_SUP_1 IS NOT NULL AND ANIO IN (2015, 2018) GROUP BY ALL),
  h AS (
    SELECT %s AS id_productor, P105_N, coalesce(FACTOR, FACTOR_PRODUCTOR) AS w, P217_SUP_ha, P217_SUP_1 + dec3(P217_SUP_2) AS sup_local, %s
      FROM raw.h_2014_2024 WHERE P204_COD BETWEEN 2610 AND 2615 AND ANIO >= 2015)
  SELECT h.id_productor, h.anio, h.ubigeo, h.departamento, h.provincia, h.distrito, h.region_cod, h.cod_cultivo, h.cultivo,
         h.w, coalesce(h.P217_SUP_ha, h.sup_local * parc.factor_ha) AS ha, h.kg,
         h.P212, h.P214, h.P220_1_CANT_1, h.P223A, h.P223B_1, h.P223B_3, h.P223B_4, h.P223B_5,
         CASE WHEN h.P217_SUP_ha IS NULL THEN 'reconstruida' ELSE 'INEI' END AS fuente_ha
    FROM h LEFT JOIN parc USING (id_productor, P105_N)
  UNION ALL BY NAME
  SELECT %s AS id_productor, FACTOR_PRODUCTOR AS w, P217_SUP_ha AS ha, 'INEI' AS fuente_ha, %s
    FROM raw.e25_cap200ab WHERE TRY_CAST(P204_COD AS INTEGER) BETWEEN 2610 AND 2615",
  ID_H, ID_H, CAMPOS, ID_25, CAMPOS))

x("CREATE TABLE ena.papa_registro AS
  SELECT *, kg / ha / 1000 AS rend_t_ha,
         CASE WHEN P212 BETWEEN 2 AND 7 THEN 1 WHEN P212 IS NOT NULL THEN 0 END AS riego,
         CASE WHEN P214 = 1 THEN 1 WHEN P214 IS NOT NULL THEN 0 END AS semilla_certificada,
         CASE WHEN cod_cultivo IN (2610, 2615) THEN 1 ELSE 0 END AS papa_nativa_amarga
    FROM ena.papa_raw")

# ---------------------------------------------------------------- 2. prácticas del productor
x(sprintf("CREATE TABLE ena.productor AS
  WITH ins AS (
    SELECT %s AS id_productor,
           max(CASE WHEN P236 = 1 THEN 1 WHEN P236 IS NOT NULL THEN 0 END) abono,
           max(CASE WHEN P238 = 1 THEN 1 WHEN P238 IS NOT NULL THEN 0 END) fertilizante,
           max(CASE WHEN P240 = 1 THEN 1 WHEN P240 IS NOT NULL THEN 0 END) plaguicida
      FROM raw.h_2014_2024 WHERE ANIO >= 2015 AND (P236 IS NOT NULL OR P238 IS NOT NULL OR P240 IS NOT NULL) GROUP BY 1
    UNION ALL
    SELECT %s, max(CASE WHEN P236 = 1 THEN 1 WHEN P236 IS NOT NULL THEN 0 END),
           max(CASE WHEN P238 = 1 THEN 1 WHEN P238 IS NOT NULL THEN 0 END),
           max(CASE WHEN P240 = 1 THEN 1 WHEN P240 IS NOT NULL THEN 0 END)
      FROM raw.e25_cap200e GROUP BY 1),
  ext AS (
    SELECT %s AS id_productor, max(CASE WHEN P704 = 1 THEN 1 WHEN P704 IS NOT NULL THEN 0 END) asistencia_tecnica,
           max(CASE WHEN P701 = 1 THEN 1 WHEN P701 IS NOT NULL THEN 0 END) capacitacion
      FROM raw.h_2014_2024 WHERE ANIO >= 2015 AND (P704 IS NOT NULL OR P701 IS NOT NULL) GROUP BY 1
    UNION ALL
    SELECT %s, max(CASE WHEN P704 = 1 THEN 1 WHEN P704 IS NOT NULL THEN 0 END),
           max(CASE WHEN P701 = 1 THEN 1 WHEN P701 IS NOT NULL THEN 0 END) FROM raw.e25_cap700 GROUP BY 1),
  aso AS (
    SELECT %s AS id_productor, max(CASE WHEN P801 = 1 THEN 1 WHEN P801 IS NOT NULL THEN 0 END) asociatividad
      FROM raw.h_2014_2024 WHERE ANIO >= 2015 AND P801 IS NOT NULL GROUP BY 1
    UNION ALL
    SELECT %s, max(CASE WHEN P801 = 1 THEN 1 WHEN P801 IS NOT NULL THEN 0 END) FROM raw.e25_cap800 GROUP BY 1)
  SELECT * FROM ins FULL JOIN ext USING (id_productor) FULL JOIN aso USING (id_productor)",
  ID_H, ID_25, ID_H, ID_25, ID_H, ID_25))

# ---------------------------------------------------------------- 3. depuración
# válido: kg > 0, ha > 0 y rendimiento entre 0.1 y 60 t/ha (fuera de ese rango es error de registro o unidad)
x("CREATE TABLE ena.papa_valido AS
  SELECT r.*, p.abono, p.fertilizante, p.plaguicida, p.asistencia_tecnica, p.capacitacion, p.asociatividad
    FROM ena.papa_registro r LEFT JOIN ena.productor p USING (id_productor)
   WHERE r.kg > 0 AND r.ha > 0 AND r.w > 0 AND r.rend_t_ha BETWEEN 0.1 AND 60")

# ---------------------------------------------------------------- 4. distrito-año
x("CREATE TABLE ena.papa_distrito AS
  SELECT anio, ubigeo, any_value(departamento) departamento, any_value(provincia) provincia, any_value(distrito) distrito,
         mode(region_cod) region_cod,
         count(*) n_registros, count(DISTINCT id_productor) n_productores,
         sum(w * kg) / sum(w * ha) / 1000                            AS rendimiento_t_ha,
         sum(w * ha)                                                 AS superficie_expandida_ha,
         sum(w * ha) / sum(w)                                        AS superficie_media_ha,
         sum(w * ha * riego) / sum(w * ha) FILTER (WHERE riego IS NOT NULL)                 AS prop_riego,
         sum(w * ha * semilla_certificada) / sum(w * ha) FILTER (WHERE semilla_certificada IS NOT NULL) AS prop_semilla_cert,
         sum(w * ha * papa_nativa_amarga) / sum(w * ha)              AS prop_papa_nativa,
         sum(w * abono) / sum(w) FILTER (WHERE abono IS NOT NULL)               AS prop_abono,
         sum(w * fertilizante) / sum(w) FILTER (WHERE fertilizante IS NOT NULL) AS prop_fertilizante,
         sum(w * plaguicida) / sum(w) FILTER (WHERE plaguicida IS NOT NULL)     AS prop_plaguicida,
         sum(w * asistencia_tecnica) / sum(w) FILTER (WHERE asistencia_tecnica IS NOT NULL) AS prop_asistencia,
         sum(w * capacitacion) / sum(w) FILTER (WHERE capacitacion IS NOT NULL) AS prop_capacitacion,
         sum(w * asociatividad) / sum(w) FILTER (WHERE asociatividad IS NOT NULL) AS prop_asociatividad,
         sum(w * least(coalesce(P220_1_CANT_1, 0), kg) ) / NULLIF(sum(w * kg), 0) AS prop_venta_aprox,
         avg(CASE WHEN fuente_ha = 'reconstruida' THEN 1.0 ELSE 0 END) AS prop_ha_reconstruida
    FROM ena.papa_valido GROUP BY anio, ubigeo")

# ---------------------------------------------------------------- 5. controles
cat("\n== Depuración por año\n")
print(q("SELECT r.anio, count(*) registros, count(v.id_productor) validos,
                round(100.0 * count(v.id_productor) / count(*), 1) pct_validos,
                round(100.0 * avg(CASE WHEN r.ha IS NULL THEN 1 ELSE 0 END), 1) pct_sin_ha
           FROM ena.papa_registro r LEFT JOIN ena.papa_valido v
             ON r.id_productor = v.id_productor AND r.rend_t_ha IS NOT DISTINCT FROM v.rend_t_ha AND r.ha IS NOT DISTINCT FROM v.ha AND r.cod_cultivo = v.cod_cultivo
          GROUP BY 1 ORDER BY 1"))
cat("\n== Rendimiento por registro (t/ha), cuantiles\n")
print(q("SELECT anio, round(quantile_cont(rend_t_ha, 0.01), 2) p01, round(quantile_cont(rend_t_ha, 0.25), 1) p25,
                round(median(rend_t_ha), 1) p50, round(quantile_cont(rend_t_ha, 0.75), 1) p75, round(quantile_cont(rend_t_ha, 0.99), 1) p99
           FROM ena.papa_registro WHERE rend_t_ha > 0 GROUP BY 1 ORDER BY 1"))
cat("\n== Distrito-año\n")
print(q("SELECT anio, count(*) distritos, sum(CASE WHEN n_productores >= 5 THEN 1 ELSE 0 END) distritos_5mas,
                round(median(n_productores), 0) med_productores,
                round(sum(rendimiento_t_ha * superficie_expandida_ha) / sum(superficie_expandida_ha), 2) rend_nacional_t_ha,
                round(median(rendimiento_t_ha), 2) rend_mediano
           FROM ena.papa_distrito GROUP BY 1 ORDER BY 1"))
cat("\n== Puno (dpto 21)\n")
print(q("SELECT anio, count(*) distritos, round(sum(rendimiento_t_ha * superficie_expandida_ha) / sum(superficie_expandida_ha), 2) rend_t_ha
           FROM ena.papa_distrito WHERE ubigeo LIKE '21%' GROUP BY 1 ORDER BY 1"))
cat("\n== Completitud de covariables (% distrito-año con dato)\n")
print(q("SELECT anio, round(100*avg(CASE WHEN prop_riego IS NOT NULL THEN 1 ELSE 0 END)) riego,
                round(100*avg(CASE WHEN prop_semilla_cert IS NOT NULL THEN 1 ELSE 0 END)) semilla,
                round(100*avg(CASE WHEN prop_abono IS NOT NULL THEN 1 ELSE 0 END)) abono,
                round(100*avg(CASE WHEN prop_fertilizante IS NOT NULL THEN 1 ELSE 0 END)) fertil,
                round(100*avg(CASE WHEN prop_asistencia IS NOT NULL THEN 1 ELSE 0 END)) asist,
                round(100*avg(CASE WHEN prop_asociatividad IS NOT NULL THEN 1 ELSE 0 END)) asoc
           FROM ena.papa_distrito GROUP BY 1 ORDER BY 1"))
dbDisconnect(con, shutdown = TRUE)
