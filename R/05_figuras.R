# =============================================================================
# 05 - Figuras del artículo (PDF vectorial para LaTeX + PNG para revisión)
#   fig1_area_estudio   : mapa del Perú por departamento: rendimiento medio y anomalía de lluvia 2023
#   fig2_variograma     : variograma espacio-temporal empírico y ajustado de los residuos
#   fig3_comparacion    : RMSE y R2 por modelo y esquema de validación
#   fig4_mapas_2025     : predicción 2025 por departamento: observado, predicho (RF espacial) y ancho del IP 95 %
#   fig5_importancia    : importancia por permutación de los predictores
# Paleta: categórica validada (dataviz), secuencial azul, divergente azul-rojo con punto medio gris.
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
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(tidyr) })
fig <- file.path(raiz, "figures"); res <- file.path(raiz, "results")
options(OutDec = ",")   # coma decimal, igual que el texto del artículo

TINTA <- "#0b0b0b"; TINTA2 <- "#52514e"; MUTED <- "#898781"; GRID <- "#e1e0d9"; NEUTRO <- "#f0efec"
SEQ <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
DIV <- c(rojo = "#d03b3b", gris = NEUTRO, azul = "#2a78d6")
MODELOS <- c(base_rf = "Réplica RF", base_gb = "Réplica GB", xgb = "XGBoost",
             rf_esp = "RF espacial", rk_st = "Kriging ET", rf_rkst = "RF + kriging ET")
COL_MOD <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300"), MODELOS)
ESQ <- c(temporal = "Temporal (2019-2024)", espacial = "Espacial (bloques)", futuro = "Futuro (2025)")

tema <- theme_minimal(base_size = 8, base_family = "sans") +
  theme(text = element_text(colour = TINTA), axis.text = element_text(colour = TINTA2),
        axis.title = element_text(colour = TINTA2), panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = GRID, linewidth = 0.25), strip.text = element_text(face = "bold", colour = TINTA),
        legend.title = element_text(colour = TINTA2), legend.text = element_text(colour = TINTA2),
        plot.margin = margin(3, 3, 3, 3))
tema_mapa <- tema + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major = element_blank(),
                          legend.position = "bottom", legend.title.position = "top", legend.title = element_text(hjust = 0.5, size = 7),
                          legend.key.height = unit(0.15, "cm"), legend.key.width = unit(0.42, "cm"),
                          plot.subtitle = element_text(size = 7.5, hjust = 0.5))
guardar <- function(p, nombre, ancho, alto) {
  ggsave(file.path(fig, paste0(nombre, ".pdf")), p, width = ancho, height = alto, units = "in", device = cairo_pdf)
  ggsave(file.path(fig, paste0(nombre, ".png")), p, width = ancho, height = alto, units = "in", dpi = 300, bg = "white")
  message("figura: ", nombre)
}

# ---------------------------------------------------------------- datos comunes
d <- cargar_base(raiz)
mapa <- st_read(file.path(raiz, "data", "mapas", "hdx_cod_ab", "per_admin_boundaries.gdb"), layer = "per_admin3", quiet = TRUE)
mapa$ubigeo <- sub("^PE", "", mapa$adm3_pcode)
mapa <- st_simplify(st_transform(mapa, 32718), dTolerance = 300)
dptos <- st_read(file.path(raiz, "data", "mapas", "hdx_cod_ab", "per_admin_boundaries.gdb"), layer = "per_admin1", quiet = TRUE) |>
  st_transform(32718) |> st_simplify(dTolerance = 1000)
con <- dbConnect(duckdb(), ruta_db(raiz), read_only = TRUE)
clim23 <- dbGetQuery(con, "SELECT ubigeo, anom_pp_campana FROM clima.distrito_campana WHERE anio = 2023")
dbDisconnect(con, shutdown = TRUE)
base_mapa <- geom_sf(data = dptos, fill = "#f6f6f3", colour = "#c3c2b7", linewidth = 0.15)

# ---------------------------------------------------------------- fig 1: mapa del Perú por departamento
# (a) rendimiento medio 2015-2025 de la papa de la sierra (estimador de razón con el factor de expansión)
# (b) anomalía de lluvia de la campaña 2023, promedio de los distritos de la base en cada departamento
con <- dbConnect(duckdb(), ruta_db(raiz), read_only = TRUE)
dep <- dbGetQuery(con, "SELECT substr(ubigeo, 1, 2) AS dd, anio, sum(w * kg) / sum(w * ha) / 1000 AS rend,
                               sum(w * kg) AS wkg, sum(w * ha) AS wha, count(*) AS n
                          FROM ena.papa_valido WHERE region_cod = 2 GROUP BY 1, 2")
dbDisconnect(con, shutdown = TRUE)
write.csv(dep[order(dep$dd, dep$anio), c("dd", "anio", "rend", "n")], file.path(res, "rendimiento_departamento_anio.csv"), row.names = FALSE)
dep_med <- dep |> group_by(dd) |> summarise(rend = sum(wkg) / sum(wha) / 1000, n = sum(n), .groups = "drop")
dep_med <- dep_med[dep_med$n >= 100, ]                                  # departamentos con al menos 100 registros
anom <- clim23 |> mutate(dd = substr(ubigeo, 1, 2)) |> filter(ubigeo %in% unique(d$ubigeo)) |>
  group_by(dd) |> summarise(anom = 100 * mean(anom_pp_campana), .groups = "drop")
dptos$dd <- sub("^PE", "", dptos$adm1_pcode)
tildes <- c(Junin = "Junín", Huanuco = "Huánuco", Apurimac = "Apurímac", Ancash = "Áncash")
dptos$nombre <- ifelse(dptos$adm1_name %in% names(tildes), tildes[dptos$adm1_name], dptos$adm1_name)
m1 <- merge(dptos, dep_med, by = "dd", all.x = TRUE)
m2 <- merge(dptos, anom[anom$dd %in% dep_med$dd, ], by = "dd", all.x = TRUE)
etiq <- st_point_on_surface(m1[!is.na(m1$rend), ])
et <- cbind(st_drop_geometry(etiq), st_coordinates(etiq))
# columna oeste (sobre el mar) y columna este (sobre la selva, sin datos)
et$lado <- ifelse(et$nombre %in% c("Amazonas", "Huánuco", "Pasco", "Junín", "Cusco", "Apurímac", "Puno", "Tacna", "Moquegua"), "este", "oeste")
x_oeste <- st_bbox(dptos)[["xmin"]] + 60000; x_este <- st_bbox(dptos)[["xmax"]] - 120000
p1a <- ggplot() + geom_sf(data = m1, aes(fill = rend), colour = "white", linewidth = 0.25) +
  ggrepel::geom_text_repel(data = subset(et, lado == "oeste"), aes(X, Y, label = nombre), size = 1.6, colour = TINTA,
                           hjust = 1, direction = "y", xlim = c(NA, x_oeste), nudge_x = x_oeste - subset(et, lado == "oeste")$X,
                           segment.size = 0.2, segment.colour = MUTED, box.padding = 0.08, min.segment.length = 0, seed = 1) +
  ggrepel::geom_text_repel(data = subset(et, lado == "este"), aes(X, Y, label = nombre), size = 1.6, colour = TINTA,
                           hjust = 0, direction = "y", xlim = c(x_este, NA), nudge_x = x_este - subset(et, lado == "este")$X,
                           segment.size = 0.2, segment.colour = MUTED, box.padding = 0.08, min.segment.length = 0, seed = 1) +
  coord_sf(xlim = st_bbox(dptos)[c(1, 3)] + c(-330000, 420000), expand = FALSE, clip = "off") +
  scale_fill_gradientn(colours = SEQ[2:7], limits = c(4, 19), breaks = c(5, 10, 15), na.value = "#f0efec", name = "t/ha") +
  labs(subtitle = "(a) Rendimiento medio") + tema_mapa
p1b <- ggplot() + geom_sf(data = m2, aes(fill = anom), colour = "white", linewidth = 0.25) +
  scale_fill_gradient2(low = DIV["rojo"], mid = DIV["gris"], high = DIV["azul"], midpoint = 0, limits = c(-35, 35),
                       oob = scales::squish, breaks = c(-30, 0, 30), na.value = "#f0efec", name = "%") +
  labs(subtitle = "(b) Lluvia 2023") + tema_mapa
guardar(p1a + p1b + plot_layout(widths = c(1.4, 1)), "fig1_area_estudio", 3.5, 2.45)
print(merge(st_drop_geometry(dptos[, c("dd", "nombre")]), merge(dep_med, anom, by = "dd", all.x = TRUE), by = "dd")[, c("nombre", "rend", "n", "anom")])

# ---------------------------------------------------------------- fig 2: variograma espacio-temporal
aux <- readRDS(file.path(res, "modelos_aux.rds"))
vg <- aux$variogramas[["futuro_F2025_rk_st"]]                       # ajustado con 2015-2024
ev <- as.data.frame(vg$emp) |> filter(!is.na(gamma), np > 0) |>
  mutate(rezago = round(as.numeric(timelag) / 365.25))
grid <- expand.grid(spacelag = seq(0, 250, 5), timelag = sort(unique(as.numeric(vg$emp$timelag))))
grid$gamma <- variogramSurface(vg$modelo, grid)$gamma
grid$rezago <- round(grid$timelag / 365.25)
COL_LAG <- setNames(SEQ[c(7, 6, 5, 4, 3)], 0:4)
p2 <- ggplot() +
  geom_line(data = grid, aes(spacelag, gamma, colour = factor(rezago)), linewidth = 0.6) +
  geom_point(data = ev, aes(dist, gamma, colour = factor(rezago)), size = 1.4, stroke = 0) +
  scale_colour_manual(values = COL_LAG, name = "Rezago (años)") +
  labs(x = "Distancia (km)", y = expression("Semivarianza ((t/ha)"^2*")")) +
  tema + theme(legend.position = "right", legend.key.height = unit(0.3, "cm"))
guardar(p2, "fig2_variograma", 3.5, 1.75)

# ---------------------------------------------------------------- fig 3: comparación de modelos
pp <- read.csv(file.path(res, "metricas_pliegue.csv")) |>
  mutate(Modelo = factor(MODELOS[modelo], levels = rev(MODELOS)), Esquema = factor(ESQ[esquema], levels = ESQ))
rs <- pp |> group_by(Esquema, Modelo) |>
  summarise(rmse_m = mean(rmse), rmse_lo = mean(rmse) - sd(rmse), rmse_hi = mean(rmse) + sd(rmse),
            r2_m = mean(r2), r2_lo = mean(r2) - sd(r2), r2_hi = mean(r2) + sd(r2), .groups = "drop")
panel <- function(m, lo, hi, xlab) {
  ggplot(rs, aes(x = .data[[m]], y = Modelo, colour = Modelo)) +
    geom_errorbar(aes(xmin = .data[[lo]], xmax = .data[[hi]]), width = 0, linewidth = 0.5, orientation = "y", na.rm = TRUE) +
    geom_point(size = 2) + facet_wrap(~ Esquema, nrow = 1) +
    scale_colour_manual(values = COL_MOD, guide = "none") + labs(x = xlab, y = NULL) + tema
}
p3 <- panel("rmse_m", "rmse_lo", "rmse_hi", "RMSE (t/ha), media ± d.e. de pliegues") /
      panel("r2_m", "r2_lo", "r2_hi", expression(R^2*", media ± d.e. de pliegues"))
guardar(p3, "fig3_comparacion", 7.16, 3.0)

# ---------------------------------------------------------------- fig 4: predicción 2025 por departamento
# resumen departamental (promedio ponderado por productores) de las predicciones distritales del RF espacial
P25 <- read.csv(file.path(res, "prediccion_2025.csv"), colClasses = c(ubigeo = "character"))
P25$dd <- substr(sprintf("%06s", P25$ubigeo), 1, 2)
P25$dd <- gsub(" ", "0", P25$dd)
dep25 <- P25 |> group_by(dd) |>
  summarise(obs = weighted.mean(obs, n_productores), pred = weighted.mean(pred, n_productores),
            ancho = weighted.mean(ls - li, n_productores), cobertura = 100 * mean(dentro), n = n(), .groups = "drop") |>
  filter(n >= 5)
write.csv(dep25, file.path(res, "prediccion_2025_departamento.csv"), row.names = FALSE)
m4 <- merge(dptos, dep25, by = "dd", all.x = TRUE)
esc_rend <- scale_fill_gradientn(colours = SEQ[2:7], limits = c(4, 19), breaks = c(5, 10, 15), oob = scales::squish,
                                 na.value = "#f0efec", name = "t/ha")
tema_mapa4 <- tema_mapa + theme(legend.key.width = unit(0.32, "cm"), legend.text = element_text(size = 6),
                                plot.subtitle = element_text(size = 7, hjust = 0.5))
p4a <- ggplot() + geom_sf(data = m4, aes(fill = obs), colour = "white", linewidth = 0.2) + esc_rend +
  labs(subtitle = "(a) Observado") + tema_mapa4
p4b <- ggplot() + geom_sf(data = m4, aes(fill = pred), colour = "white", linewidth = 0.2) + esc_rend +
  labs(subtitle = "(b) Predicho") + tema_mapa4
p4c <- ggplot() + geom_sf(data = m4, aes(fill = ancho), colour = "white", linewidth = 0.2) +
  scale_fill_gradientn(colours = c("#fde3d6", "#f6b08f", "#eb6834", "#b8441a", "#7a2a0c"), na.value = "#f0efec",
                       breaks = c(10, 15), name = "Ancho IP (t/ha)") +
  labs(subtitle = "(c) Incertidumbre") + tema_mapa4
guardar(p4a + p4b + p4c, "fig4_mapas_2025", 3.5, 2.0)
print(merge(st_drop_geometry(dptos[, c("dd", "nombre")]), dep25, by = "dd")[, c("nombre", "obs", "pred", "ancho", "cobertura", "n")], digits = 3)

# ---------------------------------------------------------------- fig 5: importancia por permutación
dev <- d[d$anio <= 2024, ]
rec <- receta(dev, PRED); X <- matriz(rec, dev)
set.seed(1)
rf <- ranger(x = X, y = dev$y_obs, num.trees = 1000, mtry = aux$hiperparametros$rf_rkst$mtry,
             min.node.size = aux$hiperparametros$rf_rkst$nodo, importance = "permutation", seed = 1, num.threads = 8)
NOMBRES <- c(superficie_media_ha = "Superficie media", prop_riego = "Riego", prop_semilla_cert = "Semilla certificada",
             prop_papa_nativa = "Papa nativa/amarga", prop_abono = "Abono orgánico", prop_fertilizante = "Fertilizante",
             prop_plaguicida = "Plaguicida", prop_asistencia = "Asistencia técnica", prop_capacitacion = "Capacitación",
             prop_asociatividad = "Asociatividad", pp_campana = "Lluvia campaña", pp_siembra = "Lluvia siembra",
             pp_crecimiento = "Lluvia crecimiento", anom_pp_campana = "Anomalía de lluvia", meses_secos = "Meses secos",
             tmin_crecimiento = "T. mínima crecimiento", tmax_crecimiento = "T. máxima crecimiento",
             amplitud_termica = "Amplitud térmica", tmin_minima = "T. mínima más baja", meses_helada = "Meses con helada")
imp <- tibble(var = names(rf$variable.importance), imp = rf$variable.importance) |>
  mutate(Grupo = ifelse(var %in% MANEJO, "Manejo (ENA)", "Clima (PISCO, TerraClimate)"),
         etiqueta = factor(NOMBRES[var], levels = NOMBRES[var][order(imp)]))
write.csv(imp, file.path(res, "importancia_permutacion.csv"), row.names = FALSE)
p5 <- ggplot(imp, aes(imp, etiqueta, fill = Grupo)) + geom_col(width = 0.7) +
  scale_fill_manual(values = c("Manejo (ENA)" = "#2a78d6", "Clima (PISCO, TerraClimate)" = "#eb6834"), name = NULL) +
  labs(x = expression("Aumento del ECM al permutar ((t/ha)"^2*")"), y = NULL) +
  tema + theme(legend.position = "top", panel.grid.major.y = element_blank())
guardar(p5, "fig5_importancia", 3.5, 2.6)
print(imp |> arrange(desc(imp)), n = 20)
