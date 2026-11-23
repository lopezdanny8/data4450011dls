# Fuentes de los datos climáticos

| Archivo | Producto | Variable | Periodo | Resolución | Descarga |
|---|---|---|---|---|---|
| `pisco_prec_v2p1_mensual_2014_2025_peru.nc` | SENAMHI HSR PISCO Prec v2p1 unstable monthly (IRI/LDEO Climate Data Library) | Precipitación, mm/mes | 2014-01 a 2025-12 | 0,1° | 22-nov-2026, por el usuario |
| `terraclimate_tmax_2014_2025_peru.nc` | TerraClimate v1.1, agg_terraclimate_tmax_1950_CurrentYear_GLOBE (THREDDS, Northwest Knowledge Network) | Temperatura máxima, °C | 2014-01 a 2025-12 | 1/30° | 22-nov-2026, por el usuario |
| `terraclimate_tmin_2014_2025_peru.nc` | TerraClimate v1.1, agg_terraclimate_tmin_1950_CurrentYear_GLOBE (servicio NCSS del mismo THREDDS) | Temperatura mínima, °C | 2014-01 a 2025-12 | 1/30° | 22-nov-2026, por script |

Recorte: lat −18,78 a 1,02; lon −81,38 a −67,98.

URLs
- PISCO: https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.Prec/.v2p1/.unstable/.monthly/.Prec/
- TerraClimate (servicio agregado): http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
- TerraClimate v1.1 (Zenodo): https://doi.org/10.5281/zenodo.23961197

Referencias de método
- Aybar et al. (2020), Hydrological Sciences Journal 65(5):770–785, doi:10.1080/02626667.2019.1649411 — verificada en Crossref.
- Abatzoglou et al. (2018), Scientific Data 5:170191, doi:10.1038/sdata.2017.191 — pendiente de verificar.
- Abatzoglou, Hegewisch, Yin y Kalashnikov (2026), TerraClimate v1.1, Scientific Data — pendiente de confirmar publicación.

Notas: PISCOp "unstable" = meses recientes preliminares. PISCOt v1.1 solo llega a 2016, por eso la temperatura viene de TerraClimate.
