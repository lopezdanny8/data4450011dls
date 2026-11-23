# Predicción espacio-temporal del rendimiento de papa en los Andes peruanos

Kriging espacio-temporal, Random Forest espacial y XGBoost con la ENA, PISCO y TerraClimate, 2015–2025.

**Autor:** Danny Christian Lopez Pajsi. Facultad de Ingeniería Estadística e Informática, Universidad Nacional del Altiplano, Puno.

Informe reproducible hecho con **R** y **Quarto**. El artículo, en formato de conferencia IEEE, está en
[`paper/paper.qmd`](paper/paper.qmd). Al renderizarlo, todas sus cifras, tablas y figuras se calculan a partir de los
resultados y de la base incluidos en este repositorio. Ningún número está escrito a mano.

## Contenido

| Carpeta / archivo | Qué contiene |
|---|---|
| `paper/paper.qmd` | Artículo en Quarto (fuente principal) |
| `paper/plantilla_ieee.tex` | Plantilla Pandoc con la clase IEEEtran |
| `paper/fig_flujo.tex`, `paper/refs.bib` | Figura del flujo metodológico (TikZ) y bibliografía |
| `paper/paper_quarto.pdf` | PDF generado |
| `paper/paper_latex.tex` | Versión LaTeX previa del mismo artículo (referencia) |
| `R/` | Scripts numerados de la cadena de análisis y `ejecutar_todo.R` |
| `data/ena_papa_ligera.duckdb` | Base ligera (10 MB): tablas procesadas de la ENA, clima por distrito y base de modelado |
| `data/base_integrada.csv` | Base integrada distrito-año (todo el Perú) |
| `data/clima/` | PISCOp v2.1 y TerraClimate v1.1 recortados al Perú (NetCDF), con `FUENTES.md` |
| `data/mapas/` | Límites distritales COD-AB (IGN/HDX), con `FUENTES.md` |
| `results/` | Métricas, predicciones, validación robusta e inventario de datos (CSV) |
| `figures/` | Figuras en PDF y PNG |
| `renv.lock` | Versiones exactas de los paquetes de R |

## Datos

| Fuente | Datos procesados | Uso |
|---|---:|---|
| ENA 2014–2025, INEI | 3 843 598 filas; 130 098 registros de papa válidos | rendimiento y 10 variables de manejo |
| PISCOp v2.1, SENAMHI | 2 824 847 valores | 5 variables de lluvia |
| TerraClimate v1.1 | 51 209 856 valores | 5 variables de temperatura |
| Mapa distrital IGN (COD-AB) | 1 873 distritos | coordenadas y mapas |
| **Base de modelado** | **4 919 distrito-año × 21 variables** | 978 distritos de la sierra |

El detalle está en `results/inventario_datos.md`.

Los microdatos originales de la ENA (unos 14 GB en `.sav`) **no se incluyen**. Se descargan gratis del portal de
microdatos del INEI (<https://proyectos.inei.gob.pe/microdatos/>, Encuesta Nacional Agropecuaria):
- `ENA_2014_2024.sav`, la base consolidada 2014–2024;
- las carpetas `1036-Modulo1893` … `1036-Modulo1913`, que son los módulos de la ENA 2025.

## Cómo reproducir

Requisitos: R ≥ 4.4, Quarto ≥ 1.5 y una distribución LaTeX con XeLaTeX. TinyTeX sirve: se instala con
`quarto install tinytex`. Funciona igual en Positron, RStudio o la terminal.

```r
# 1. instalar las mismas versiones de paquetes
install.packages("renv")
renv::restore()
```

### Opción A: con lo incluido en el repositorio (recomendada)

Solo regenera el artículo con los resultados guardados (unos segundos):

```bash
quarto render paper/paper.qmd          # -> paper/paper_quarto.pdf
```

En **Positron** o **RStudio**: abrir la carpeta del proyecto, abrir `paper/paper.qmd` y pulsar *Preview* / *Render*.

Para rehacer también los modelos, la validación (temporal, espacial, anidada y bootstrap), la predicción de 2025,
las figuras y el PDF, a partir de la base ligera (unos 40 min):

```bash
Rscript R/ejecutar_todo.R
```

### Opción B: desde los microdatos originales de la ENA

Coloque los archivos del INEI en `data/ena/`, o indique su carpeta con la variable `ENA_DIR`. Luego:

```bash
Rscript R/ejecutar_todo.R completo
```

### Variables de entorno

| Variable | Efecto |
|---|---|
| `USAR_BASE_LIGERA=1` | Usa `data/ena_papa_ligera.duckdb` aunque exista la base completa |
| `REGENERAR_FIGURAS=0` | El render de Quarto no rehace las figuras |
| `ENA_DIR` | Carpeta con los `.sav` del INEI (solo para el script 00) |

## Cadena de análisis

| Script | Qué hace | Necesita la ENA original |
|---|---|:---:|
| `00_extraer_ena.R` | Extrae las columnas usadas de los `.sav` a DuckDB | sí |
| `01_base_papa.R` | Registros de papa, superficie en ha, rendimiento por distrito (estimador de razón) | sí |
| `02_clima_distrito.R` | PISCOp y TerraClimate por distrito y campaña (agosto–julio) | sí |
| `03_base_integrada.R` | Une ENA, clima y coordenadas en la base distrito-año | sí |
| `04_modelos.R` | 6 modelos × 3 esquemas de validación (temporal, espacial por bloques, 2025) | no |
| `05_figuras.R` | Figuras 1–5 (mapas del Perú por departamento, variograma, importancia) | no |
| `06_tabla_variables.R` | Estadísticos de las variables (Tabla II) | no |
| `07_prediccion_2025.R` | Mejor modelo y predicción 2025 con intervalos conformales al 95 % | no |
| `08_inventario_datos.R` | Volumen de datos por fuente y etapa (Tabla I) | sí |
| `09_validacion_robusta.R` | Validación cruzada anidada, bootstrap por distrito e índice de Moran | no |
| `10_base_ligera.R` | Crea la base ligera incluida en el repositorio | sí |
| `comun.R` | Particiones, receta de preprocesamiento y definición de los 6 modelos | no |

## Resultados principales

| Esquema | Mejor modelo | RMSE (t/ha) | R² | Mejora frente a la réplica de Tamayo-Vera et al. (2025) |
|---|---|---:|---:|---|
| Temporal (2019–2024) | RF espacial | 2,96 | 0,41 | 5,9 % (IC 95 %: 3,9–7,8) |
| Espacial (bloques) | RF + kriging ET | 3,38 | 0,24 | 3,3 % (IC 95 %: 2,0–4,6) |
| Futuro (2025) | RF espacial | 2,79 | 0,53 | 9,2 % (IC 95 %: 5,3–13,0) |

En 2025, los intervalos de predicción conformales al 95 % cubrieron el 97,3 % de los 585 distritos.

## Licencias de los datos

- ENA: INEI, microdatos de acceso público.
- PISCOp: SENAMHI.
- TerraClimate: Climatology Lab, CC0.
- Límites COD-AB: OCHA/HDX con fuente IGN.

Cite las fuentes originales si reutiliza los datos.
