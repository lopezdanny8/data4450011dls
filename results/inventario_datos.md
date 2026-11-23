# Inventario de datos

Generado por `R/08_inventario_datos.R` a partir de los archivos y de `data/ena_papa.duckdb`.

| Fuente | Etapa | Cantidad | Detalle |
|---|---|---:|---|
| ENA | Filas de ENA_2014_2024.sav (todos los capítulos) | 3 843 598 | 2014: 196 962; 2015: 251 795; 2016: 281 677; 2017: 284 751; 2018: 368 718; 2019: 366 615; 2021: 374 116; 2022: 291 667; 2023: 582 748; 2024: 844 549 |
| ENA | Filas de cultivos 2014-2024 | 1 499 409 |  |
| ENA | Filas del módulo de cultivos ENA 2025 | 158 747 |  |
| ENA | Registros de papa 2015-2025 | 130 112 |  |
| ENA | Registros de papa válidos (0,1-60 t/ha) | 130 098 |  |
| ENA | Distrito-año con papa, todo el Perú | 7 584 |  |
| ENA | Distrito-año con papa, sierra | 7 113 |  |
| Base de modelado | Distrito-año, sierra, >= 5 productores | 4 919 | 2015: 442; 2016: 472; 2017: 460; 2018: 460; 2019: 446; 2021: 475; 2022: 462; 2023: 508; 2024: 609; 2025: 585 |
| Base de modelado | Distritos distintos | 978 |  |
| Base de modelado | Entrenamiento 2015-2024 | 4 334 |  |
| Base de modelado | Prueba 2025 | 585 |  |
| Base de modelado | Variables (objetivo + predictores) | 21 |  |
| Clima | Distrito-mes por variable | 269 712 |  |
| Clima | Distrito-campaña | 20 603 |  |
| PISCOp v2.1 | Valores en el archivo (celda-mes) | 3 792 096 | 26 334 celdas x 144 meses; lluvia, 0,1° |
| PISCOp v2.1 | Valores válidos (celdas sobre tierra) | 2 824 847 |  |
| TerraClimate Tmín | Valores en el archivo (celda-mes) | 34 529 040 | 239 785 celdas x 144 meses; 1/30° |
| TerraClimate Tmín | Valores válidos (celdas sobre tierra) | 25 604 928 |  |
| TerraClimate Tmáx | Valores en el archivo (celda-mes) | 34 529 040 | 239 785 celdas x 144 meses; 1/30° |
| TerraClimate Tmáx | Valores válidos (celdas sobre tierra) | 25 604 928 |  |
| Mapa distrital (IGN) | Distritos | 1 873 |  |
