# Impacto de la energía fotovoltaica sobre murciélagos en el AMBA

Scripts de procesamiento de datos y análisis estadístico correspondientes a la tesis de maestría *"Potencial impacto de la energía fotovoltaica sobre la presencia y actividad de los murciélagos en el Área Metropolitana de Buenos Aires (AMBA)"*.

## Resumen del estudio

Este trabajo evaluó, mediante monitoreo bioacústico pasivo en un diseño comparativo pareado intrasitio, si la presencia de un panel solar fotovoltaico portátil se asocia a cambios en la actividad acústica y la riqueza de especies de murciélagos, en 5 sitios del AMBA.

## Contenido del repositorio

| Archivo | Descripción |
|---|---|
| `00_tema_visual.R` | Tema gráfico y paleta de colores compartida por los dos scripts de análisis. |
| `01_analisis_exploratorio.R` | Carga y limpieza de la base cruda, definición de la noche lógica, filtro por ventanas de muestreo, control de calidad (umbral de MATCH RATIO y reclasificación taxonómica), cálculo del índice de actividad acústica (IAA) y de la riqueza de especies, y estadística descriptiva. |
| `02_analisis_estadistico.R` | Agregación a nivel de sitio, prueba de Wilcoxon pareada (análisis principal), prueba t de sensibilidad, modelo lineal mixto, curvas de rarefacción, y correlación de Spearman entre la diferencia de IAA por sitio y el NDVI. |

## Cómo correr el análisis

1. Colocar `raw_data.xlsx` en la misma carpeta que los scripts (ver estructura esperada de columnas en la Metodología de la tesis).
2. Correr `01_analisis_exploratorio.R` de punta a punta. Exporta las tablas procesadas (`01_tabla_noche.csv`, entre otras) y las figuras a la carpeta `figuras/`.
3. Correr `02_analisis_estadistico.R`, que lee las tablas exportadas por el paso anterior y ejecuta el análisis estadístico completo.

### Paquetes de R necesarios

```r
install.packages(c("readxl", "dplyr", "lubridate", "ggplot2", "tidyr", "forcats",
                    "lunar", "hms", "scales", "coin", "vegan", "lme4", "lmerTest",
                    "ggrepel", "readr"))
```

## Autoría

Rocío Araoz — Tesis de Maestría en Ciencias Ambientales, Universidad de Buenos Aires, 2026.

## Datos

Los datos crudos se incluyen en este repositorio como `raw_data.xlsx`
