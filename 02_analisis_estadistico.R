##############################################################################
# TESIS: Potencial impacto de la energia fotovoltaica sobre la presencia y
#        actividad de los murcielagos en Gran Buenos Aires
#
# SCRIPT 2 de 2: ANALISIS ESTADISTICO Y PUESTA A PRUEBA DE HIPOTESIS
#
# Objetivo de este script:
#   Partir de las tablas ya calculadas y exportadas por el Script 1
#   ("01_analisis_exploratorio.R") y poner a prueba las hipotesis de la
#   tesis siguiendo el diseño de muestras pareadas intrasitio definido en
#   la metodologia:
#
#     H1 (principal):  la presencia del panel reduce el indice de
#         actividad acustica (IAA = pases de murcielago por hora,
#         incluyendo NoID, excluyendo Noise).
#     H3 (secundaria): el efecto de reduccion se atenua en sitios con
#         mayor cobertura vegetal (NDVI).
#
#   La prediccion 2 (sensibilidad por gremio de forrajeo) no se evalua en
#   esta tesis: la reclasificacion de especies sin distribucion reportada
#   para el AMBA (ver Script 1, seccion de MATCH RATIO) dejo muy pocos
#   sitios con datos completos en las categorias de gremio como para
#   sostener esa comparacion.
#
#   Este script nunca recalcula el IAA ni la riqueza desde el detalle
#   crudo: lee directamente "01_tabla_noche.csv", la tabla que ya calculo
#   el Script 1, para que ambas etapas de la tesis usen siempre la misma
#   definicion de las variables respuesta.
#
# Estructura:
#   1. Carga de librerias, tema visual y tabla noche-lugar (Script 1)
#   2. Verificacion de la tabla heredada del Script 1
#   3. Agregacion a nivel de sitio: promedios por condicion y diferencias di
#   4. Exploracion visual de las diferencias (previo a H1)
#   5. Analisis principal H1: Wilcoxon de rangos con signo pareado
#   6. Analisis de sensibilidad: prueba t pareada (si corresponde)
#   7. Variable secundaria: riqueza de grupos acusticos + rarefaccion
#   8. Analisis complementario: LMM con covariables ambientales
#   9. Analisis complementario H3: atenuacion del efecto segun NDVI
#  10. Exportacion de resultados
##############################################################################


# =============================================================================
# 1. CARGA DE LIBRERIAS, TEMA VISUAL Y TABLA NOCHE-LUGAR (SCRIPT 1)
# =============================================================================

# install.packages(c("dplyr","tidyr","ggplot2","forcats","readr","scales",
#                     "coin","vegan","lme4","lmerTest","ggrepel"))

library(dplyr)      # manipulacion de tablas
library(tidyr)      # pivot_wider/pivot_longer
library(ggplot2)    # graficos
library(forcats)    # reordenar factores en graficos
library(readr)      # lectura de CSV
library(coin)       # wilcoxsign_test(): da el estadistico Z exacto, necesario para el tamaño del efecto r
library(vegan)      # specaccum(): curvas de acumulacion/rarefaccion de especies
library(lme4)       # modelos lineales mixtos (LMM)
library(lmerTest)   # agrega p-valores (por Satterthwaite) a los LMM de lme4
library(ggrepel)    # etiquetas de sitio sin superponerse en los slopegraphs

# Tema, paleta y guardar_figura(): mismo archivo que usa el Script 1, para
# que las figuras de ambos scripts tengan exactamente el mismo estilo.
source("00_tema_visual.R")

# Carpeta donde el Script 1 guardo las tablas y figuras. Tiene que ser la
# misma carpeta que se uso como 'carpeta_salida' en ese script.
carpeta_salida  <- "C:/Users/de la Tierra/Documents/RESULTADOS MURCI/ANALISIS_ESTADISTICO"
carpeta_figuras <- file.path(carpeta_salida, "figuras")
dir.create(carpeta_figuras, showWarnings = FALSE, recursive = TRUE)

# Se lee la tabla noche-lugar YA CALCULADA por el Script 1: ya trae el IAA
# (pases_por_hora, incluyendo NoID y excluyendo Noise), la riqueza de
# especies validas, el esfuerzo de muestreo (fijo, 4h por protocolo) y el
# clima (temperatura, viento, precipitacion, fase lunar) por noche. No
# hace falta -- ni conviene -- recalcular nada de esto aca: usar la misma
# tabla que el Script 1 exporta es lo que garantiza que ambas etapas de
# la tesis usen la misma definicion de IAA.
tabla_noche <- read_csv(file.path(carpeta_salida, "01_tabla_noche.csv"),
                         show_col_types = FALSE) %>%
  mutate(
    fecha       = as.Date(fecha),
    lugar       = factor(lugar),
    tratamiento = factor(tratamiento, levels = c("sin_panel", "con_panel"))
  ) %>%
  rename(IAA = pases_por_hora)

glimpse(tabla_noche)


# =============================================================================
# 2. VERIFICACION DE LA TABLA HEREDADA DEL SCRIPT 1
# =============================================================================

# Chequeo minimo de que la tabla trae las columnas que este script
# necesita, para detectar a tiempo si se cargo una version vieja o
# incompleta del CSV (por ejemplo, de antes de que el Script 1 corrigiera
# el IAA para incluir NoID).
columnas_necesarias <- c("lugar", "fecha", "tratamiento", "IAA", "riqueza",
                          "horas_esfuerzo", "temp_media", "ff_media",
                          "fase_lunar_pct", "precip_media", "NDVI_medio")
faltantes <- setdiff(columnas_necesarias, names(tabla_noche))

if (length(faltantes) > 0) {
  stop("Faltan columnas en 01_tabla_noche.csv: ", paste(faltantes, collapse = ", "),
       ". Revisar que el Script 1 se haya corrido con la version mas reciente.")
}

cat("Recordatorio: el umbral de MATCH RATIO se decide y se aplica en el",
    "Script 1, no en este script. La tabla noche-lugar ya viene filtrada.\n")

# Chequeo de diseño: noches por sitio y condicion (documentar desvios del
# diseño nominal de 3 vs 3, tal como ya se sabe que existen en algunos
# sitios).
tabla_noche %>% count(lugar, tratamiento)


# =============================================================================
# 3. AGREGACION A NIVEL DE SITIO: PROMEDIOS POR CONDICION Y DIFERENCIAS di
# =============================================================================

# Esta es la unidad de analisis del test principal: por cada sitio, el
# promedio de IAA de las noches con panel (T-barra) y de las noches sin
# panel (C-barra), y la diferencia di = T-barra - C-barra.
tabla_sitio <- tabla_noche %>%
  group_by(lugar, tratamiento) %>%
  summarise(
    IAA_prom     = mean(IAA, na.rm = TRUE),
    riqueza_prom = mean(riqueza, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(tabla_noche %>% distinct(lugar, NDVI_medio), by = "lugar") %>%
  pivot_wider(
    id_cols     = c(lugar, NDVI_medio),
    names_from  = tratamiento,
    values_from = c(IAA_prom, riqueza_prom)
  ) %>%
  rename(
    IAA_con_panel     = IAA_prom_con_panel,
    IAA_sin_panel     = IAA_prom_sin_panel,
    riqueza_con_panel = riqueza_prom_con_panel,
    riqueza_sin_panel = riqueza_prom_sin_panel
  ) %>%
  mutate(
    d_IAA     = IAA_con_panel     - IAA_sin_panel,     # di negativo = apoya H1 (el panel reduce actividad)
    d_riqueza = riqueza_con_panel - riqueza_sin_panel
  )

tabla_sitio


# =============================================================================
# 4. EXPLORACION VISUAL DE LAS DIFERENCIAS (previo a H1)
# =============================================================================

# Slopegraph pareado: la figura mas directa para mostrar lo que
# efectivamente compara el Wilcoxon -- el cambio de IAA sitio por sitio,
# sin_panel -> con_panel. Cada linea es un sitio; la pendiente y el color
# de cada segmento muestran si ese sitio bajo o subio.
datos_slope_IAA <- tabla_noche %>%
  group_by(lugar, tratamiento) %>%
  summarise(IAA = mean(IAA, na.rm = TRUE), .groups = "drop")

fig_slope_IAA <- ggplot(datos_slope_IAA, aes(x = tratamiento, y = IAA, group = lugar)) +
  geom_line(color = "grey60", linewidth = 0.6) +
  geom_point(aes(color = tratamiento), size = 3.2) +
  geom_text_repel(
    data = datos_slope_IAA %>% filter(tratamiento == "con_panel"),
    aes(label = lugar), nudge_x = 0.15, hjust = 0, size = 3.4, segment.color = "grey70"
  ) +
  scale_x_discrete(labels = etiquetas_tratamiento, expand = expansion(mult = c(0.1, 0.35))) +
  scale_color_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  labs(
    title    = "Cambio de IAA por sitio, sin panel -> con panel",
    subtitle = "Cada linea conecta el promedio de un sitio en ambas condiciones (unidad de analisis del Wilcoxon)",
    x = NULL, y = "IAA (pases / hora)"
  )

fig_slope_IAA
guardar_figura(fig_slope_IAA, "fig11_slopegraph_IAA_por_sitio", ancho_cm = 16, alto_cm = 12)

# Distribucion de las 6 diferencias di (histograma + boxplot con puntos),
# para inspeccionar su forma antes de decidir el test.
fig_hist_dIAA <- ggplot(tabla_sitio, aes(x = d_IAA)) +
  geom_histogram(bins = 6, fill = "#2C7FB8", color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#D95F02", linewidth = 0.6) +
  labs(
    title    = "Distribucion de las diferencias por sitio (IAA)",
    subtitle = expression(d[i] == IAA[con~panel] - IAA[sin~panel]),
    x = "Diferencia por sitio (pases/hora)", y = "Frecuencia"
  )

fig_hist_dIAA
guardar_figura(fig_hist_dIAA, "fig12_histograma_diferencias_IAA", ancho_cm = 14, alto_cm = 10)

fig_box_dIAA <- ggplot(tabla_sitio, aes(x = "", y = d_IAA)) +
  geom_boxplot(width = 0.35, fill = "grey92", outlier.shape = NA) +
  geom_jitter(width = 0.05, size = 2.6, color = "#2C7FB8") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#D95F02", linewidth = 0.6) +
  labs(title = "Diferencias por sitio (IAA)", x = NULL, y = "Diferencia (pases/hora)")

fig_box_dIAA
guardar_figura(fig_box_dIAA, "fig13_boxplot_diferencias_IAA", ancho_cm = 10, alto_cm = 12)

# Prueba de Shapiro-Wilk sobre las diferencias di, para decidir si
# corresponde reportar tambien la prueba t pareada como analisis de
# sensibilidad (ver seccion 6).
shapiro_IAA <- shapiro.test(tabla_sitio$d_IAA)
shapiro_IAA
# Si Shapiro-Wilk da p > 0.05 -> no hay evidencia de que las diferencias
# se aparten de la normalidad, y se reporta tambien la prueba t (seccion 6).


# =============================================================================
# 5. ANALISIS PRINCIPAL H1: WILCOXON DE RANGOS CON SIGNO PAREADO
# =============================================================================

# H0: la mediana de las diferencias di es igual a cero (el panel no
#     modifica sistematicamente el IAA).
# H1: la mediana de las diferencias di es negativa (el panel reduce
#     el IAA) -> hipotesis alternativa direccional, "menor" (less).
#
# Se usa wilcox.test() del paquete base "stats", con exact = TRUE para
# obtener el p-valor exacto (apropiado dado el n pequeño, n = 5 sitios).
test_H1_wilcox <- wilcox.test(
  x           = tabla_sitio$IAA_con_panel,
  y           = tabla_sitio$IAA_sin_panel,
  paired      = TRUE,
  alternative = "less",
  exact       = TRUE
)
test_H1_wilcox

# El estadistico Z de la prueba (necesario para el tamaño del efecto r)
# no lo devuelve wilcox.test() directamente. Se obtiene con
# coin::wilcoxsign_test(), que implementa el mismo test pero informa Z.
test_H1_z <- wilcoxsign_test(
  tabla_sitio$IAA_con_panel ~ tabla_sitio$IAA_sin_panel,
  distribution = "exact"
)
test_H1_z
Z_H1 <- statistic(test_H1_z)

# Tamaño del efecto r = Z / sqrt(N), con N = numero de pares (n = 5
# sitios), siguiendo la formula y los umbrales de interpretacion de
# Cohen (1992) indicados en la metodologia.
N_pares <- nrow(tabla_sitio)
r_H1    <- as.numeric(Z_H1) / sqrt(N_pares)

cat("Estadistico Z (H1):", round(as.numeric(Z_H1), 3), "\n")
cat("Tamaño del efecto r (H1):", round(r_H1, 3), "\n")
cat("Interpretacion: ",
    ifelse(abs(r_H1) < 0.1, "despreciable",
    ifelse(abs(r_H1) < 0.3, "pequeño",
    ifelse(abs(r_H1) < 0.5, "moderado", "grande"))), "\n")


# =============================================================================
# 6. ANALISIS DE SENSIBILIDAD: PRUEBA t PAREADA (si corresponde)
# =============================================================================

# Segun la metodologia, si las diferencias di resultan aproximadamente
# normales (Shapiro-Wilk con p > 0.05), se reporta tambien la prueba t
# de Student pareada como analisis de sensibilidad complementario al
# Wilcoxon (no como reemplazo).
if (shapiro_IAA$p.value > 0.05) {
  test_H1_ttest <- t.test(
    x           = tabla_sitio$IAA_con_panel,
    y           = tabla_sitio$IAA_sin_panel,
    paired      = TRUE,
    alternative = "less"
  )
  print(test_H1_ttest)
} else {
  cat("Shapiro-Wilk indica desvio de normalidad (p =", round(shapiro_IAA$p.value, 3),
      ") -> no se reporta t pareada, el Wilcoxon queda como test principal.\n")
}


# =============================================================================
# 7. VARIABLE SECUNDARIA: RIQUEZA DE GRUPOS ACUSTICOS
# =============================================================================

## 7.1 Mismo procedimiento pareado que para el IAA -----------------------

datos_slope_riqueza <- tabla_noche %>%
  group_by(lugar, tratamiento) %>%
  summarise(riqueza = mean(riqueza, na.rm = TRUE), .groups = "drop")

fig_slope_riqueza <- ggplot(datos_slope_riqueza, aes(x = tratamiento, y = riqueza, group = lugar)) +
  geom_line(color = "grey60", linewidth = 0.6) +
  geom_point(aes(color = tratamiento), size = 3.2) +
  geom_text_repel(
    data = datos_slope_riqueza %>% filter(tratamiento == "con_panel"),
    aes(label = lugar), nudge_x = 0.15, hjust = 0, size = 3.4, segment.color = "grey70"
  ) +
  scale_x_discrete(labels = etiquetas_tratamiento, expand = expansion(mult = c(0.1, 0.35))) +
  scale_color_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  labs(
    title    = "Cambio de riqueza por sitio, sin panel -> con panel",
    subtitle = "Cada linea conecta el promedio de un sitio en ambas condiciones",
    x = NULL, y = "Riqueza (N° especies / noche)"
  )

fig_slope_riqueza
guardar_figura(fig_slope_riqueza, "fig14_slopegraph_riqueza_por_sitio", ancho_cm = 16, alto_cm = 12)

shapiro_riqueza <- shapiro.test(tabla_sitio$d_riqueza)
shapiro_riqueza

test_riqueza_wilcox <- wilcox.test(
  x           = tabla_sitio$riqueza_con_panel,
  y           = tabla_sitio$riqueza_sin_panel,
  paired      = TRUE,
  alternative = "less",
  exact       = TRUE
)
test_riqueza_wilcox

test_riqueza_z <- wilcoxsign_test(
  tabla_sitio$riqueza_con_panel ~ tabla_sitio$riqueza_sin_panel,
  distribution = "exact"
)
Z_riqueza <- statistic(test_riqueza_z)
r_riqueza <- as.numeric(Z_riqueza) / sqrt(N_pares)

cat("Estadistico Z (riqueza):", round(as.numeric(Z_riqueza), 3), "\n")
cat("Tamaño del efecto r (riqueza):", round(r_riqueza, 3), "\n")

## 7.2 Curvas de acumulacion de especies por condicion (rarefaccion) -----

# Se arma una matriz de incidencia (noche-lugar x especie) por separado
# para cada condicion, usando el detalle crudo del Script 1 (la unica
# tabla que tiene el desglose por especie a nivel de noche individual;
# tabla_noche solo trae el total agregado). Se usan las mismas categorias
# de exclusion (Noise, NoID, Baja_confianza) ya definidas alli.
#
# Se agrupa por `noche` (noche logica, ya calculada en el Script 1), no
# por `fecha` (fecha calendario cruda de cada pase). Si se agrupara por
# `fecha`, una sesion que cruza la medianoche contaria como DOS noches de
# muestreo en la matriz de incidencia en vez de una sola, inflando
# artificialmente el numero de unidades de muestreo de la rarefaccion.
detalle <- read_csv(file.path(carpeta_salida, "01_datos_limpios_detalle.csv"),
                     show_col_types = FALSE)

stopifnot("especie_valida" %in% names(detalle))
stopifnot("noche" %in% names(detalle))

especie_noche <- detalle %>%
  filter(!especie_valida %in% c("Noise", "NoID", "Baja_confianza", "Fuera_de_rango")) %>%
  count(lugar, noche, tratamiento, especie_valida, name = "n_pases")

armar_matriz_incidencia <- function(tratamiento_elegido) {
  especie_noche %>%
    filter(tratamiento == tratamiento_elegido) %>%
    pivot_wider(
      id_cols     = c(lugar, noche),
      names_from  = especie_valida,
      values_from = n_pases,
      values_fill = 0
    ) %>%
    select(-lugar, -noche)
}

matriz_con_panel <- armar_matriz_incidencia("con_panel")
matriz_sin_panel <- armar_matriz_incidencia("sin_panel")

# method = "random" promedia muchos ordenes aleatorios de las noches, lo
# que equivale a una rarefaccion basada en el numero de noches (todos los
# sitios agrupados dentro de cada condicion, no una curva por sitio).
rarefaccion_con_panel <- specaccum(matriz_con_panel, method = "random", permutations = 100)
rarefaccion_sin_panel <- specaccum(matriz_sin_panel, method = "random", permutations = 100)

# Se convierten los objetos de specaccum a un data.frame para graficar con
# ggplot2 y mantener el mismo estilo que el resto de las figuras (en vez
# del grafico base R de vegan).
tabla_rarefaccion <- bind_rows(
  data.frame(
    tratamiento = "con_panel",
    noches      = rarefaccion_con_panel$sites,
    riqueza     = rarefaccion_con_panel$richness,
    sd          = rarefaccion_con_panel$sd
  ),
  data.frame(
    tratamiento = "sin_panel",
    noches      = rarefaccion_sin_panel$sites,
    riqueza     = rarefaccion_sin_panel$richness,
    sd          = rarefaccion_sin_panel$sd
  )
) %>%
  mutate(tratamiento = factor(tratamiento, levels = c("sin_panel", "con_panel")))

fig_rarefaccion <- ggplot(tabla_rarefaccion, aes(x = noches, y = riqueza, color = tratamiento, fill = tratamiento)) +
  geom_ribbon(aes(ymin = riqueza - sd, ymax = riqueza + sd), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_color_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  scale_fill_manual(values = paleta_tratamiento, guide = "none") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  labs(
    title    = "Curvas de rarefaccion de especies, por condicion",
    subtitle = "Riqueza acumulada vs. numero de noches muestreadas (todos los sitios agrupados por condicion)",
    x = "Numero de noches muestreadas", y = "Riqueza acumulada de especies"
  )

fig_rarefaccion
guardar_figura(fig_rarefaccion, "fig15_curvas_rarefaccion", ancho_cm = 16, alto_cm = 12)


# =============================================================================
# 8. ANALISIS COMPLEMENTARIO: LMM CON COVARIABLES AMBIENTALES
# =============================================================================

# Modelo lineal mixto complementario (no reemplaza al Wilcoxon, que sigue
# siendo el test principal): evalua si el efecto de tratamiento sobre el
# IAA persiste al controlar por temperatura, viento, precipitacion y fase
# lunar, con sitio como efecto aleatorio. Con pocos sitios, y dado que
# cada sitio se muestreo en un mes distinto (temperatura muy confundida
# con sitio), la estimacion de la varianza entre sitios es imprecisa -> los
# resultados de este modelo se interpretan con cautela, como indica la
# metodologia.
modelo_LMM_IAA <- lmer(
  IAA ~ tratamiento + temp_media + ff_media + fase_lunar_pct + precip_media + (1 | lugar),
  data = tabla_noche
)
summary(modelo_LMM_IAA)

# Chequeo basico de supuestos: residuos vs. ajustados, y qqplot de
# residuos (graficos diagnosticos estandar, se dejan en base R/lme4).
plot(modelo_LMM_IAA)
qqnorm(resid(modelo_LMM_IAA)); qqline(resid(modelo_LMM_IAA))


# =============================================================================
# 9. ANALISIS COMPLEMENTARIO H3: ATENUACION DEL EFECTO SEGUN NDVI
# =============================================================================

# H3: el efecto de reduccion del panel se atenua (di se acerca a cero o
# se vuelve positivo) en sitios con mayor NDVI.
#
# Con pocos sitios (uno por unidad de NDVI), la forma mas razonable de
# testear esto es una correlacion entre di (diferencia de IAA por sitio,
# ya calculada en tabla_sitio) y el NDVI de ese sitio. Se usa Spearman en
# vez de Pearson, en linea con el mismo criterio no parametrico usado
# para H1 dado el n pequeño.
cor_H3 <- cor.test(
  tabla_sitio$d_IAA,
  tabla_sitio$NDVI_medio,
  method      = "spearman",
  alternative = "greater"  # H3: a mayor NDVI, di menos negativo (mas alto) -> correlacion positiva
)
cor_H3

fig_H3_NDVI <- ggplot(tabla_sitio, aes(x = NDVI_medio, y = d_IAA, label = lugar)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", fill = "grey80", linewidth = 0.7) +
  geom_point(color = "#2C7FB8", size = 3.2) +
  geom_text_repel(size = 3.4, segment.color = "grey70") +
  labs(
    title    = "Diferencia de IAA (con panel - sin panel) vs. NDVI del sitio",
    subtitle = "H3: se espera una relacion positiva (a mayor NDVI, menor reduccion)",
    x = "NDVI medio del sitio", y = "Diferencia de IAA (pases/hora)"
  )

fig_H3_NDVI
guardar_figura(fig_H3_NDVI, "fig16_H3_diferencia_IAA_vs_NDVI", ancho_cm = 16, alto_cm = 12)


# =============================================================================
# 10. EXPORTACION DE RESULTADOS
# =============================================================================

write.csv(tabla_sitio, file.path(carpeta_salida, "02_tabla_sitio_IAA_riqueza.csv"), row.names = FALSE)

# Resumen final en consola con los resultados clave de cada hipotesis.
cat("========================================\n")
cat("RESUMEN DE RESULTADOS\n")
cat("========================================\n")
cat("H1 (IAA)  - Wilcoxon: W =", test_H1_wilcox$statistic,
    " p =", round(test_H1_wilcox$p.value, 4),
    " r =", round(r_H1, 3), "\n")
cat("Riqueza   - Wilcoxon: W =", test_riqueza_wilcox$statistic,
    " p =", round(test_riqueza_wilcox$p.value, 4),
    " r =", round(r_riqueza, 3), "\n")

cat("H3 (NDVI) - Spearman: rho =", round(as.numeric(cor_H3$estimate), 3),
    " p =", round(cor_H3$p.value, 4), "\n")
cat("Resultados (tabla) exportados a:", carpeta_salida, "\n")
cat("Figuras (PNG, 300 dpi) exportadas a:", carpeta_figuras, "\n")
