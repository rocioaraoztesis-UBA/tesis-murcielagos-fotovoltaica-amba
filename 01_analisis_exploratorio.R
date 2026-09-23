################################################################################
# TESIS: Potencial impacto de la energía fotovoltaica sobre la presencia y
#        actividad de los murciélagos en Gran Buenos Aires
#
# SCRIPT 1 de 2 — ANÁLISIS EXPLORATORIO Y DESCRIPTIVO
#
# Objetivo:
#   Cargar la base cruda exportada por Kaleidoscope (Auto ID), depurarla,
#   aplicar el criterio de calidad por MATCH RATIO, restringir el análisis
#   a las ventanas de muestreo de atardecer y amanecer, y describir el
#   conjunto de datos resultante mediante tablas y gráficos. Este script
#   no ajusta modelos estadísticos ni pone a prueba hipótesis: esa etapa
#   corresponde al Script 2 ("02_analisis_estadistico.R"), que toma como
#   entrada las tablas que este script exporta al final.
#
# Estructura (formato tipo artículo científico):
#   PARTE I   — Preparación de datos (carga, limpieza, diagnóstico de
#               calidad, filtro por ventanas de atardecer/amanecer)
#   PARTE II  — Criterio de calidad por MATCH RATIO y gremio de forrajeo
#   PARTE III — Variables derivadas (esfuerzo fijo, fase lunar, NDVI,
#               tablas agregadas)
#   PARTE IV  — Resultados descriptivos (actividad, riqueza, variables
#               ambientales, abundancia por especie/familia/gremio)
#   PARTE V   — Exportación de datos procesados
################################################################################


################################################################################
# PARTE I — PREPARACIÓN DE DATOS
################################################################################

# ------------------------------------------------------------------------------
# I.1 Carga de librerías y datos crudos
# ------------------------------------------------------------------------------

# install.packages(c("readxl","dplyr","lubridate","ggplot2","tidyr","forcats","lunar","hms","scales"))

library(readxl)     # lectura de archivos .xlsx
library(dplyr)      # manipulación de tablas
library(lubridate)  # manejo de fechas
library(ggplot2)    # gráficos
library(tidyr)      # reorganización de tablas (pivot_wider, replace_na)
library(forcats)    # reordenamiento de factores en gráficos
library(lunar)       # cálculo del porcentaje de iluminación lunar
library(hms)         # manejo de horas puras
library(scales)      # formato de ejes y breaks en los gráficos

# ------------------------------------------------------------------------------
# I.1.b Tema visual, paleta de colores y carpetas de salida
# ------------------------------------------------------------------------------

# El tema de ggplot2, la paleta de colores de tratamiento y la función
# guardar_figura() se definen en un archivo aparte ("00_tema_visual.R"),
# que también usa el Script 2. Así ambos scripts producen figuras con
# exactamente el mismo estilo sin mantener dos copias del mismo código —
# una diferencia entre copias fue justamente el origen del bug del IAA
# que se corrigió en la revisión anterior, y no queremos repetir ese
# patrón acá.
source("00_tema_visual.R")

# Carpetas de salida (datos procesados y figuras). Se definen acá para que
# tanto la Parte IV (que guarda las figuras) como la Parte V (que guarda las
# tablas) usen la misma ruta. El Script 2 usa esta misma carpeta bajo el
# nombre "carpeta_salida" para leer lo que este script exporta.
carpeta_salida  <- "C:/Users/de la Tierra/Documents/RESULTADOS MURCI/ANALISIS_ESTADISTICO"
carpeta_figuras <- file.path(carpeta_salida, "figuras")
dir.create(carpeta_salida,  showWarnings = FALSE, recursive = TRUE)
dir.create(carpeta_figuras, showWarnings = FALSE, recursive = TRUE)


raw <- read_excel("C:/Users/de la Tierra/Documents/RESULTADOS MURCI/ANALISIS_ESTADISTICO/raw_data.xlsx")

dim(raw)
names(raw)
glimpse(raw)


# ------------------------------------------------------------------------------
# I.2 Limpieza y recodificación de variables
# ------------------------------------------------------------------------------

datos <- raw %>%
  rename(
    lugar       = Lugar,
    match_ratio = `MATCH RATIO`,
    margin      = MARGIN,
    alt1        = `ALTERNATE 1`,
    alt2        = `ALTERNATE 2`,
    precip      = `Precp(mm)`
  ) %>%


    # NOCHE LÓGICA DE MUESTREO. `fecha` es la fecha calendario del archivo,
    # pero una sesión de grabación nocturna que arranca a la tardecita de
    # un día puede seguir después de la medianoche, ya con fecha calendario
    # del día siguiente. Si se agrupara directamente por `fecha`, esos
    # pases de la madrugada (que en realidad pertenecen a la sesión de la
    # noche anterior) quedarían separados de ella y mezclados con la
    # sesión de la noche siguiente -- exactamente el problema detectado al
    # revisar los rangos horarios de `01_tabla_noche.csv` (varias noches
    # con ~24 horas de "esfuerzo" porque la primera detección del día
    # calendario era la cola de la noche anterior, ya pasada la
    # medianoche). Se define entonces `noche` como la fecha en que empezó
    # la sesión: si la hora es de madrugada (antes del mediodía), la
    # detección se asigna a la noche que arrancó el día calendario
    # anterior; si es de tarde/noche, se asigna a la noche que arranca ese
    # mismo día. El mediodía es un corte seguro porque no hay actividad de
    # murciélagos ni de "Noise" a esa hora.
    noche = if_else(hour(hora) < 12, fecha - days(1), fecha),

    # Varias columnas numéricas (TEMP, PNM, MATCH RATIO, MARGIN) contienen en
    # el Excel original una mezcla de celdas guardadas como número y como
    # texto. read_excel() es estricto con el tipo de celda: al encontrar esa
    # mezcla puede leer la columna entera como texto. Se fuerza la
    # conversión numérica de forma explícita para evitar este problema.
    TEMP        = as.numeric(trimws(as.character(TEMP))),
    HUM         = as.numeric(trimws(as.character(HUM))),
    PNM         = as.numeric(trimws(as.character(PNM))),
    DD          = as.numeric(trimws(as.character(DD))),
    FF          = as.numeric(trimws(as.character(FF))),
    precip      = as.numeric(trimws(as.character(precip))),
    match_ratio = as.numeric(trimws(as.character(match_ratio))),
    margin      = as.numeric(trimws(as.character(margin))),
    n_pulsos    = as.numeric(trimws(as.character(n_pulsos))),

    # DD (dirección del viento, en grados) usa el código meteorológico
    # estándar "990" para "viento variable" -- no es un ángulo real. Se
    # documenta esa condición aparte y se reemplaza por NA en la columna
    # numérica.
    DD_cod = case_when(
      DD == 990 ~ "variable",
      is.na(DD) ~ NA_character_,
      TRUE      ~ "definida"
    ),
    DD = ifelse(DD == 990, NA, DD)
  )


# ------------------------------------------------------------------------------
# I.2.b Verificación del tratamiento de Monte_blanco
# ------------------------------------------------------------------------------

# Registro de control: fecha x tratamiento de Monte_blanco, para poder
# verlo de un vistazo y detectar a tiempo si el Excel de origen cambiara
# esta asignación.
mb_estado <- datos %>%
  filter(lugar == "Monte_blanco") %>%
  distinct(fecha, tratamiento) %>%
  arrange(fecha)

cat("Verificación Monte_blanco (fecha x tratamiento):\n")
print(mb_estado)


# ------------------------------------------------------------------------------
# I.3 Diagnóstico de calidad de datos
# ------------------------------------------------------------------------------

sapply(datos, function(x) sum(is.na(x)))

sapply(datos[c("TEMP", "HUM", "PNM", "DD", "FF", "precip", "match_ratio", "margin", "n_pulsos")],
       function(x) sum(is.na(x)))

table(datos$DD_cod, useNA = "always")

# Cobertura temporal general por sitio (en base a la noche lógica, no a la
# fecha calendario cruda)
datos %>%
  group_by(lugar) %>%
  summarise(
    noche_inicio     = min(noche),
    noche_fin        = max(noche),
    noches_distintas = n_distinct(noche)
  )

# Diseño realmente ejecutado (noches por sitio y tratamiento). Esta tabla
# debe compararse con el diseño nominal de 3 noches con panel + 3 sin panel
# por sitio descrito en la metodología: cualquier desbalance encontrado acá
# tiene que quedar explícito en la tesis, no asumirse resuelto.
diseno_realizado <- datos %>%
  group_by(lugar, tratamiento) %>%
  summarise(
    noche_inicio     = min(noche),
    noche_fin        = max(noche),
    noches_distintas = n_distinct(noche),
    .groups = "drop"
  )

diseno_realizado

# Alerta si alguna noche lógica tiene más de un tratamiento asignado. Con
# la fecha calendario cruda esto no se detectaba nunca (cada fecha tenía
# un solo tratamiento asignado en la base). Con la noche lógica, sí puede
# pasar en el límite entre dos condiciones -- por ejemplo, si la sesión
# que arranca la última noche de "sin_panel" sigue después de medianoche
# ya con el archivo de esa madrugada etiquetado "con_panel" en la base
# (porque el panel se instaló esa misma madrugada, o porque la etiqueta de
# tratamiento se asignó por fecha calendario en el armado de la base
# cruda). Si aparece algo acá, HAY QUE decidirlo a mano -- no es algo que
# el script pueda resolver solo -- y típicamente implica revisar la
# planilla de campo para saber a qué condición pertenece realmente esa
# franja horaria.
chequeo_tratamiento_mixto <- datos %>%
  group_by(lugar, noche) %>%
  summarise(n_tratamientos = n_distinct(tratamiento), .groups = "drop") %>%
  filter(n_tratamientos > 1)

if (nrow(chequeo_tratamiento_mixto) > 0) {
  warning("Hay noches logicas con mas de un tratamiento asignado. Revisar manualmente con la planilla de campo antes de continuar -- ver 'chequeo_tratamiento_mixto'.")
  print(chequeo_tratamiento_mixto)
} else {
  cat("OK: ninguna noche lógica quedó con más de un tratamiento asignado.\n")
}

# Cuántos pases se reasignaron de fecha calendario a la noche anterior
# (es decir, madrugada de un día que en realidad pertenece a la sesión de
# la noche calendario previa). Sirve como registro de cuánto corrigió la
# noche lógica, para poder mencionarlo en el texto metodológico.
n_reasignados <- sum(datos$noche != datos$fecha)
cat("Pases reasignados a la noche anterior por cruce de medianoche:", n_reasignados,
    "de", nrow(datos), "\n")

# Detalle de los pases de madrugada (00:00-06:00) ya reasignados a la
# noche anterior mediante la columna `noche`. Se deja este detalle por
# sitio para poder chequearlo contra la planilla de campo si hace falta,
# pero la corrección en sí ya se aplicó arriba.
chequeo_madrugada <- datos %>%
  filter(hour(hora) >= 0 & hour(hora) < 6) %>%
  distinct(lugar, fecha, noche, tratamiento) %>%
  arrange(lugar, fecha)

cat("Combinaciones lugar-fecha con pases de madrugada (ya reasignados a la noche anterior):\n")
print(chequeo_madrugada)

# Alerta de pases con menos de 2 pulsos. La definición de "pase" en la
# metodología (>= 2 pulsos separados por < 1 segundo) implica que un
# archivo con n_pulsos < 2 no debería contarse como pase válido. Se separan
# acá para decidir si se excluyen o si el campo n_pulsos no es comparable
# 1 a 1 con esa definición (por ejemplo, si Kaleidoscope ya pre-filtra por
# este criterio antes de generar el archivo).
chequeo_pulsos_bajos <- datos %>%
  filter(!especie_auto_id %in% c("Noise"), n_pulsos < 2)

cat("Filas con n_pulsos < 2 (fuera de Noise):", nrow(chequeo_pulsos_bajos), "\n")


# ------------------------------------------------------------------------------
# I.4 Filtro por ventanas de actividad pico (atardecer y amanecer)
# ------------------------------------------------------------------------------
#
# Como las ventanas son fijas y conocidas de antemano por protocolo, el
# esfuerzo de muestreo no depende de los propios datos acústicos: pasa a
# ser un valor constante por noche (2 h + 2 h = 4 h), evitando que un
# posible efecto del panel sobre el momento de actividad distorsione el
# propio proxy usado para medirla.
ventanas_mes <- tribble(
  ~mes, ~atardecer_inicio,        ~atardecer_fin,           ~amanecer_inicio,         ~amanecer_fin,
  1,    hms::as_hms("20:08:00"),  hms::as_hms("22:08:00"),  hms::as_hms("05:56:00"),  hms::as_hms("07:56:00"),  # enero (Casa_Blanca)
  2,    hms::as_hms("19:45:00"),  hms::as_hms("21:45:00"),  hms::as_hms("04:30:00"),  hms::as_hms("06:30:00"),  # febrero (Monte_blanco)
  4,    hms::as_hms("18:25:00"),  hms::as_hms("20:25:00"),  hms::as_hms("05:15:00"),  hms::as_hms("07:15:00"),  # abril (Naudir)
  9,    hms::as_hms("18:50:00"),  hms::as_hms("20:50:00"),  hms::as_hms("04:50:00"),  hms::as_hms("06:50:00"),  # septiembre (Maschwitz_Village)
  10,   hms::as_hms("19:15:00"),  hms::as_hms("21:15:00"),  hms::as_hms("04:00:00"),  hms::as_hms("06:00:00")   # octubre (Los_Robles)
)

# El mes se toma de `noche` (no de `fecha`), para que una detección de
# madrugada ya reasignada a la noche anterior use la ventana del mes de esa
# noche, no la del mes calendario en que cayó la detección.
datos <- datos %>%
  mutate(mes = month(noche)) %>%
  left_join(ventanas_mes, by = "mes")

# Chequeo: noches cuyo mes no tiene ventana definida en la tabla de arriba.
# Si aparece algo acá, hay que agregar ese mes a `ventanas_mes` antes de
# seguir -- esas noches quedarían sin ventana y se perderían del análisis.
sin_ventana <- datos %>%
  filter(is.na(atardecer_inicio)) %>%
  distinct(lugar, noche, mes)

if (nrow(sin_ventana) > 0) {
  warning("Hay noches con un mes sin ventana de atardecer/amanecer definida en 'ventanas_mes'. Revisar 'sin_ventana' antes de continuar.")
  print(sin_ventana)
}

datos <- datos %>%
  mutate(
    en_ventana_pico = (hora >= atardecer_inicio & hora < atardecer_fin) |
                       (hora >= amanecer_inicio  & hora < amanecer_fin),
    # Qué ventana corresponde a cada detección (NA para las que van a
    # quedar filtradas por no caer en ninguna de las dos).
    momento = case_when(
      hora >= atardecer_inicio & hora < atardecer_fin ~ "atardecer",
      hora >= amanecer_inicio  & hora < amanecer_fin  ~ "amanecer",
      TRUE ~ NA_character_
    )
  )

n_antes_ventana <- nrow(datos)

cat("=== Filtro por ventana de actividad pico ===\n")
cat("Filas antes del filtro:", n_antes_ventana, "\n")
cat("Filas dentro de las ventanas de atardecer/amanecer:", sum(datos$en_ventana_pico),
    sprintf(" (%.1f%% del total)\n", 100 * sum(datos$en_ventana_pico) / n_antes_ventana))

datos <- datos %>% filter(en_ventana_pico)


################################################################################
# PARTE II — CRITERIO DE CALIDAD POR MATCH RATIO
################################################################################

# ------------------------------------------------------------------------------
# II.1 Manejo de identificaciones NoID / Noise / ALTERNATE 1-2
# ------------------------------------------------------------------------------

# Kaleidoscope reporta, además de la especie asignada, una 2da y 3ra especie
# candidata (ALTERNATE 1 y 2). Para archivos NoID, un ALTERNATE presente
# indica candidatas cercanas que no alcanzaron el umbral de coincidencia.
datos <- datos %>%
  mutate(
    revisar_manual = especie_auto_id == "NoID" & (!is.na(alt1) | !is.na(alt2))
  )

datos %>%
  filter(especie_auto_id == "NoID") %>%
  count(revisar_manual)


# ------------------------------------------------------------------------------
# II.2 Estadística descriptiva del Auto ID y selección del umbral de MATCH RATIO
# ------------------------------------------------------------------------------

datos %>%
  count(especie_auto_id, sort = TRUE)

datos %>%
  filter(especie_auto_id != "Noise") %>%
  summarise(
    n       = n(),
    media   = mean(match_ratio, na.rm = TRUE),
    mediana = median(match_ratio, na.rm = TRUE),
    minimo  = min(match_ratio, na.rm = TRUE),
    maximo  = max(match_ratio, na.rm = TRUE),
    pct_25  = quantile(match_ratio, 0.25, na.rm = TRUE),
    pct_75  = quantile(match_ratio, 0.75, na.rm = TRUE)
  )

ggplot(datos %>% filter(especie_auto_id != "Noise"), aes(x = match_ratio)) +
  geom_histogram(binwidth = 0.05, boundary = 0) +
  labs(
    title = "Distribución de MATCH RATIO (todas las detecciones, sin Noise)",
    x     = "Match ratio (pulsos coincidentes / pulsos totales)",
    y     = "Cantidad de archivos"
  ) +
  theme_minimal()

## Selección del umbral -------------------------------------------------------
#
# Sin datos de validación manual disponibles, el umbral se fija en un valor
# propio del criterio de tesis (0.50), en base a la distribución observada
# de MATCH RATIO (histograma de arriba) y al boxplot por especie (más
# abajo).

UMBRAL_MATCH_RATIO <- 0.50


datos %>%
  filter(!especie_auto_id %in% c("Noise", "NoID")) %>%
  ggplot(aes(
    x = fct_reorder(especie_auto_id, match_ratio, .fun = median),
    y = match_ratio
  )) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Match ratio por especie (Auto ID)", x = "Especie", y = "Match ratio") +
  theme_minimal()

## Aplicación del filtro: columna 'especie_valida' ----------------------------

datos <- datos %>%
  mutate(
    especie_valida = case_when(
      especie_auto_id == "Noise"        ~ "Noise",
      especie_auto_id == "NoID"         ~ "NoID",
      # Especies sin registros previos reportados para el AMBA (revisión
      # del comité de tesis): se reclasifican como "Fuera_de_rango"
      # independientemente de su MATCH RATIO. Se interpretan como
      # identificaciones erróneas del software -- el pulso detectado es
      # un pase real de murciélago (cuenta para el IAA), pero no se
      # puede confiar en la especie asignada (se excluye de riqueza y de
      # gremio de forrajeo, igual que Baja_confianza y NoID).
      especie_auto_id %in% c("MOLRUF", "EPTBRA", "EUMGLA", "PROCEN", "NYCLAT", "EUMPER") ~ "Fuera_de_rango",
      match_ratio >= UMBRAL_MATCH_RATIO ~ especie_auto_id,
      TRUE                              ~ "Baja_confianza"
    )
  )

datos %>% count(especie_valida, sort = TRUE)

cat("Pases reclasificados como 'Fuera_de_rango' (Molossus rufus, Eptesicus brasiliensis, Eumops glaucinus, Promops centralis, Nyctinomops laticaudatus, Eumops perotis):",
    sum(datos$especie_valida == "Fuera_de_rango"), "\n")

# Tabla comparativa: pases por especie antes y después del filtro.
comparacion_filtro_especie <- datos %>%
  filter(!especie_auto_id %in% c("Noise", "NoID")) %>%
  count(especie_auto_id, name = "antes_del_filtro") %>%
  left_join(
    datos %>%
      filter(!especie_valida %in% c("Noise", "NoID", "Baja_confianza", "Fuera_de_rango")) %>%
      count(especie_valida, name = "despues_del_filtro") %>%
      rename(especie_auto_id = especie_valida),
    by = "especie_auto_id"
  ) %>%
  mutate(despues_del_filtro = replace_na(despues_del_filtro, 0)) %>%
  arrange(desc(antes_del_filtro))

comparacion_filtro_especie

datos %>%
  filter(!especie_valida %in% c("Noise", "NoID", "Baja_confianza", "Fuera_de_rango")) %>%
  count(especie_valida) %>%
  ggplot(aes(x = fct_reorder(especie_valida, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Cantidad de detecciones por especie (después del filtro por MATCH RATIO)",
    x     = "Especie",
    y     = "N de detecciones"
  ) +
  theme_minimal()

total_pases_especie <- datos %>%
  filter(!especie_valida %in% c("Noise", "NoID", "Baja_confianza", "Fuera_de_rango")) %>%
  count(especie_valida, name = "total_pases") %>%
  rename(especie = especie_valida) %>%
  arrange(desc(total_pases))

total_pases_especie

total_pases_especie_tratamiento <- datos %>%
  filter(!especie_valida %in% c("Noise", "NoID", "Baja_confianza", "Fuera_de_rango")) %>%
  count(especie_valida, tratamiento, name = "total_pases") %>%
  rename(especie = especie_valida) %>%
  pivot_wider(names_from = tratamiento, values_from = total_pases, values_fill = 0)

total_pases_especie_tratamiento


################################################################################
# PARTE III — VARIABLES DERIVADAS
################################################################################

# ------------------------------------------------------------------------------
# III.1 Esfuerzo de muestreo por noche
# ------------------------------------------------------------------------------

# El esfuerzo de muestreo no se estima a partir de la primera y la última
# detección: desde que el análisis se restringe a las ventanas fijas de
# atardecer y amanecer (sección I.4), el esfuerzo por noche es un valor
# constante determinado por protocolo, no por los datos acústicos
# observados: 2 horas de atardecer + 2 horas de amanecer = 4 horas.
#
HORAS_ESFUERZO_POR_VENTANA <- 4

esfuerzo <- datos %>%
  distinct(lugar, noche) %>%
  mutate(horas_esfuerzo = HORAS_ESFUERZO_POR_VENTANA)

summary(esfuerzo$horas_esfuerzo)


# ------------------------------------------------------------------------------
# III.2 Fase lunar por noche
# ------------------------------------------------------------------------------

fase_lunar <- datos %>%
  distinct(lugar, noche) %>%
  mutate(fase_lunar_pct = lunar.illumination(noche) * 100)

summary(fase_lunar$fase_lunar_pct)

ggplot(fase_lunar, aes(x = noche, y = fase_lunar_pct)) +
  geom_col() +
  facet_wrap(~ lugar, scales = "free_x") +
  labs(
    title = "Porcentaje de iluminación lunar por noche de muestreo",
    x     = "Noche (fecha de inicio de la sesión)",
    y     = "Iluminación lunar (%)"
  ) +
  theme_minimal()


# ------------------------------------------------------------------------------
# III.3 Tablas agregadas (noche-lugar y noche-lugar-especie)
# ------------------------------------------------------------------------------

# NDVI por sitio (predicción 3 de la hipótesis de trabajo: la vegetación
# circundante atenúa el efecto del panel). Es un valor fijo por sitio, no
# varía noche a noche. Si esta columna no existe en la base cruda, el
# script corta acá con un mensaje claro en vez de fallar más adelante con
# un error críptico de columna inexistente.
if (!"NDVI_medio" %in% names(datos)) {
  stop("Falta la columna 'NDVI_medio' en raw_data.xlsx. ",
       "Es necesaria para la predicción 3 (efecto modulador de la vegetación). ",
       "Agregala al Excel (un valor por sitio) y volvé a correr el script.")
}

ndvi_lugar <- datos %>%
  distinct(lugar, NDVI_medio)

ndvi_lugar

clima_noche <- datos %>%
  group_by(lugar, noche) %>%
  summarise(
    temp_media   = mean(TEMP,   na.rm = TRUE),
    hum_media    = mean(HUM,    na.rm = TRUE),
    pnm_media    = mean(PNM,    na.rm = TRUE),
    ff_media     = mean(FF,     na.rm = TRUE),
    precip_media = mean(precip, na.rm = TRUE),
    .groups = "drop"
  )

# tabla_noche: unidad de análisis principal (noche-sitio).
#
# n_pases_murcielago = TODOS los pases que corresponden a un murciélago,
#   identificado o no (especie_valida distinto de "Noise"). Esta es la base
#   del IAA / pases_por_hora, la variable respuesta principal de la tesis.
# n_especies_validas  = solo pases con especie identificada y confiable
#   (excluye NoID, Noise y Baja_confianza). Es la base de la riqueza, no de
#   la actividad total.
tabla_noche <- datos %>%
  group_by(lugar, noche, tratamiento) %>%
  summarise(
    n_total_archivos    = n(),
    n_pases_murcielago  = sum(especie_valida != "Noise"),
    n_especies_validas  = sum(!especie_valida %in% c("NoID", "Noise", "Baja_confianza", "Fuera_de_rango")),
    n_baja_confianza    = sum(especie_valida == "Baja_confianza"),
    n_noid              = sum(especie_valida == "NoID"),
    n_noise             = sum(especie_valida == "Noise"),
    riqueza             = n_distinct(especie_valida[!especie_valida %in% c("NoID", "Noise", "Baja_confianza", "Fuera_de_rango")]),
    .groups = "drop"
  ) %>%
  left_join(esfuerzo,    by = c("lugar", "noche")) %>%
  left_join(clima_noche, by = c("lugar", "noche")) %>%
  left_join(fase_lunar,  by = c("lugar", "noche")) %>%
  left_join(ndvi_lugar,  by = "lugar") %>%
  mutate(
    pases_por_hora = n_pases_murcielago / horas_esfuerzo
  ) %>%
  # Se renombra `noche` a `fecha` en la tabla final: mantiene compatible el
  # resto de este script (gráficos, tablas de diferencias) y el Script 2
  # (que lee esta tabla y espera una columna `fecha`), sin tener que tocar
  # ese código. Lo que cambió es lo que hay ADENTRO de esa columna: ahora
  # es la fecha de inicio de la noche lógica, no la fecha calendario cruda
  # de cada pase individual.
  rename(fecha = noche)

glimpse(tabla_noche)

# Tabla simple: conteo de pases de murciélago por sitio, separado por
# ventana (atardecer/amanecer) de cada noche -- sin dividir por horas de
# esfuerzo, es decir, sin calcular el IAA. Un "pase" acá es cualquier
# archivo que no sea "Noise" (incluye especies identificadas,
# Baja_confianza y NoID), igual criterio que el numerador del IAA, pero
# mostrado como conteo crudo.
#
# `dia` numera las noches de cada sitio en orden cronológico (1, 2, 3...),
# no la fecha calendario -- así se puede comparar "atardecer día 1" de un
# sitio contra "atardecer día 1" de otro sin importar en qué mes se
# muestreó cada uno.
#
# Se usa tidyr::crossing() + left_join() (en vez de agrupar directo) para
# que las combinaciones sitio-noche-ventana sin ningún pase (por ejemplo,
# Monte_blanco no tiene NINGUNA detección en la ventana de amanecer)
# queden explícitas con n_pases = 0, en vez de faltar directamente de la
# tabla.
conteo_ventana <- datos %>%
  group_by(lugar, noche, tratamiento, momento) %>%
  summarise(n_pases = sum(especie_valida != "Noise"), .groups = "drop")

noches_existentes <- tabla_noche %>%
  distinct(lugar, fecha, tratamiento) %>%
  rename(noche = fecha)

pases_por_ventana <- noches_existentes %>%
  tidyr::crossing(momento = c("atardecer", "amanecer")) %>%
  left_join(conteo_ventana, by = c("lugar", "noche", "tratamiento", "momento")) %>%
  mutate(n_pases = replace_na(n_pases, 0)) %>%
  group_by(lugar) %>%
  mutate(dia = dense_rank(noche)) %>%
  ungroup() %>%
  mutate(momento = factor(momento, levels = c("atardecer", "amanecer"))) %>%
  arrange(lugar, dia, momento) %>%
  mutate(
    etiqueta = paste0(if_else(momento == "atardecer", "Atardecer", "Amanecer"), " día ", dia)
  ) %>%
  rename(fecha = noche) %>%
  select(lugar, dia, momento, etiqueta, fecha, tratamiento, n_pases)

cat("=== Pases de murciélago por sitio, día y ventana (sin IAA) ===\n")
print(pases_por_ventana, n = Inf)

tabla_especie_noche <- datos %>%
  filter(!especie_valida %in% c("NoID", "Noise", "Baja_confianza", "Fuera_de_rango")) %>%
  group_by(lugar, noche, tratamiento, especie_valida) %>%
  summarise(n_pases = n(), .groups = "drop") %>%
  rename(especie = especie_valida) %>%
  left_join(esfuerzo,   by = c("lugar", "noche")) %>%
  left_join(fase_lunar, by = c("lugar", "noche")) %>%
  left_join(ndvi_lugar, by = "lugar") %>%
  rename(fecha = noche)

glimpse(tabla_especie_noche)


# ------------------------------------------------------------------------------
# III.4 Tabla binomial por ventana (atardecer/amanecer, bins de 1 hora)
# ------------------------------------------------------------------------------

# Se arma una tabla alternativa para un modelo
# binomial logístico: en vez de la tasa continua "pases por hora" (IAA),
# se discretiza cada ventana de 2 horas (atardecer o amanecer) en dos
# "ensayos" de 1 hora cada uno, y se registra si hubo al menos un pase de
# murciélago (Horas_murcielago, 0/1/2) sobre el total de horas de esa
# ventana (Horas_totales, siempre 2). La unidad de análisis pasa a ser
# noche x ventana (atardecer/amanecer), no la noche completa -- son más
# filas que en `tabla_noche`, lo que le da más potencia al modelo que
# corre en 03_modelo_binomial.R.
datos <- datos %>%
  mutate(
    bin_ventana = case_when(
      momento == "atardecer" & hora < atardecer_inicio + hms::hms(hours = 1) ~ 1L,
      momento == "atardecer"                                                  ~ 2L,
      momento == "amanecer"  & hora < amanecer_inicio  + hms::hms(hours = 1) ~ 1L,
      momento == "amanecer"                                                  ~ 2L,
      TRUE ~ NA_integer_
    )
  )

# Presencia/ausencia de murciélago (especie_valida != "Noise") en cada bin
# de 1 hora que efectivamente tiene al menos una detección registrada.
presencia_bin <- datos %>%
  filter(!is.na(bin_ventana)) %>%
  group_by(lugar, noche, tratamiento, momento, bin_ventana) %>%
  summarise(presencia = as.integer(any(especie_valida != "Noise")), .groups = "drop")

# Grilla completa de noche x ventana x bin: un bin sin ninguna fila en
# `datos` (ni siquiera "Noise") significa que no hubo ninguna detección
# esa hora -- se completa con presencia = 0 en vez de que ese bin
# directamente falte de la tabla.
noches_tratamiento <- datos %>% distinct(lugar, noche, tratamiento)

grid_bins <- noches_tratamiento %>%
  tidyr::crossing(momento = c("atardecer", "amanecer"), bin_ventana = c(1L, 2L))

tabla_binomial <- grid_bins %>%
  left_join(presencia_bin, by = c("lugar", "noche", "tratamiento", "momento", "bin_ventana")) %>%
  mutate(presencia = replace_na(presencia, 0L)) %>%
  group_by(lugar, noche, tratamiento, momento) %>%
  summarise(
    Horas_totales    = n(),          # = 2 (dos bins de 1h por ventana)
    Horas_murcielago = sum(presencia),
    .groups = "drop"
  ) %>%
  left_join(clima_noche, by = c("lugar", "noche")) %>%
  left_join(fase_lunar,  by = c("lugar", "noche")) %>%
  left_join(ndvi_lugar,  by = "lugar") %>%
  rename(fecha = noche)

cat("=== Tabla binomial por ventana (atardecer/amanecer) ===\n")
glimpse(tabla_binomial)


################################################################################
# PARTE IV — RESULTADOS DESCRIPTIVOS
################################################################################

# ------------------------------------------------------------------------------
# IV.0 Estadística descriptiva: general y por sitio
# ------------------------------------------------------------------------------

# Función auxiliar para no repetir el mismo bloque de summarise() en cada
# tabla: calcula n, media, mediana, desvío estándar, error estándar,
# intervalo de confianza del 95% (t de Student), mínimo y máximo de una
# variable ya agrupada con group_by() previo.
resumen_stat <- function(df, var) {
  df %>%
    summarise(
      n       = n(),
      media   = mean({{ var }}, na.rm = TRUE),
      mediana = median({{ var }}, na.rm = TRUE),
      de      = sd({{ var }}, na.rm = TRUE),
      ee      = de / sqrt(n),
      ic_inf  = media - qt(0.975, df = n - 1) * ee,
      ic_sup  = media + qt(0.975, df = n - 1) * ee,
      min     = min({{ var }}, na.rm = TRUE),
      max     = max({{ var }}, na.rm = TRUE),
      .groups = "drop"
    )
}

# --- Estadística GENERAL (agrupando todos los sitios), por tratamiento -----

resumen_general_iaa <- tabla_noche %>%
  group_by(tratamiento) %>%
  resumen_stat(pases_por_hora)

resumen_general_riqueza <- tabla_noche %>%
  group_by(tratamiento) %>%
  resumen_stat(riqueza)

cat("=== Estadística descriptiva GENERAL — IAA (pases/hora) por tratamiento ===\n")
print(resumen_general_iaa)

cat("=== Estadística descriptiva GENERAL — riqueza por tratamiento ===\n")
print(resumen_general_riqueza)

# --- Estadística POR SITIO, dentro de cada tratamiento ----------------------

resumen_sitio_iaa <- tabla_noche %>%
  group_by(lugar, tratamiento) %>%
  resumen_stat(pases_por_hora)

resumen_sitio_riqueza <- tabla_noche %>%
  group_by(lugar, tratamiento) %>%
  resumen_stat(riqueza)

cat("=== Estadística descriptiva POR SITIO — IAA (pases/hora) ===\n")
print(resumen_sitio_iaa, n = Inf)

cat("=== Estadística descriptiva POR SITIO — riqueza ===\n")
print(resumen_sitio_riqueza, n = Inf)

# --- Diferencias pareadas por sitio (T-barra - C-barra), insumo directo del Script 2 ---
# Se calculan y muestran ya en esta etapa exploratoria porque son el dato
# que efectivamente entra al test de Wilcoxon: conviene revisarlas acá,
# antes de pasar al análisis inferencial.

diferencias_pareadas_iaa <- resumen_sitio_iaa %>%
  select(lugar, tratamiento, media) %>%
  pivot_wider(names_from = tratamiento, values_from = media) %>%
  mutate(diferencia_con_menos_sin = con_panel - sin_panel)

cat("=== Diferencias pareadas por sitio — IAA (con_panel - sin_panel) ===\n")
print(diferencias_pareadas_iaa)

diferencias_pareadas_riqueza <- resumen_sitio_riqueza %>%
  select(lugar, tratamiento, media) %>%
  pivot_wider(names_from = tratamiento, values_from = media) %>%
  mutate(diferencia_con_menos_sin = con_panel - sin_panel)

cat("=== Diferencias pareadas por sitio — riqueza (con_panel - sin_panel) ===\n")
print(diferencias_pareadas_riqueza)

# --- Estadística descriptiva de variables ambientales, por sitio -----------

resumen_ambiental_sitio <- tabla_noche %>%
  group_by(lugar) %>%
  summarise(
    # OJO CON EL ORDEN: temp_de tiene que calcularse ANTES de que
    # temp_media se sobrescriba con su propio promedio. Si se calcula
    # despues, sd() opera sobre el valor ya colapsado a un solo numero (la
    # media), y sd() de un unico valor da NA -- exactamente el bug que
    # esto corrige (daba NA en los 5 sitios).
    temp_de      = sd(temp_media,     na.rm = TRUE),
    temp_media   = mean(temp_media,   na.rm = TRUE),
    hum_media    = mean(hum_media,    na.rm = TRUE),
    ff_media     = mean(ff_media,     na.rm = TRUE),
    precip_media = mean(precip_media, na.rm = TRUE),
    lunar_media  = mean(fase_lunar_pct, na.rm = TRUE),
    NDVI_medio   = first(NDVI_medio),
    .groups = "drop"
  )

cat("=== Variables ambientales, resumen por sitio ===\n")
print(resumen_ambiental_sitio)


# ------------------------------------------------------------------------------
# IV.1 Actividad acústica (IAA) y riqueza — figuras para la tesis
# ------------------------------------------------------------------------------

# Boxplot + puntos individuales por sitio y tratamiento. jitterdodge alinea
# los puntos con la caja de su mismo tratamiento en vez de superponerlos.
fig_iaa_sitio <- ggplot(tabla_noche, aes(x = lugar, y = pases_por_hora, fill = tratamiento)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.65,
               alpha = 0.85, outlier.shape = NA, linewidth = 0.4) +
  geom_point(aes(color = tratamiento),
             position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
             size = 1.8, alpha = 0.75, show.legend = FALSE) +
  scale_fill_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  scale_color_manual(values = paleta_tratamiento) +
  labs(
    title    = "Actividad acústica de murciélagos según presencia de panel solar",
    subtitle = "Índice de actividad acústica (IAA): pases de murciélago por hora efectiva de grabación",
    x = NULL, y = "IAA (pases / hora)",
    caption = "Incluye pases identificados y NoID; excluye Noise. Cada punto = una noche de muestreo."
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

fig_iaa_sitio
guardar_figura(fig_iaa_sitio, "fig01_IAA_por_sitio_tratamiento")

# Versión resumen general (todos los sitios juntos): barras de la media
# con intervalo de confianza del 95%. Es la figura típica de "resultado
# principal" para la tesis, complementaria al detalle por sitio de arriba.
fig_iaa_general <- ggplot(resumen_general_iaa, aes(x = tratamiento, y = media, fill = tratamiento)) +
  geom_col(width = 0.55, alpha = 0.9) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.15, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", media)), vjust = -1.6, size = 4.2, fontface = "bold") +
  scale_fill_manual(values = paleta_tratamiento, guide = "none") +
  scale_x_discrete(labels = etiquetas_tratamiento) +
  labs(
    title    = "IAA general, con y sin panel solar",
    subtitle = "Media e intervalo de confianza del 95%, todos los sitios combinados",
    x = NULL, y = "IAA (pases / hora)",
    caption = paste0("n = ", sum(resumen_general_iaa$n), " noches-sitio (", 
                      paste(resumen_general_iaa$tratamiento, resumen_general_iaa$n, sep = ": ", collapse = " | "), ")")
  )

fig_iaa_general
guardar_figura(fig_iaa_general, "fig02_IAA_general_barras", ancho_cm = 12, alto_cm = 12)

# Riqueza: mismo esquema (boxplot + puntos por sitio, y barras generales)
fig_riqueza_sitio <- ggplot(tabla_noche, aes(x = lugar, y = riqueza, fill = tratamiento)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.65,
               alpha = 0.85, outlier.shape = NA, linewidth = 0.4) +
  geom_point(aes(color = tratamiento),
             position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
             size = 1.8, alpha = 0.75, show.legend = FALSE) +
  scale_fill_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  scale_color_manual(values = paleta_tratamiento) +
  scale_y_continuous(breaks = scales::breaks_pretty()) +
  labs(
    title    = "Riqueza de especies según presencia de panel solar",
    subtitle = "Número de especies (grupos acústicos) identificados por noche",
    x = NULL, y = "Riqueza (N° especies / noche)",
    caption = "Excluye NoID, Noise y detecciones de baja confianza."
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

fig_riqueza_sitio
guardar_figura(fig_riqueza_sitio, "fig03_riqueza_por_sitio_tratamiento")

fig_riqueza_general <- ggplot(resumen_general_riqueza, aes(x = tratamiento, y = media, fill = tratamiento)) +
  geom_col(width = 0.55, alpha = 0.9) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.15, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", media)), vjust = -1.6, size = 4.2, fontface = "bold") +
  scale_fill_manual(values = paleta_tratamiento, guide = "none") +
  scale_x_discrete(labels = etiquetas_tratamiento) +
  labs(
    title    = "Riqueza general, con y sin panel solar",
    subtitle = "Media e intervalo de confianza del 95%, todos los sitios combinados",
    x = NULL, y = "Riqueza (N° especies / noche)",
    caption = paste0("n = ", sum(resumen_general_riqueza$n), " noches-sitio")
  )

fig_riqueza_general
guardar_figura(fig_riqueza_general, "fig04_riqueza_general_barras", ancho_cm = 12, alto_cm = 12)

# Serie cronológica por sitio: útil para inspeccionar visualmente el orden
# temporal de las condiciones (ver advertencia de la revisión metodológica:
# en la mayoría de los sitios, sin_panel ocurre antes que con_panel).
fig_serie_temporal <- ggplot(tabla_noche, aes(x = fecha, y = pases_por_hora, color = tratamiento)) +
  geom_line(aes(group = lugar), color = "grey75", linewidth = 0.4) +
  geom_point(size = 2.6) +
  facet_wrap(~ lugar, scales = "free_x", ncol = 3) +
  scale_color_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  labs(
    title    = "IAA en orden cronológico, por sitio",
    subtitle = "Permite ver si el orden de las condiciones (sin/con panel) coincide con una tendencia temporal",
    x = "Fecha", y = "IAA (pases / hora)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.75)))

fig_serie_temporal
guardar_figura(fig_serie_temporal, "fig05_IAA_serie_temporal_por_sitio", ancho_cm = 22, alto_cm = 14)


# ------------------------------------------------------------------------------
# IV.2 Variables ambientales (clima y fase lunar)
# ------------------------------------------------------------------------------

summary(tabla_noche[c("temp_media", "hum_media", "pnm_media", "ff_media", "fase_lunar_pct")])

resumen_ambiental_sitio

fig_iaa_temp <- ggplot(tabla_noche, aes(x = temp_media, y = pases_por_hora)) +
  geom_point(aes(color = tratamiento), size = 2.4, alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey70", alpha = 0.25, linewidth = 0.6) +
  scale_color_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  labs(title = "IAA vs. temperatura media nocturna",
       x = "Temperatura media (°C)", y = "IAA (pases / hora)")

fig_iaa_temp
guardar_figura(fig_iaa_temp, "fig06_IAA_vs_temperatura")

fig_iaa_viento <- ggplot(tabla_noche, aes(x = ff_media, y = pases_por_hora)) +
  geom_point(aes(color = tratamiento), size = 2.4, alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey70", alpha = 0.25, linewidth = 0.6) +
  scale_color_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  labs(title = "IAA vs. velocidad media del viento",
       x = "Viento medio (km/h)", y = "IAA (pases / hora)")

fig_iaa_viento
guardar_figura(fig_iaa_viento, "fig07_IAA_vs_viento")

fig_iaa_lunar <- ggplot(tabla_noche, aes(x = fase_lunar_pct, y = pases_por_hora)) +
  geom_point(aes(color = tratamiento), size = 2.4, alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey70", alpha = 0.25, linewidth = 0.6) +
  scale_color_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  labs(title = "IAA vs. iluminación lunar",
       x = "Iluminación lunar (%)", y = "IAA (pases / hora)")

fig_iaa_lunar
guardar_figura(fig_iaa_lunar, "fig08_IAA_vs_fase_lunar")

fig_iaa_precip <- ggplot(tabla_noche, aes(x = precip_media, y = pases_por_hora)) +
  geom_point(aes(color = tratamiento), size = 2.4, alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey70", alpha = 0.25, linewidth = 0.6) +
  scale_color_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  labs(title = "IAA vs. precipitación",
       x = "Precipitación (mm)", y = "IAA (pases / hora)")

fig_iaa_precip
guardar_figura(fig_iaa_precip, "fig20_IAA_vs_precipitacion")

# NDVI (predicción 3: la vegetación circundante atenúa el efecto del panel).
fig_ndvi_sitio <- ggplot(ndvi_lugar, aes(x = fct_reorder(lugar, NDVI_medio), y = NDVI_medio)) +
  geom_col(fill = "#4C9A6A", width = 0.6, alpha = 0.9) +
  coord_flip() +
  labs(title = "Cobertura vegetal (NDVI) por sitio",
       subtitle = "NDVI medio del entorno de muestreo",
       x = NULL, y = "NDVI")

fig_ndvi_sitio
guardar_figura(fig_ndvi_sitio, "fig09_NDVI_por_sitio", ancho_cm = 16, alto_cm = 10)

fig_iaa_ndvi <- ggplot(tabla_noche, aes(x = NDVI_medio, y = pases_por_hora, color = tratamiento, fill = tratamiento)) +
  geom_point(size = 2.6, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.7) +
  scale_color_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  scale_fill_manual(values = paleta_tratamiento, guide = "none") +
  labs(
    title    = "IAA vs. NDVI del sitio, según tratamiento",
    subtitle = "Evalúa si la vegetación circundante modula el efecto del panel (predicción 3)",
    x = "NDVI medio del sitio", y = "IAA (pases / hora)"
  )

fig_iaa_ndvi
guardar_figura(fig_iaa_ndvi, "fig10_IAA_vs_NDVI_por_tratamiento")


# ------------------------------------------------------------------------------
# IV.3 Abundancia total de pases por especie y por familia
# ------------------------------------------------------------------------------

# Abundancia por especie: total de pases reportados (post-filtro por MATCH
# RATIO), desglosado por tratamiento. Se usa `especie_valida` (no
# especie_auto_id) para que solo entren identificaciones que pasaron el
# umbral de calidad.
abundancia_especie <- datos %>%
  filter(!especie_valida %in% c("Noise", "NoID", "Baja_confianza", "Fuera_de_rango")) %>%
  count(especie_valida, tratamiento, name = "n_pases") %>%
  rename(especie = especie_valida)

fig_abundancia_especie <- ggplot(
  abundancia_especie,
  aes(x = fct_reorder(especie, n_pases, .fun = sum, .desc = FALSE), y = n_pases, fill = tratamiento)
) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  labs(
    title    = "Abundancia total de pases por especie",
    subtitle = "Total de pases identificados (post-filtro MATCH RATIO), por tratamiento",
    x = NULL, y = "N° de pases"
  )

fig_abundancia_especie
guardar_figura(fig_abundancia_especie, "fig18_abundancia_por_especie", ancho_cm = 16, alto_cm = 12)

# Abundancia por familia: mismo criterio, agrupado a nivel de familia
# taxonómica (columna `familia`, ya presente en la base cruda).
abundancia_familia <- datos %>%
  filter(!especie_valida %in% c("Noise", "NoID", "Baja_confianza", "Fuera_de_rango")) %>%
  count(familia, tratamiento, name = "n_pases")

fig_abundancia_familia <- ggplot(
  abundancia_familia,
  aes(x = fct_reorder(familia, n_pases, .fun = sum, .desc = FALSE), y = n_pases, fill = tratamiento)
) +
  geom_col(alpha = 0.9, width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = paleta_tratamiento, labels = etiquetas_tratamiento, name = "Tratamiento") +
  labs(
    title    = "Abundancia total de pases por familia",
    subtitle = "Total de pases identificados (post-filtro MATCH RATIO), por tratamiento",
    x = NULL, y = "N° de pases"
  )

fig_abundancia_familia
guardar_figura(fig_abundancia_familia, "fig19_abundancia_por_familia", ancho_cm = 14, alto_cm = 8)


################################################################################
# PARTE V — EXPORTACIÓN DE DATOS PROCESADOS
################################################################################

# carpeta_salida y carpeta_figuras ya fueron definidas y creadas en I.1.b

write.csv(datos,                           file.path(carpeta_salida, "01_datos_limpios_detalle.csv"),          row.names = FALSE)
write.csv(tabla_noche,                     file.path(carpeta_salida, "01_tabla_noche.csv"),                     row.names = FALSE)
write.csv(pases_por_ventana,               file.path(carpeta_salida, "01_pases_por_ventana.csv"),             row.names = FALSE)
write.csv(tabla_binomial,                  file.path(carpeta_salida, "01_tabla_binomial.csv"),                row.names = FALSE)
write.csv(tabla_especie_noche,             file.path(carpeta_salida, "01_tabla_especie_noche.csv"),            row.names = FALSE)
write.csv(total_pases_especie,             file.path(carpeta_salida, "01_total_pases_por_especie.csv"),        row.names = FALSE)
write.csv(total_pases_especie_tratamiento, file.path(carpeta_salida, "01_total_pases_especie_tratamiento.csv"), row.names = FALSE)
write.csv(comparacion_filtro_especie,      file.path(carpeta_salida, "01_comparacion_filtro_match_ratio.csv"),  row.names = FALSE)
write.csv(diseno_realizado,                file.path(carpeta_salida, "01_diseno_realizado.csv"),               row.names = FALSE)
write.csv(resumen_general_iaa,             file.path(carpeta_salida, "01_resumen_general_IAA.csv"),            row.names = FALSE)
write.csv(resumen_general_riqueza,         file.path(carpeta_salida, "01_resumen_general_riqueza.csv"),        row.names = FALSE)
write.csv(resumen_sitio_iaa,               file.path(carpeta_salida, "01_resumen_por_sitio_IAA.csv"),          row.names = FALSE)
write.csv(resumen_sitio_riqueza,           file.path(carpeta_salida, "01_resumen_por_sitio_riqueza.csv"),      row.names = FALSE)
write.csv(diferencias_pareadas_iaa,        file.path(carpeta_salida, "01_diferencias_pareadas_IAA.csv"),       row.names = FALSE)
write.csv(diferencias_pareadas_riqueza,    file.path(carpeta_salida, "01_diferencias_pareadas_riqueza.csv"),   row.names = FALSE)
write.csv(resumen_ambiental_sitio,         file.path(carpeta_salida, "01_resumen_ambiental_por_sitio.csv"),    row.names = FALSE)
write.csv(abundancia_especie,              file.path(carpeta_salida, "01_abundancia_por_especie.csv"),         row.names = FALSE)
write.csv(abundancia_familia,              file.path(carpeta_salida, "01_abundancia_por_familia.csv"),         row.names = FALSE)


cat("========================================\n")
cat("RESUMEN DEL ANÁLISIS EXPLORATORIO\n")
cat("========================================\n")
cat("Filas totales en la base cruda:           ", nrow(datos), "\n")
cat("Umbral de MATCH RATIO efectivamente usado:", UMBRAL_MATCH_RATIO, "\n")
cat("Archivos válidos (especie, post-filtro):  ",
    sum(!datos$especie_valida %in% c("NoID", "Noise", "Baja_confianza", "Fuera_de_rango")), "\n")
cat("Archivos de baja confianza (excluidos):   ", sum(datos$especie_valida == "Baja_confianza"), "\n")
cat("Archivos Fuera_de_rango (cuentan como pase, no como especie/gremio):", sum(datos$especie_valida == "Fuera_de_rango"), "\n")
cat("Archivos NoID (cuentan como pase, no como especie):", sum(datos$especie_valida == "NoID"), "\n")
cat("Archivos Noise (excluidos de todo):       ", sum(datos$especie_valida == "Noise"), "\n")
cat("Noches-lugar en tabla_noche:              ", nrow(tabla_noche), "\n")
cat("Filas con n_pulsos < 2 (revisar):         ", nrow(chequeo_pulsos_bajos), "\n")
cat("Combinaciones lugar-fecha con pases de madrugada (ya reasignados):", nrow(chequeo_madrugada), "\n")
cat("Pases totales reasignados a la noche anterior:            ", n_reasignados, "\n")
cat("Tablas exportadas a: ", carpeta_salida, "\n")
cat("Figuras (PNG, 300 dpi) exportadas a:", carpeta_figuras, "\n")
