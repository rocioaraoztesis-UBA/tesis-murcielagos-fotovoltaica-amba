################################################################################
# TEMA VISUAL COMPARTIDO — Script 1 y Script 2
#
# Se centraliza acá el tema de ggplot2, la paleta de colores de tratamiento
# y la función para guardar figuras, para que ambos scripts del análisis
# (01_analisis_exploratorio.R y 02_analisis_estadistico.R) produzcan
# figuras con exactamente el mismo estilo, sin mantener dos copias del
# mismo código. Se carga con:
#
#   source("00_tema_visual.R")
#
# desde cada script, ANTES de generar cualquier gráfico.
################################################################################

library(ggplot2)
library(scales)

# Tema unificado para todos los gráficos, con un estilo formal orientado a
# publicación académica: fondo con caja (panel.border), tipografía serif,
# títulos sin negrita (siguiendo la convención de journals de ecología,
# donde el título vive en el pie de figura del texto, no destacado dentro
# de la imagen), y grilla mínima solo en los ejes principales.
theme_tesis <- function(base_size = 13, base_family = "serif") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title       = element_text(face = "plain", size = rel(1.05), hjust = 0,
                                       margin = margin(b = 4)),
      plot.subtitle    = element_text(face = "italic", color = "grey25", size = rel(0.85),
                                       margin = margin(b = 10)),
      plot.caption     = element_text(color = "grey40", size = rel(0.65), hjust = 0,
                                       margin = margin(t = 8)),
      axis.title       = element_text(size = rel(0.95), color = "black"),
      axis.text        = element_text(color = "black", size = rel(0.85)),
      panel.border     = element_rect(fill = NA, color = "grey30", linewidth = 0.5),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      legend.position    = "top",
      legend.title       = element_text(face = "plain", size = rel(0.85)),
      legend.text        = element_text(size = rel(0.85)),
      legend.background  = element_rect(fill = "white", color = NA),
      strip.text         = element_text(face = "plain", size = rel(0.9), color = "black"),
      strip.background   = element_rect(fill = "grey92", color = "grey30", linewidth = 0.3)
    )
}
theme_set(theme_tesis())

# Paleta fija para tratamiento en TODOS los gráficos de la tesis
# (colorblind-friendly: azul para sin_panel, naranja para con_panel).
paleta_tratamiento    <- c("sin_panel" = "#2C7FB8", "con_panel" = "#D95F02")
etiquetas_tratamiento <- c("sin_panel" = "Sin panel", "con_panel" = "Con panel")

# Intervalo de confianza del 95% (basado en la distribución t de Student,
# apropiado para los n chicos de esta tesis) para un vector numérico.
# Devuelve un vector con media, límite inferior y límite superior. Se usa
# en los gráficos de barras generales en vez de graficar solo el error
# estándar, a pedido del comité de tesis.
ic95 <- function(x) {
  x  <- x[!is.na(x)]
  n  <- length(x)
  m  <- mean(x)
  ee <- sd(x) / sqrt(n)
  tc <- qt(0.975, df = n - 1)
  c(media = m, ic_inf = m - tc * ee, ic_sup = m + tc * ee)
}

# Guarda un ggplot ya armado como PNG a 300 dpi, con un tamaño pensado para
# ocupar media página o una página completa de una tesis en A4.
# `carpeta` tiene un valor por defecto (`carpeta_figuras`) que cada script
# debe definir antes de llamar a esta función.
guardar_figura <- function(plot, nombre, carpeta = carpeta_figuras, ancho_cm = 18, alto_cm = 12) {
  ggsave(
    filename = file.path(carpeta, paste0(nombre, ".png")),
    plot     = plot, width = ancho_cm, height = alto_cm, units = "cm", dpi = 300
  )
}
