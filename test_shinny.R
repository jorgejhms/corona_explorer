# Prueba de gráficas y calculos

# Aqui se probarán los gráficos y tablas para la app de shiny antes de enviarlas
# a la aplicación.

# Setup -------------------------------------------------------------------

# setea el sistema en español
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8") 

# Librerias
library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)

# aumento memoría
memory.limit(size = 250000) # Solo windows
memory.size() # Solo Windows

# Funciones
source("funciones.R")

# Carga de Data
positivos <- fst::read_fst("data/positivos.fst")
fallecidos <- fst::read_fst("data/fallecidos.fst")
vacunacion <- fst::read_fst("data/vacunacion.fst")


# Valores -----------------------------------------------------------------

fecha_positivos <- last(sort(positivos$fecha_resultado))
fecha_fallecidos <- last(sort(fallecidos$fecha_fallecimiento))
fecha_vacunacion <- last(sort(vacunacion$fecha_vacunacion))

total_positivos <- sum(positivos$n)
total_fallecidos <- sum(fallecidos$n)
total_vacunas <- sum(vacunacion$n)


# Generación de tablas ----------------------------------------------------

# Numero de positivos por día
tabla_positivos <- positivos %>% 
  group_by(fecha_resultado) %>% 
  summarise(n = sum(n))

# Número de fallecidos por día
tabla_fallecidos <- fallecidos %>% 
  group_by(fecha_fallecimiento) %>% 
  summarise(n = sum(n))

# Total de vacunaciones según dosis
tabla_dosis <- vacunacion %>% 
  group_by(dosis) %>% 
  summarise(n = sum(n)) %>% 
  adorn_totals()

# Tabla de vacunaciones por día, dosis y acumuladas
tabla_vacunaciones <- vacunacion %>%
  pivot_wider(
    id_cols = fecha_vacunacion,
    values_from = n,
    names_from = dosis,
    names_prefix = "dosis_",
    values_fn = sum,
    values_fill = 0
  ) %>%
  arrange(fecha_vacunacion) %>%
  mutate(total = dosis_1 + dosis_2) %>%
  mutate(
    dosis_1_cum = cumsum(dosis_1),
    dosis_2_cum = cumsum(dosis_2),
    total_cum = cumsum(total)
  ) %>%
  pivot_longer(cols = -fecha_vacunacion) %>%
  filter(str_detect(name, "cum"))

# Gráficos ----------------------------------------------------------------

# Gráfica de positivos diarios
plot.diarios(tabla_positivos, fecha_resultado, n) +
  labs(title = "Casos diarios detectados")

# Gráfica de fallecidos diarios
plot.diarios(tabla_fallecidos, fecha_fallecimiento, n) +
  labs(title = "Fallecimientos diarios")

# Grafica de vacunaciones acumuladas
plot.vacunaciones(tabla_vacunaciones, fecha_vacunacion, value, name) +
  labs(title = "Progreso de las vacunaciones") +
  scale_color_discrete(labels = c("Primera dosis", "Segunda dosis", "Total"))
