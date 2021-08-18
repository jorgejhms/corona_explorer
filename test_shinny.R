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

tabla_dosis <- vacunacion %>% 
  group_by(dosis) %>% 
  summarise(n = sum(n))

# Gráficos ----------------------------------------------------------------

# Gráfica de positivos diarios
positivos %>% 
  group_by(fecha_resultado) %>% 
  summarise(n = sum(n)) %>% 
  plot.diarios(fecha_resultado, n)

# Gráfica de fallecidos diarios
fallecidos %>% 
  group_by(fecha_fallecimiento) %>% 
  summarise(n = sum(n)) %>% 
  plot.diarios(fecha_fallecimiento, n)


# Grafica de vacunaciones acumuladas
vacunacion %>%
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
  filter(str_detect(name, "cum")) %>%
  ggplot(aes(fecha_vacunacion, value, color = name)) +
  theme_minimal() +
  geom_line()

# Primera dosis
vacunacion %>% 
  select(fecha_vacunacion, dosis, n) %>% 
  ggplot(aes(fecha_vacunacion, n, group = dosis, color = dosis)) +

