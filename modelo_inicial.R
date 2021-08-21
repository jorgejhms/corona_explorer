library(tidyverse)
library(fts)
library(scales)
library(janitor)
library(openxlsx)



### Cargamos los datos ###

positivos <-
  data.table::fread("data/positivos_covid.csv", sep = ";")
fallecidos <-
  data.table::fread("data/fallecidos_covid.csv", sep = ";")
vacunacion <- data.table::fread("data/vacunas_covid.csv", sep = ",")
poblacion_inei <- data.table::fread("data/TB_POBLACION_INEI.csv") 


poblacion_edad <- poblacion_inei %>% 
  mutate(edad_simple = parse_number(Edad_Anio)) %>%
  filter(edad_simple >= 60) %>%
  clean_names() %>%
  mutate(
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito)) ## filtracion personas más de 60 años

poblacion_filtrada <- poblacion_edad %>%
  group_by(departamento, provincia, distrito) %>%
  summarise(poblacion = sum(cantidad)) %>%
  ungroup() ## base filtrada

poblacion_edad$edad_simple

pobreza <- read.csv("data/pobreza.csv", sep = ";") %>%
  clean_names() %>%
  mutate(
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito))

pobreza <- modify_if(pobreza, is.character, str_trim) 

min(vacunacion$FECHA_VACUNACION)
max(vacunacion$FECHA_VACUNACION)
max(fallecidos$FECHA_FALLECIMIENTO)
positivos$FECHA_RESULTADO

positivos_filtro <- positivos %>%
  filter(FECHA_RESULTADO >= min(vacunacion$FECHA_VACUNACION) & FECHA_RESULTADO <= max(fallecidos$FECHA_FALLECIMIENTO)) %>%
  filter(EDAD >= 60) %>%
  clean_names() %>%
  mutate(
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito)) %>%
  group_by(departamento, provincia, distrito) %>%
  summarise(positivos = n())%>%
  ungroup()

fallecidos_filtro <- fallecidos %>%
  filter(FECHA_FALLECIMIENTO >= min(vacunacion$FECHA_VACUNACION) & FECHA_FALLECIMIENTO <= max(fallecidos$FECHA_FALLECIMIENTO)) %>%
  filter(EDAD_DECLARADA >= 60) %>%
  clean_names() %>%
  mutate(
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito)) %>%
  group_by(departamento, provincia, distrito) %>%
  summarise(fallecidos = n()) %>%
  ungroup()

vacunados_filtro <- vacunacion %>%
  filter(FECHA_VACUNACION >= min(vacunacion$FECHA_VACUNACION) & FECHA_VACUNACION <= max(fallecidos$FECHA_FALLECIMIENTO)) %>%
  filter(EDAD >= 60) %>%
  clean_names() %>%
  mutate(
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito)) %>%
  group_by(departamento, provincia, distrito) %>%
  summarise(vacunados = n()) %>%
  ungroup()

tabla_modelo <- left_join(poblacion_filtrada, pobreza) %>%
  left_join(positivos_filtro) %>%
  left_join(vacunados_filtro) %>%
  left_join(fallecidos_filtro) %>%
  drop_na()

modelo = lm(fallecidos~ positivos + vacunados + pobreza, tabla_modelo)

summary(modelo)

tabla_final <- tabla_modelo %>%
  mutate(raw_index = 1.86 + 0.27*(positivos) + 0.003*(vacunados) + -0.06*(pobreza)) 

me = mean(tabla_final$raw_index)
dev = sd(tabla_final$raw_index)

tabla_final <- tabla_final %>%
  mutate(index = (raw_index-me)/(dev)) 

tabla_final <- tabla_final %>%
  mutate(index_t = index*10+50) 

summary(tabla_final$index)
summary(scale(tabla_final$raw_index))

summary(scale(tabla_final$raw_index)*10+50)

tabla <- baremar(tabla_final$raw_index)
