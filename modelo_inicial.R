library(tidyverse)
library(fts)
library(scales)
library(janitor)
library(openxlsx)
library(e1071)
library(dgof)
library(nortest)
library(lmtest)
library(psych)

install.packages("psych")

### Cargamos los datos ###

vif()

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
pobreza$distrito <- gsub("ñ","ã‘", pobreza$distrito)

View(pobreza)

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
  mutate(DEPARTAMENTO = recode(
    DEPARTAMENTO,
    "LIMA REGION" = "LIMA",
    "LIMA METROPOLITANA" = "LIMA"
  )) %>%
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

tabla_modelo <- left_join(pobreza, positivos_filtro) %>%
  left_join(vacunados_filtro) %>%
  left_join(fallecidos_filtro) %>%
  left_join(poblacion_filtrada)


modelo = lm(fallecidos~ vacunados + pobreza + positivos, tabla_modelo)
summary(modelo)
            
tabla_modelo2 <- tabla_modelo %>%
  mutate(ratio_fallecidos = (fallecidos/poblacion),
         ratio_vacunados = (vacunados/poblacion)) 

modelo2 = lm(fallecidos~ ratio_vacunados + pobreza + positivos, tabla_modelo2)
summary(modelo2)

tabla_modelo_lineal <- tabla_modelo2 %>%
  mutate(raw_index = 0.72 + 0.41*(positivos) + -0.19*(ratio_vacunados) + -0.17*(pobreza)) %>%
  mutate(index = (raw_index-me)/(dev))

me = mean(tabla_modelo_lineal$raw_index, na.rm = T)
dev = sd(tabla_modelo_lineal$raw_index, na.rm = T)

summary(tabla_modelo_lineal$index)

### PRUEBA SOBRE SUPUESTOS ####

dwtest(modelo2) ##durbin waton - autocorrelaci?n, lo que te importa es el D-W Statistic
plot(modelo2, 1)

### Modelo anterior

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


