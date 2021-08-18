# Descarga y limpieza de datos

# Ejecución de este script tomo 5min aprox

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(fst)

# Importación datos ------------------------------------------------------------

# Carga de Data
positivos <- data.table::fread("data/positivos_covid.csv", sep = ";")
fallecidos <- data.table::fread("data/fallecidos_covid.csv", sep = ";")
vacunacion <- data.table::fread("data/vacunas_covid.csv", sep = ",")

# Funciones
source("funciones.R")

# Limpieza data ----------------------------------------------------------------

positivos <- positivos %>%
  clean_names() %>% 
  mutate(
    fecha_corte = fechaFix(fecha_corte),
    uuid = as.character(uuid),
    departamento = recode(departamento, "LIMA REGION" = "LIMA"),
    #Junta Lima y Lima pronvincias
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito),
    sexo = str_to_sentence(sexo),
    fecha_resultado = fechaFix(fecha_resultado),
  ) %>%
  filter(!is.na(fecha_resultado)) # Elimina datos sin registro de fecha

fallecidos <- fallecidos %>%
  clean_names() %>% 
  mutate(
    fecha_corte = fechaFix(fecha_corte),
    uuid = as.character(uuid),
    fecha_fallecimiento = fechaFix(fecha_fallecimiento),
    edad_declarada = as.numeric(edad_declarada),
    sexo = str_to_sentence(sexo),
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito)
  ) %>%
  filter(!is.na(fecha_fallecimiento))

vacunacion <- vacunacion %>%
  clean_names() %>% 
  mutate(
    fecha_corte = fechaFix(fecha_corte),
    uuid = as.character(uuid),
    grupo_riesgo = str_to_sentence(grupo_riesgo),
    sexo = str_to_title(sexo),
    fecha_vacunacion = fechaFix(fecha_vacunacion),
    fabricante = str_to_title(fabricante),
    diresa = str_to_title(diresa),
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito)
  ) %>%
  filter(!is.na(fecha_vacunacion))

# Transformación por distrito ---------------------------------------------

positivos <- positivos %>% 
  count(departamento, provincia, distrito, fecha_resultado, metododx, sexo)

fallecidos <- fallecidos %>% 
  count(departamento, provincia, distrito, fecha_fallecimiento, sexo)

vacunacion <- vacunacion %>% 
  count(departamento, provincia, distrito, sexo, fecha_vacunacion, fabricante, dosis, grupo_riesgo)

# Guardado data -----------------------------------------------------------

write_fst(positivos, "data/positivos.fst")
write_fst(fallecidos, "data/fallecidos.fst")
write_fst(vacunacion, "data/vacunacion.fst")
