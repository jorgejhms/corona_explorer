# Descarga y limpieza de datos

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(feather)

# Importación datos ------------------------------------------------------------

# Carga de Data
positivos <- data.table::fread("data/positivos_covid.csv", sep = ";")
fallecidos <- data.table::fread("data/fallecidos_covid.csv", sep = ";")
vacunacion <- data.table::fread("data/vacunas_covid.csv", sep = ",")

db <- list(positivos, fallecidos, vacunacion)
names(db) <- c("positivos", "fallecidos", "vacunacion")
rm(positivos, fallecidos, vacunacion)
gc()

# Limpieza data ----------------------------------------------------------------

db <- modify(db, mutate_if, is.character, as.factor) # Convierte a factores
gc()

db <- modify(db, clean_names) # Limpia nombres de variables

db$positivos <- db$positivos %>%
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

db$fallecidos <- db$fallecidos %>%
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

db$vacunacion <- db$vacunacion %>%
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


# Guardado data -----------------------------------------------------------

write_feather(db$positivos, "data/positivos.feather")
write_feather(db$fallecidos, "data/fallecidos.feather")
write_feather(db$vacunacion, "data/vacunacion.feather")
