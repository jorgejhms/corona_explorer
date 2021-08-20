# Descarga y limpieza de datos

# Ejecución de este script tomo 5min aprox

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(fst)

# Importación datos ------------------------------------------------------------

### curl::curl_download("https://cloud.minsa.gob.pe/s/Jwck8Z59snYAK8S/download","TB_POBLACION_INEI.csv", quiet = FALSE)
### de aquí se bajo la data de población

# Carga de Data
positivos <-
  data.table::fread("data/positivos_covid.csv", sep = ";")
fallecidos <-
  data.table::fread("data/fallecidos_covid.csv", sep = ";")
vacunacion <- data.table::fread("data/vacunas_covid.csv", sep = ",")
poblacion_inei <- data.table::fread("data/TB_POBLACION_INEI.csv") 

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
    departamento = recode(
      departamento,
      "LIMA REGION" = "LIMA",
      "LIMA METROPOLITANA" = "LIMA"
    ),
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

poblacion_inei <- poblacion_inei %>%
  clean_names() %>%
  mutate(
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito))

# Transformación por distrito ---------------------------------------------

positivos <- positivos %>%
  count(departamento,
        provincia,
        distrito,
        fecha_resultado,
        metododx,
        sexo)

fallecidos <- fallecidos %>%
  count(departamento, provincia, distrito, fecha_fallecimiento, sexo)

vacunacion <- vacunacion %>%
  count(
    departamento,
    provincia,
    distrito,
    sexo,
    fecha_vacunacion,
    fabricante,
    dosis,
    grupo_riesgo
  )

poblacion_inei <- suma_poblacion(poblacion_inei, cantidad)

# Guardado data -----------------------------------------------------------

write_fst(positivos, "data/positivos.fst")
write_fst(fallecidos, "data/fallecidos.fst")
write_fst(vacunacion, "data/vacunacion.fst")
write_fst(poblacion_inei, "data/poblacion_inei.fst")
