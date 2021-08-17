# Descarga y limpieza de datos

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(curl)

# Importación datos ------------------------------------------------------------

vacunacion <-
  data.table::fread("https://cloud.minsa.gob.pe/s/ZgXoXqK2KLjRLxD/download")

#Descarga y descomprime data vacunacion
temp <- tempfile()
curl_download("https://cloud.minsa.gob.pe/s/To2QtqoNjKqobfw/download",temp)
vacunacion <- data.table::fread(unz(temp, "vacunas_covid.7z"))
unlink(temp)

positivos <-
  data.table::fread("https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download")

fallecidos <-
  data.table::fread("https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download")

# Creación db general
db <- list(positivos,
           fallecidos,
           vacunacion) %>%
  set_names("positivos",
            "fallecidos",
            "vacunacion")

rm(positivos, fallecidos, vacunacion)


# LimpiezVja data ----------------------------------------------------------------

db <- db %>% 
  modify(clean_names) %>%  #Limpia nombres de columnas
  modify(as.character)

db <- db %>%  
  modify_in(c("positivos", "fecha_corte"), as.Date, format = "%Y%m%d", as.character) 

%>% 
  modify_in(c("positivos", "fecha_resultado"), as.Date.numeric, format = "%Y%m%d") 

%>% 
  modify_in(c("fallecidos", "fecha_corte"), as.Date.numeric, format = "%Y%m%d") %>% 
  modify_in(c("fallecidos", "fecha_fallecimiento"), as.Date.numeric, format = "%Y%m%d") %>% 
  modify_in(c("vacunacion", "fecha_corte"), as.Date.numeric, format = "%Y%m%d") %>% 
  modify_in(c("vacunacion", "fecha_vacunacion"), as.Date.numeric, format = "%Y%m%d") 

  modify_at(vars(contains("fecha")), as.character) %>% 
  modify_at(vars(contains("fecha")), as.Date, format = "%Y%m%d" )

db %>% 
  modify_in("vacunacion", mutate_at, c("fecha_corte", "fecha_vacunacion"), as.character) %>% 
  modify_at("vacunacion", mutate_at, c("fecha_corte", "fecha_vacunacion"), as.Date, format = "%Y%m%d" ) %>% 
  modify_at("vacunaci")

vacunacion <- vacunacion %>%
  clean_names() %>%
  mutate_at(c("fecha_corte", "fecha_vacunacion"), as.character) %>%
  mutate_at(c("fecha_corte", "fecha_vacunacion"),
            as.Date, format = "%Y%m%d") %>%
  mutate_at(
    c(
      "departamento",
      "provincia",
      "distrito",
      "sexo",
      "diresa",
      "fabricante"
    ),
    str_to_title
  ) %>%
  mutate(grupo_riesgo = grupo_riesgo %>% str_to_sentence) %>%
  mutate_at(
    c(
      "grupo_riesgo",
      "sexo",
      "fabricante",
      "diresa",
      "departamento",
      "provincia",
      "distrito"
    ),
    as.factor
  )

positivos <- positivos %>%
  clean_names() %>%
  mutate_at(c("fecha_corte", "fecha_resultado"), as.character) %>%
  mutate_at(c("fecha_corte", "fecha_resultado"),
            as.Date, format = "%Y%m%d") %>%
  mutate_at(c("departamento", "provincia", "distrito", "sexo"), str_to_title) %>%
  mutate_at(c("departamento", "provincia", "distrito", "metododx", "sexo"),
            as.factor)

fallecidos.df <- fallecidos.df %>%
  clean_names() %>% 
  mutate_at(c("'fecha_corte", "fecha_fallecimiento"), as.character) %>% 
  mutate_at(c("fecha_corte", "fecha_fallecimiento"),
            as.Date, format = "%Y%m%d")

sinadef <- sinadef %>% clean_names()

# Guardando archivos------------------------------------------------------------
saveRDS(db, file ="data/db.Rds")
