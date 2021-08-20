if (!require("fts")) install.packages("fts")
if (!require("scales")) install.packages("scales")

library(tidyverse)
library(fts)
library(scales)
library(janitor)

### curl::curl_download("https://cloud.minsa.gob.pe/s/Jwck8Z59snYAK8S/download","TB_POBLACION_INEI.csv", quiet = FALSE)
### de aquí se bajo la data de población

poblacion_inei <- read.csv("data/TB_POBLACION_INEI.csv") %>%
  clean_names() %>%
  mutate(
    departamento = str_to_title(departamento),
    provincia = str_to_title(provincia),
    distrito = str_to_title(distrito))

suma_poblacion <- function(DataFrame) {
  DataFrame <- DataFrame %>% 
    group_by(departamento, provincia, distrito) %>%
    summarise(n = sum(cantidad))
  }

suma_poblacion(poblacion_inei)


### Usando la data de positivos

dummy <- positivos
poblacion <- sample(5000:300000, 1872)

dummy <- dummy_pobl %>%
  select(c("departamento", "provincia", "distrito"))

dummy <- unique(dummy)

### dummy población

dummy_pobl <- dummy
dummy_pobl$poblacion <- poblacion

dummy_pobl

### dummy Otras variables

imc <- round(runif(1872, min=16, max=40),1)
pobreza <- round(runif(1872, min=0.1, max=81.3),1)

dummy_vars <- dummy
dummy_vars$imc <- imc
dummy_vars$pobreza <- pobreza

dummy_vars

write_fst(dummy_vars, "data/dummy_vars.fst")
write_fst(dummy_pobl, "data/dummy_poblacion.fst")


#### DUMMYs

##### IDEAS AUN no trabajadas

## transformando las variables de vacunacion y casos positivos

indicadores_normalizados <- data.frame(scale(tabla_indicador[,c(4:9)],center=T,scale=T))
nombres <- tabla_indicador %>% select(1:3)

tabla_indicador_escalada <- cbind(nombres, indicadores_normalizados) %>%
  mutate(raw_vulnerabilidad = (n_positivos - ratio_vacunados + imc + pobreza)) %>%
  mutate(index_vulnerabilidad = (n_positivos - ratio_vacunados + imc + pobreza))

### Dividirlo por tres cuartiles ###

