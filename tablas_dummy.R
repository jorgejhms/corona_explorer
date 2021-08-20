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


### Usando la data de positivos (dummy de pobreza) ###

dummy <- positivos
dummy <- positivos %>% select(departamento, provincia, distrito)
dummy <- distinct(dummy, distrito, .keep_all=TRUE)

pobreza <- round(runif(1697, min=0.1, max=81.3),1)

dummy_pobreza <- dummy
dummy_pobreza$pobreza <- pobreza


##### FORMULA DE INDICADOR #####

## transformando las variables de vacunacion y casos positivos

calculo_indicador <- function(Positivos, Vacunacion, Poblacion, Pobreza) {
  a = 3.4 ## dummy casos positivos
  b = 2.4 ## dummy pobreza monetaria
  c = -1.4 ## dummy vacunación
  st.d = -1000 ## dummy
  me = 30000 ## dummy
    
  Positivos <- Positivos %>% group_by(departamento, provincia, distrito) %>%
    summarise(positivos = sum(n))
  Vacunacion <- Vacunacion %>% group_by(departamento, provincia, distrito) %>%
    summarise(vacunacion = sum(n))
  
  tabla <- left_join(Positivos, Vacunacion)
  tabla <- left_join(tabla, Pobreza)
  tabla <- left_join(tabla, Poblacion)
  tabla <- tabla %>%
    mutate(raw_vulnerabilidad = (positivos*a + ((vacunacion/(poblacion/10000))*c) + pobreza*b)) %>%
    mutate(indice_vulnerabilidad = 50+10*((raw_vulnerabilidad-me)/(st.d^2)))
  
  mean(tabla$indice_vulnerabilidad, na.rm = T)
}


