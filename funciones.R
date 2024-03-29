# Funciones

library(tidyverse)

descargar.datos <- function() {
  # Descarga las últimas bases de datos abiertos
  message("Descargando bases de datos...")
  curl::curl_download(
    "https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download",
    "data/positivos_covid.csv", quiet = FALSE
  )
  curl::curl_download(
    "https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download",
    "data/fallecidos_covid.csv", quiet = FALSE
  )
  curl::curl_download(
    "https://cloud.minsa.gob.pe/s/To2QtqoNjKqobfw/download",
    "data/vacunas_covid.7z", quiet = FALSE
  )
  message("Bases de datos descargadas")
  message("Descomprimiendo base de datos de vacunación...")
  archive::archive_extract("data/vacunas_covid.7z", dir = "data")
  message("Base de datos descomprimida.")
}

fechaFix <- function(variable) {
  # Convierte clase de variable de números a fecha
  variable %>%
    as.character() %>%
    as.Date(format = "%Y%m%d")
}

plot.diarios <- function(DataFrame, x, y) {
  # Gráfico de casos diarios
  ggplot(DataFrame, aes({{x}} , {{y}})) +
    geom_col() +
    theme_minimal() +
    geom_line(aes(y = zoo::rollmean({{y}}, 7, fill = NA)),
              size = 1.2,
              colour = "red1") +
    scale_x_date(
      labels = scales::label_date_short(),
      breaks = scales::breaks_width("1 months")
    ) +
    scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
    theme(legend.position = "none",
          legend.title = element_blank()) +
    labs(x = element_blank(),
         y = element_blank(),
         title = element_blank())
}


plot.vacunaciones <- function(DataFrame, x, y, group) {
  # Grafico de vacunaciones por tipo
  ggplot(DataFrame, aes({{x}}, {{y}}, color = {{group}})) +
    theme_minimal() +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
    scale_x_date(
      labels = scales::label_date_short(),
      breaks = scales::breaks_width("1 months"))+
    theme(legend.position = "right",
          legend.title = element_blank()) +
    labs(x = element_blank(),
         y = element_blank(),
         title = element_blank())
}

filtra_zona <- function(DataFrame, i_departamento, i_provincia, i_distrito) {
  # Filtra la base de datos otorgada según los datos ingresados en el input
  if (i_departamento != "Todos" &
      i_provincia != "Todos" &
      i_distrito != "Todos") {
    DataFrame <- DataFrame %>%
      filter(departamento == i_departamento) %>%
      filter(provincia == i_provincia) %>%
      filter(distrito == i_distrito)
    
  } else if (i_departamento != "Todos" &
             i_provincia != "Todos") {
    DataFrame <- DataFrame %>%
      filter(departamento == i_departamento) %>%
      filter(provincia == i_provincia)
    
  } else  if (i_departamento != "Todos") {
    DataFrame <- DataFrame %>%
      filter(departamento == i_departamento)
    
  } else {
    DataFrame <- DataFrame
  }
}

sum_casos <- function(DataFrame) {
  # Suma los casos del df otorgado
  DataFrame %>% 
    summarize(n = sum(n)) %>% 
    pull() %>% 
    as.character() %>% 
    prettyNum(big.mark = " ")
}


suma_poblacion <- function(DataFrame, value) {
  DataFrame <- DataFrame %>% 
    group_by(departamento, provincia, distrito) %>%
    summarise(n = sum({{value}}))
}


filtra_fechas <- function(DataFrame, fecha, fecha_inicio, fecha_fin){
  # Filtra DF según las fechas dadas
  DataFrame %>% 
    filter( {{fecha}} >= fecha_inicio & {{fecha}} <= fecha_fin)
}

### Funcion Indicador ###

unifica_tablas <- function(Positivos, Vacunacion, Poblacion, Pobreza){
  Positivos <- Positivos %>%
    group_by(departamento, provincia, distrito) %>%
    summarise(positivos = sum(n))
  
  Vacunacion <- Vacunacion %>%
    group_by(departamento, provincia, distrito) %>%
    summarise(vacunacion = sum(n))
  
  Poblacion <- Poblacion %>% rename(poblacion = n)
  
  tabla <- left_join(Positivos, Vacunacion) %>%
    left_join(Pobreza) %>%
    left_join(Poblacion) %>% 
    ungroup()
}

calculo_indicador <- function(DataFrame) {
  a = 0.41
  b = -0.17
  c = -0.19
  d = 0.72 
  st.d = 135.73
  me = 35.18
  
  DataFrame %>%
    mutate(raw = d + positivos*a + (vacunacion/poblacion)*c + pobreza*b) %>%
    mutate(indice = (raw - me)/(st.d)) %>%
    summarise(mean = mean(indice, na.rm = TRUE)) %>%
    pull()
}

