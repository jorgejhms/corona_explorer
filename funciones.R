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
  unz("data/vacunas_covid.7z", "data/vacunas_covid.csv")
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
    geom_line(aes(y = zoo::rollmean({{y}}, 3, fill = NA)),
              size = 1.2,
              colour = "red1") +
    scale_x_date(date_labels = "%b",
                 breaks = "1 month",
                 minor_breaks = NULL) +
    theme(legend.position = "none",
          legend.title = element_blank()) +
    labs(x = element_blank(),
         y = element_blank(),
         title = element_blank())
}
