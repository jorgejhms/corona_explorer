# Funciones

fechaFix <- function(variable){
  # Convierte clase de variable de números a fecha
    variable %>%
        as.character() %>% 
        as.Date(format = "%Y%m%d")
}

plot.diarios <- function(DataFrame, x, y) {
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
