# Corona Explorer

# Setup -------------------------------------------------------------------

# setea el sistema en español
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8") 

# Librerias
library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)

# aumento memoría
# memory.limit(size = 250000) Solo windows
# memory.size() Solo Windows

# Carga de Data
positivos <- data.table::fread("data/positivos_covid.csv", sep = ";")
fallecidos <- data.table::fread("data/fallecidos_covid.csv", sep = ";")
vacunacion <- data.table::fread("data/vacunas_covid.csv", sep = ",")

# Funciones
fechaFix <- \(variable){
    variable %>%
        as.character() %>% 
        as.Date(format = "%Y%m%d")
}

# Limpieza de datos
positivos <- positivos %>%
    clean_names() %>%
    mutate(
        fecha_corte = fechaFix(fecha_corte),
        fecha_resultado = fechaFix(fecha_resultado),
        #edad = as.numeric(edad),
        departamento = recode(departamento, "LIMA REGION" = "LIMA") #Junta Lima y Lima pronvincias
    ) %>%
    filter(!is.na(fecha_resultado))

fallecidos <- fallecidos %>%
    clean_names() %>%
    mutate(
        fecha_corte = fechaFix(fecha_corte),
        fecha_fallecimiento = fechaFix(fecha_fallecimiento)
    ) %>%
    filter(!is.na(fecha_fallecimiento))

vacunacion <- vacunacion %>%
    clean_names() %>%
    mutate(
        fecha_corte = fechaFix(fecha_corte), 
        fecha_vacunacion = fechaFix(fecha_vacunacion) 
    ) %>%
    filter(!is.na(fecha_vacunacion))

## Datos importantes
fecha_actualizacion <- positivos$fecha_corte %>% last()


# Frontend ----------------------------------------------------------------

ui <- fluidPage(# Application title
    titlePanel("Corona Explorer"),
    
    sidebarLayout(
        
        # Controles Barra lateral
        sidebarPanel(
            #Inputs
            dateRangeInput(
                # Selector de fechas
                inputId = "date",
                label = "Fechas",
                start = min(positivos$fecha_resultado),
                end = fecha_actualizacion,
                min = min(positivos$fecha_resultado),
                max = fecha_actualizacion
            ),
            
            selectInput(
                # Selector de Región
                inputId = "dep",
                label = "Escoge tu región:",
                choices = c("Todos", as.character(unique(
                    positivos$departamento
                ))),
                selected = "Todos"
            ),
            
            sliderInput(
                inputId = "d",
                label = "¿Media de cuantos días?",
                min = 2,
                max = 7,
                value = 2,
                step = 1
            ),
        ),
        
        mainPanel(
            # Valores a mostrar
            textOutput("fecha_actualizacion"),
            textOutput("t_pos"),
            textOutput("t_fal"),
            textOutput("d_pos"),
            textOutput("d_fal"),
            
            # Gráficos
            plotOutput("p_pos"),
            plotOutput("p_fal"),
        )
    ))

# Backend -----------------------------------------------------------------

server <- function(input, output) {
    
    # Filtros de regiones
    temp_pos <- reactive({
        if (input$dep != "Todos") {
            positivos <- positivos %>%
                filter(departamento == input$dep)
        } else {
            positivos <- positivos
        }
    })
    
    temp_fal <- reactive({
        if (input$dep != "Todos") {
            fallecidos <- fallecidos %>%
                filter(departamento == input$dep)
        } else {
            fallecidos <- fallecidos
        }
    })
    
    
    # Calculo de valores
    
    output$fecha_actualizacion <-
        renderText({
            as.character(fecha_actualizacion)
        })
    
    output$t_pos <-
        renderText({
            positivos %>% summarise(positivos = n()) %>% pull()
        })
    
    output$t_fal <-
        renderText({
            fallecidos %>% summarise (fallecidos = n()) %>% pull()
        })
    
    output$d_pos <-
        renderText({
            positivos %>%
                select(fecha_resultado) %>%
                group_by(fecha_resultado) %>%
                summarise(positivos = n()) %>%
                filter (fecha_resultado == fecha_actualizacion) %>%
                pull()
            
        })
    
    output$d_fal <-
        renderText({
            fallecidos %>%
                select(fecha_fallecimiento) %>%
                group_by(fecha_fallecimiento) %>%
                summarise(fallecidos = n()) %>%
                filter (fecha_fallecimiento == fecha_actualizacion) %>%
                pull()
        })
    
    # Gráficos
    
    output$p_pos <- renderPlot({
        temp_pos() %>%
            select(fecha_resultado) %>%
            group_by(fecha_resultado) %>%
            summarise(positivos = n()) %>%
            mutate(positivos_cum = cumsum(replace_na(positivos, 0))) %>%
            filter(fecha_resultado >= input$date[1] &
                       fecha_resultado <= input$date[2]) %>%
            ggplot(aes(x = fecha_resultado, y = positivos)) +
            geom_bar(stat = "identity") +
            geom_line(aes(y = zoo::rollmean(positivos, input$d, fill = NA)),
                      size = 1.2,
                      colour = "red1") +
            scale_x_date(
                date_labels = "%b",
                breaks = "1 month",
                minor_breaks = NULL
            ) +
            theme(legend.position = "bottom"
                  ,
                  legend.title = element_blank()) +
            labs(x = element_blank(),
                 y = element_blank(),
                 title = element_blank())
    })
    
    
    output$p_fal <- renderPlot({
        temp_fal() %>%
            select(fecha_fallecimiento) %>%
            group_by(fecha_fallecimiento) %>%
            summarise(fallecidos = n()) %>%
            mutate(fallecidos_cum = cumsum(replace_na(fallecidos, 0))) %>%
            filter(fecha_fallecimiento >= input$date[1] &
                       fecha_fallecimiento <= input$date[2]) %>%
            ggplot(aes(x = fecha_fallecimiento, y = fallecidos)) +
            geom_bar(stat = "identity") +
            geom_line(aes(y = zoo::rollmean(fallecidos, input$d, fill = NA)),
                      size = 1.2,
                      colour = "red1") +
            scale_x_date(
                date_labels = "%b",
                breaks = "1 month",
                minor_breaks = NULL
            ) +
            theme(legend.position = "bottom"
                  ,
                  legend.title = element_blank()) +
            labs(x = element_blank(),
                 y = element_blank(),
                 title = element_blank())
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
