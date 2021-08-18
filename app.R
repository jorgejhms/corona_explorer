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
memory.limit(size = 250000) # Solo windows
memory.size() # Solo Windows

# Carga de Data
positivos <- fst::read_fst("data/positivos.fst")
fallecidos <- fst::read_fst("data/fallecidos.fst")
vacunacion <- fst::read_fst("data/vacunacion.fst")

# Funciones
source("funciones.R")

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
        # Gráfica positivos diarios
        temp_pos() %>%
            select(fecha_resultado) %>%
            group_by(fecha_resultado) %>%
            summarise(positivos = n()) %>%
            mutate(positivos_cum = cumsum(replace_na(positivos, 0))) %>%
            filter(fecha_resultado >= input$date[1] &
                       fecha_resultado <= input$date[2]) %>%
            plot.diarios(fecha_resultado, positivos)
    })
    
    
    output$p_fal <- renderPlot({
        # Gráfica fallecidos diarios
        temp_fal() %>%
            select(fecha_fallecimiento) %>%
            group_by(fecha_fallecimiento) %>%
            summarise(fallecidos = n()) %>%
            mutate(fallecidos_cum = cumsum(replace_na(fallecidos, 0))) %>%
            filter(fecha_fallecimiento >= input$date[1] &
                       fecha_fallecimiento <= input$date[2]) %>%
            plot.diarios(fecha_fallecimiento, fallecidos)
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
