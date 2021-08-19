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

# Calculo de valores generales --------------------------------------------

# Numero de casos
fecha_positivos <- last(sort(positivos$fecha_resultado))
fecha_fallecidos <- last(sort(fallecidos$fecha_fallecimiento))
fecha_vacunacion <- last(sort(vacunacion$fecha_vacunacion))
fecha_actualizacion <- fecha_vacunacion

total_positivos <- sum(positivos$n)
total_fallecidos <- sum(fallecidos$n)
total_vacunas <- sum(vacunacion$n)

# Total de vacunaciones según dosis
lista_dosis <- vacunacion %>%
    group_by(dosis) %>%
    summarise(n = sum(n)) %>%
    adorn_totals() %>%
    deframe()

# Numero de positivos por día
# tabla_positivos <- positivos %>% 
#     group_by(fecha_resultado) %>% 
#     summarise(n = sum(n))

# Número de fallecidos por día
# tabla_fallecidos <- fallecidos %>% 
#     group_by(fecha_fallecimiento) %>% 
#     summarise(n = sum(n))



# Tabla de vacunaciones por día, dosis y acumuladas
tabla_vacunaciones <- vacunacion %>%
    pivot_wider(
        id_cols = fecha_vacunacion,
        values_from = n,
        names_from = dosis,
        names_prefix = "dosis_",
        values_fn = sum,
        values_fill = 0
    ) %>%
    arrange(fecha_vacunacion) %>%
    mutate(total = dosis_1 + dosis_2) %>%
    mutate(
        dosis_1_cum = cumsum(dosis_1),
        dosis_2_cum = cumsum(dosis_2),
        total_cum = cumsum(total)
    ) %>%
    pivot_longer(cols = -fecha_vacunacion) %>%
    filter(str_detect(name, "cum"))

# Frontend ----------------------------------------------------------------

ui <- fluidPage(# Application title
    titlePanel("Corona Explorer"),
    
    sidebarLayout(
        
        # Controles Barra lateral
        sidebarPanel(
            #Inputs
            dateRangeInput(
                # Selector de fechas
                inputId = "fecha",
                label = "Fechas",
                start = min(positivos$fecha_resultado),
                end = fecha_actualizacion,
                min = min(positivos$fecha_resultado),
                max = fecha_actualizacion
            ),
            
            selectInput(
                # Selector de Región
                inputId = "departamento",
                label = "Escoge tu región:",
                choices = c("Todos", 
                            as.character(unique(positivos$departamento))
                            ),
                selected = "Todos"
            ), 
        ),
        
        mainPanel(
            # Valores a mostrar
            textOutput("fecha_actualizacion"),
            textOutput("total_positivos"),
            textOutput("total_fallecidos"),
            textOutput("total_vacunas"),
            textOutput("total_primera_dosis"),
            textOutput("total_segunda_dosis"),
            textOutput("positivos_dia"),
            textOutput("fallecidos_dia"),
            textOutput("vacunados_dia"),
            textOutput("primera_dosis_dia"),
            textOutput("segunda_dosis_dia"),
            
            # Gráficos
            plotOutput("grafico_positivos"),
            plotOutput("grafico_fallecidos"),
            plotOutput("grafico_vacunaciones"),
            
        )
    ))

# Backend -----------------------------------------------------------------

server <- function(input, output) {
    
    # Filtros de regiones
    positivos_filtrada <- reactive({
        if (input$departamento != "Todos") {
            positivos <- positivos %>%
                filter(departamento == input$departamento)
        } else {
            positivos <- positivos
        }
    })
    
    fallecidos_filtrada <- reactive({
        if (input$departamento != "Todos") {
            fallecidos <- fallecidos %>%
                filter(departamento == input$departamento)
        } else {
            fallecidos <- fallecidos
        }
    })
    
    vacunacion_filtrada <- reactive({
        if (input$departamento != "Todos") {
            vacunacion <- vacunacion %>%
                filter(departamento == input$departamento)
        } else {
            vacunacion <- vacunacion
        }
    })
    
    
    # Calculo de valores
    
    output$fecha_actualizacion <- renderText({
        as.character(fecha_actualizacion)
    })
    
    output$total_positivos <- renderText({
        as.character(total_positivos)
    })
    
    output$total_fallecidos <- renderText({
        as.character(total_fallecidos)
    })
    
    output$total_vacunas <- renderText({
        as.character(total_vacunas)
    })
    
    output$total_primera_dosis <- renderText({
        as.character(lista_dosis[[1]])
    })
    
    output$total_segunda_dosis <- renderText({
        as.character(lista_dosis[[2]])
    })
    
    output$positivos_dia <-
        renderText({
            positivos %>%
                group_by(fecha_resultado) %>%
                summarise(n = sum(n)) %>%
                filter (fecha_resultado == fecha_actualizacion) %>%
                pull()
            
        })
    
    output$fallecidos_dia <-
        renderText({
            fallecidos %>%
                group_by(fecha_fallecimiento) %>%
                summarise(n = sum(n)) %>%
                filter (fecha_fallecimiento == fecha_actualizacion) %>%
                pull()
        })
    
    # Gráficos
    
    output$grafico_positivos <- renderPlot({
        # Gráfica positivos diarios
        positivos_filtrada() %>%
            group_by(fecha_resultado) %>%
            summarise(n = sum(n)) %>%
            mutate(positivos_cum = cumsum(n)) %>%
            filter(fecha_resultado >= input$fecha[1] &
                       fecha_resultado <= input$fecha[2]) %>%
            plot.diarios(fecha_resultado, n)
    })
    
    
    output$grafico_fallecidos <- renderPlot({
        # Gráfica fallecidos diarios
        fallecidos_filtrada() %>%
            group_by(fecha_fallecimiento) %>%
            summarise(n = sum(n)) %>%
            mutate(fallecidos_cum = cumsum(n)) %>%
            filter(fecha_fallecimiento >= input$fecha[1] &
                       fecha_fallecimiento <= input$fecha[2]) %>%
            plot.diarios(fecha_fallecimiento, n)
    })
    
    output$grafico_vacunaciones <- renderPlot({
        # Grafica de vacunaciones
        vacunacion_filtrada() %>%
            pivot_wider(
                id_cols = fecha_vacunacion,
                values_from = n,
                names_from = dosis,
                names_prefix = "dosis_",
                values_fn = sum,
                values_fill = 0
            ) %>%
            arrange(fecha_vacunacion) %>%
            mutate(total = dosis_1 + dosis_2) %>%
            mutate(
                dosis_1_cum = cumsum(dosis_1),
                dosis_2_cum = cumsum(dosis_2),
                total_cum = cumsum(total)
            ) %>%
            pivot_longer(cols = -fecha_vacunacion) %>%
            filter(str_detect(name, "cum")) %>%
            plot.vacunaciones(fecha_vacunacion, value, name) +
            labs(title = "Progreso de las vacunaciones") +
            scale_color_discrete(
                labels = c("Primera dosis", "Segunda dosis", "Total"))
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
