# Corona Explorer

# Setup -------------------------------------------------------------------

# setea el sistema en español
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8") 

# Librerias
library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)
library(shinydashboard)

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


# Header ------------------------------------------------------------------

header <- dashboardHeader(
    title = "Corona Explorer"
)

# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
    #Inputs
    dateRangeInput(
        # Selector de fechas
        inputId = "fecha",
        label = "Fechas",
        # start = min(positivos$fecha_resultado),
        start = today() - 90,
        end = fecha_actualizacion,
        min = min(positivos$fecha_resultado),
        max = fecha_actualizacion
    ),

    selectInput(
        # Selector de Región
        inputId = "departamento",
        label = "Escoge tu región:",
        choices = c("Todos",
                    as.character(
                        unique(positivos$departamento)
                    )),
        selected = "Todos"
    ),

    selectInput(
        # Selector de Provincia
        inputId = "provincia",
        label = "Provincia:",
        choices = NULL,
    ),

    selectInput(
        # Selector de Distrito
        inputId = "distrito",
        label = "Distrito:",
        choices = NULL
    )
)
 
# Body --------------------------------------------------------------------

body <- dashboardBody(

    # Primera fila
    fluidRow(

        infoBox(
            "Fecha de actualización",
            fecha_actualizacion,
            color = "blue",
            width = 4,
            icon = icon("calendar-alt")
        ),

        infoBox(
            "Total de positivos",
            prettyNum(total_positivos, big.mark = " "),
            color = "yellow",
            width = 4,
            icon = icon("virus")
        ),

        infoBox(
            "Total de fallecidos",
            prettyNum(total_fallecidos, big.mark = " "),
            color = "red",
            width = 4,
            icon = icon("skull-crossbones")
        )
    ),

    # Segunda fila
    fluidRow(

        infoBox(
            "Total de Vacunas aplicadas",
            prettyNum(lista_dosis[[3]], big.mark = " "),
            color = "green",
            width = 4,
            fill = TRUE,
            icon = icon("syringe")
        ),

        infoBox(
            "Primera dosis",
            prettyNum(lista_dosis[[1]], big.mark = " "),
            color = "lime",
            width = 4,
            icon = icon("syringe")
        ),

        infoBox(
            "Segunda dosis",
            prettyNum(lista_dosis[[2]], big.mark = " "),
            color = "green",
            width = 4,
            icon = icon("syringe")
        )

    ),


    textOutput("positivos_dia"),
    textOutput("fallecidos_dia"),
    textOutput("vacunados_dia"),
    textOutput("primera_dosis_dia"),
    textOutput("segunda_dosis_dia"),

    # Gráficos

    fluidRow(
        column(width = 8,

               box(
                   title = "Positivos diario",
                   width = NULL,
                   status = "warning",
                   plotOutput("grafico_positivos")
                   ),

               box(
                   title = "Fallecidos diario",
                   width = NULL,
                   status = "warning",
                   plotOutput("grafico_fallecidos")
                   ),

               box(
                   title = "Vacunaciones por día",
                   width = NULL,
                   status = "success",
                   plotOutput("grafico_vacunaciones")
                   )
               ),
        
        column(width = 4,
               infoBoxOutput("positivos_zona", width = NULL),
               infoBoxOutput("fallecidos_zona", width = NULL),
               infoBoxOutput("vacunaciones_zona", width = NULL)
               )
    )

)

# Frontend ----------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)

# Backend -----------------------------------------------------------------

server <- function(input, output, session) {

    # Actualización del selector
    observe({
        updateSelectInput(
            session,
            "provincia",
            choices = c(
                "Todos",
                positivos %>%
                    filter(departamento == input$departamento) %>%
                    pull(provincia) %>%
                    unique()
            ),
            selected = "Todos"
        )
    })

    observe({
        updateSelectInput(
            session,
            "distrito",
            choices = c(
                "Todos",
                positivos %>%
                    filter(provincia == input$provincia) %>%
                    pull(distrito) %>%
                    unique()
            ),
            selected = "Todos"
        )
    })

    # Filtros de regiones (Casos positivos)
    positivos_filtrada <- reactive({
        filtra_zona(positivos,
                    input$departamento,
                    input$provincia,
                    input$distrito)
    })

    # Filtros de regiones (Casos fallecidos)
    fallecidos_filtrada <- reactive({
        filtra_zona(fallecidos,
                    input$departamento,
                    input$provincia,
                    input$distrito)
    })

    # Filtro de zona (Vacunaciones)
    vacunacion_filtrada <- reactive({
        filtra_zona(vacunacion,
                    input$departamento,
                    input$provincia,
                    input$distrito)
    })


    # Calculo de valores

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
    
    # Valores de la zona filtrada
    output$positivos_zona <- renderInfoBox({
        infoBox(
            "Positivos en la zona",
            positivos_filtrada() %>% 
                filtra_fechas(
                    fecha_resultado,
                    input$fecha[1],
                    input$fecha[2]
                    ) %>% 
                sum_casos(),
            fill = TRUE
        )
    })
    
   output$fallecidos_zona <- renderInfoBox({
       infoBox(
           "Fallecidos en la zona",
           fallecidos_filtrada() %>% 
               filtra_fechas(fecha_fallecimiento,
                             input$fecha[1],
                             input$fecha[2]
                             ) %>% 
               sum_casos(),
           # sum_casos(fallecidos_filtrada()),
           fill = TRUE,
           color = "red"
       )
   })
   
   output$vacunaciones_zona <- renderInfoBox({
       infoBox(
           "Vacunados en la zona",
           vacunacion_filtrada() %>% 
               filtra_fechas(fecha_vacunacion,
                             input$fecha[1],
                             input$fecha[2]
                            ) %>% 
               sum_casos(),
           # sum_casos(vacunacion_filtrada()),
           fill = TRUE,
           color = "green"
       )
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
            filter(fecha_vacunacion >= input$fecha[1] &
                       fecha_vacunacion <= input$fecha[2]) %>%
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