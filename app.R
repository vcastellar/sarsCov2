#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# #
# # set de prueba:
# input = list(tasas = FALSE, 
#              variable = "confirmed")

library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)

options(encoding="UTF-8")


# system("Rscript ./cargaDatos.R")
load("./data/datosESP.RData")
load("./data/mapas.RData")
source("customTheme.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),

  # Application title
  titlePanel("SARS-COV2 España"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 3,
      dateRangeInput("dates",
        label = h3("Date range"),
        start = "2020-01-01",
        end = "2020-12-31",
      ),

      hr(),

      selectInput("variable", h3("Selecciona variable"),
        c(
          "casos acumulados" = "confirmed",
          "casos diarios" = "daily_confirmed",
          "casos IA 14d" = "inc_14d",
          "razón de tasas casos IA 14d" = "rat_inc_14d",
          "fallecidos acumulados" = "deaths",
          "fallecidos diarios" = "daily_deaths",
          "fallecidos IA 14" = "inc_14d_deaths",
          "razón de tasas fall. IA 14d" = "rat_inc_14d_deaths",
          "porc. fallecidos vs casos" = "rat_acum_confirmed_vs_deaths",
          "porc. fallecidos vs casos 14d" = "rat_inc_14d_acum_confirmed_vs_deaths"
        ),
        multiple = FALSE,
        selected = "confirmados diarios"
      ),

      # Parámetro de suavización
      sliderInput("parmSuavizado",
        label = h4("Parámetro de suavizado"), min = 0,
        max = 100, value = 0
      ),
      # número de días de predicción
      sliderInput("diasPredict",
        label = h4("nº de días a predecir"), min = 0,
        max = DIAS_PREDICT, value = DIAS_PREDICT
      ),

      hr(),
      selectInput("selPAI", h3("País"),
        choices = listaPaises,
        selected = "Spain",
        multiple = TRUE
      ),



      selectInput("selCA", h3("Comunidades Autónomas:"),
        choices = listaComunidades,
        multiple = TRUE,
        selected = "Comunidad Valenciana"
      ),

      selectInput("selPO", h3("Provincia:"),
        choices = listaProvincias,
        multiple = TRUE,
        selected = "Valencia"
      ),

      br(),

      p('Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, doi:
  10.21105/joss.02376.')
    ),



    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Países",
          plotlyOutput("mapaWorld"),
          plotlyOutput("mainPlotPais")
        ),
        tabPanel(
          "Com. Autónomas",
          plotlyOutput("mapaCom"),
          plotlyOutput("mainPlotCom")
        ),
        tabPanel(
          "Provincias",
          plotlyOutput("mapaProv"),
          plotlyOutput("mainPlotProv")
        )
      ),

      checkboxInput("tasas",
        label = h6("representar en tasas x 100.000 hab."),
        value = TRUE
      )
    )
  )
)


#------------------------------------------------------------------------------
# Define server 
#------------------------------------------------------------------------------
server <- function(input, output, session) {
  #----------------------------------------------------------------------------
  # calculo de gráficas
  #----------------------------------------------------------------------------
  
  # métricas países
  #----------------------------------------------------------------------------
  mainCalcPais <- reactive({
    datosPais <- datosESP1 %>% filter(administrative_area_level_1 %in% input$selPAI &
      as.Date(date) >= input$dates[1] &
      as.Date(date) <= input$dates[2])
    datosPais <- datosPais %>% mutate(UnidadGeografica = administrative_area_level_1)


    datosPais <- datosPais %>%
      group_by(administrative_area_level_1) %>%
      mutate(
        nuOrden = ifelse(pred == FALSE, 0, 1),
        nuOrden = cumsum(nuOrden)
      ) %>%
      filter(nuOrden <= input$diasPredict)


    datosPais
  })

  # métricas comunidades
  #----------------------------------------------------------------------------
  mainCalcCom <- reactive({
    datosCom <- datosESP2 %>% filter(administrative_area_level_2 %in% input$selCA &
      as.Date(date) >= input$dates[1] &
      as.Date(date) <= input$dates[2])
    datosCom <- datosCom %>% mutate(UnidadGeografica = administrative_area_level_2)

    datosCom <- datosCom %>%
      group_by(UnidadGeografica) %>%
      mutate(
        nuOrden = ifelse(pred == FALSE, 0, 1),
        nuOrden = cumsum(nuOrden)
      ) %>%
      filter(nuOrden <= input$diasPredict)

    datosCom
  })

  # métricas provincias España
  #----------------------------------------------------------------------------
  mainCalcProv <- reactive({
    datosProv <- datosESP3 %>% filter(administrative_area_level_3 %in% input$selPO &
      as.Date(date) >= input$dates[1] &
      as.Date(date) <= input$dates[2])
    datosProv <- datosProv %>% mutate(UnidadGeografica = administrative_area_level_3)

    datosProv <- datosProv %>%
      group_by(UnidadGeografica) %>%
      mutate(
        nuOrden = ifelse(pred == FALSE, 0, 1),
        nuOrden = cumsum(nuOrden)
      ) %>%
      filter(nuOrden <= input$diasPredict)

    datosProv
  })

  # Calcula nombre de la métrica
  nameMetric <- reactive({
    paste0(input$variable, ifelse(input$tasas, "Rat", ""))
  })
  
  #----------------------------------------------------------------------------
  # Cálculo de mapas
  #----------------------------------------------------------------------------
  
  # cálculo de mapa x comunidades
  #----------------------------------------------------------------------------
  datMapaComunidades <- reactive({
    variable <- input$variable
    tasas <- input$tasas
    dat <- mapComunidades %>% 
      dplyr::select(name, variable, population, geometry)
    
    names(dat) <- c("name", "variable", "population", "geometry")
    dat[is.na(dat)] <- 0
    
    # tranformación de johnson para normalizar la variable. Esta variable 
    # transformada será la que definirá el color del area geografica
    tryCatch({
      y <- dat$variable
      if (!grepl("rat", variable) & tasas) {
        y <- dat$variable / dat$population * 1e5
      }
      isfinite <- sapply(y, is.finite)
      y[isfinite] <- jtrans(y[isfinite])$transformed
      nueva_col <- y 
    }, error = function(e) {
      nueva_col <<- dat$variable
    })
    
    
    # este es el valor numerico que se representa en el text del plotly
    nueva_var <- dat$variable
    if (grepl("rat", variable)) {
      nueva_var <- dat$variable
    }
    if (!grepl("rat", variable) & tasas) {
      nueva_var <- dat$variable / dat$population * 1e5
    }
    
    
    dat$Z_score<- nueva_col
    dat$variable <- nueva_var
    
    dat <- dat %>% filter(is.finite(Z_score))
  })
 
  #----------------------------------------------------------------------------
  # OUTPUTS
  #----------------------------------------------------------------------------
  
  # GRAFICAS
  #----------------------------------------------------------------------------
  
  # gráficas paises
  #----------------------------------------------------------------------------
  output$mainPlotPais <- renderPlotly({
    dat <- mainCalcPais()
    if (input$tasas & !grepl("rat", input$variable)) {
      denominador <- dat$population / 1e5
    } else {
      denominador <- 1
    }

    if (input$parmSuavizado != 0) {
      p <- ggplot(
        dat,
        aes(
          x = as.Date(date), y = !!as.symbol(input$variable) / denominador,
          color = as.factor(UnidadGeografica)
        )
      ) +
        geom_smooth(span = input$parmSuavizado / 100, se = FALSE) +
        custom_theme +
        xlab("Fecha") +
        ylab("n") +
        labs(color = "") +
        ggtitle(input$variable) +
        scale_x_date(date_breaks = "1 month")
    } else {
      p <- ggplot(
        dat,
        aes(as.Date(date), !!as.symbol(input$variable) / denominador,
          color = as.factor(UnidadGeografica)
        )
      ) +
        custom_theme +
        geom_line(linetype = ifelse(dat$pred, "dotted", "solid"), size = 0.75 - 0.5 * dat$pred) +
        xlab("Fecha") +
        ylab("n") +
        labs(color = "") +
        ggtitle(input$variable) +
        scale_x_date(date_breaks = "1 month")
    }

    ggplotly(p)
  })
  #----------------------------------------------------------------------------
  
  # gráficas comunidades
  #----------------------------------------------------------------------------
  output$mainPlotCom <- renderPlotly({
    dat <- mainCalcCom()
    if (input$tasas & !input$variable %in% c("rat_inc_14d", "rat_inc_14d_deaths", "rat_acum_confirmed_vs_deaths")) {
      denominador <- dat$population / 1e5
    } else {
      denominador <- 1
    }


    if (input$parmSuavizado != 0) {
      p <- ggplot(
        dat,
        aes(
          x = as.Date(date), y = !!as.symbol(input$variable) / denominador,
          color = as.factor(UnidadGeografica)
        )
      ) +
        geom_smooth(span = input$parmSuavizado / 100, se = FALSE, size = 1) +
        custom_theme +
        xlab("Fecha") +
        ylab("n") +
        labs(color = "") +
        ggtitle(input$variable) +
        scale_x_date(date_breaks = "1 month")
    } else {
      p <- ggplot(
        dat,
        aes(as.Date(date), !!as.symbol(input$variable) / denominador,
          color = as.factor(UnidadGeografica)
        )
      ) +
        custom_theme +
        geom_line() +
        xlab("Fecha") +
        ylab("n") +
        labs(color = "") +
        ggtitle(input$variable) +
        scale_x_date(date_breaks = "1 month")
    }

    ggplotly(p)
  })
  #----------------------------------------------------------------------------

  #----------------------------------------------------------------------------  
  # gráficas provincias
  #----------------------------------------------------------------------------
  output$mainPlotProv <- renderPlotly({
    dat <- mainCalcProv()
    if (input$tasas & !input$variable %in% c("rat_inc_14d", "rat_inc_14d_deaths", "rat_acum_confirmed_vs_deaths")) {
      denominador <- dat$population / 1e5
    } else {
      denominador <- 1
    }



    if (input$parmSuavizado != 0) {
      p <- ggplot(
        dat,
        aes(
          x = as.Date(date), y = !!as.symbol(input$variable) / denominador,
          color = as.factor(UnidadGeografica)
        )
      ) +
        geom_smooth(span = input$parmSuavizado / 100, se = FALSE, size = 1) +
        custom_theme +
        xlab("Fecha") +
        ylab("n") +
        labs(color = "") +
        scale_x_date(date_breaks = "1 month")
    } else {
      p <- ggplot(
        dat,
        aes(as.Date(date), !!as.symbol(input$variable) / denominador,
          color = as.factor(UnidadGeografica)
        )
      ) +
        custom_theme +
        geom_line() +
        xlab("Fecha") +
        ylab("n") +
        labs(color = "") +
        scale_x_date(date_breaks = "1 month")
    }

    ggplotly(p)
  })
  #----------------------------------------------------------------------------
  
  
  #----------------------------------------------------------------------------
  # MAPAS
  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------
  # pinta mapa del mundo
  #----------------------------------------------------------------------------
  output$mapaWorld <- renderPlotly({
    get(paste0("map_World", nameMetric()))
  })
  #----------------------------------------------------------------------------
  

  # Mapas comunidades
  #----------------------------------------------------------------------------
  output$mapaCom <- renderPlotly({
    plot_ly(mapComunidades,
            showlegend = FALSE,
            size = 8,
            line = list(color = rgb(1, 1, 1, maxColorValue = 256),
                        width = 0.5)
            ) %>% 
      layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
             paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
             title = list(text = paste(variable),
                         font = list(color = "#8B9BA8",
                                     size = 14)),
                         margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
            ) %>% 
      add_sf(data = datMapaComunidades(),
             split = ~name,
             color = ~Z_score,
             colors = c("#21D19F", "#FFFFFF", "#FC3C0C"),
             alpha = 0.8,
             hoveron = "fills",
             hoverinfo = "text",
             text = ~paste0(name, ": ", 
                            round(dat$variable, 2),
                            ifelse(tasas & !grepl("rat", variable), " x 1e5 hab", "")),
             hoveron = "fills",
             hoverinfo = "text")
  })
  
  # observeEvent(input$variable, {
  #   dat <- datMapaComunidades()
  #   plotlyProxy("mapaCom", session) %>%
  #     plotlyProxyInvoke(
  #       "add_sf",
  #       list(
  #         split = ~dat$name,
  #         color = ~dat$Z_score,
  #         colors = c("#21D19F", "#FFFFFF", "#FC3C0C"),
  #         alpha = 0.8,
  #         hoveron = "fills",
  #         hoverinfo = "text",
  #         text = ~paste0(dat$name, ": ",
  #                        round(dat$variable, 2),
  #                        ifelse(tasas & !grepl("rat", variable), " x 1e5 hab", "")),
  #         hoveron = "fills",
  #         hoverinfo = "text"
  #       )
  #     )
  # }
  # 
  # )
  #----------------------------------------------------------------------------
  

  # Mapas provincias
  #----------------------------------------------------------------------------
  output$mapaProv <- renderPlotly({
    mapPlot(x = mapProvincias, 
            variable = input$variable,
            tasas = input$tasas)
    
  })
  #----------------------------------------------------------------------------
}

# Run the application
shinyApp(ui = ui, server = server)
