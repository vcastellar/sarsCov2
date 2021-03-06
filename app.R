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
library(shinybusy)
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)
library(jtrans)
library(zoo)

options(encoding = "UTF-8")


# system("Rscript ./cargaDatos.R")
load("./data/datosESP.RData")
source("customTheme.R")

DATE_START <- as.Date('2020-01-01')

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),

  # Application title
  # titlePanel("SARS-COV2 España"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 3,

      hr(),

      selectInput("variable", h3("Selecciona variable"),
        c(
          "nº casos totales"= "confirmed",
          "nº casos diarios"= "daily_confirmed",
          "incidencia de casos últimos 14 días (IA 14d)" = "inc_14d",
          "incidencia de casos últimos 7 días (IA 7d)" = "inc_7d",
          "razón de tasas IA 14d"= "rat_inc_14d",
          "razón de tasas IA 7d"= "rat_inc_7d",
          
          "nº de defunciones" = "deaths",
          "nº defunciones diarias"= "daily_deaths",
          "incidencia de defunciones últimos 14 días (IA 14d)" = "inc_14d_deaths",
          "razón de tasas defunciones IA 14d" = "rat_inc_14d_deaths",
          "porcentaje defunciones frente a casos confirmados"= "rat_acum_confirmed_vs_deaths",
          "porcentaje defunciones frente a casos últimos 14d" = "rat_inc_14d_acum_confirmed_vs_deaths"
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


      checkboxInput("tasas",
        label = h6("representar en tasas x 100.000 hab."),
        value = TRUE
      ),

      hr(),


      br(),

      p('Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, doi:
  10.21105/joss.02376.'),
      p("https://opendata.ecdc.europa.eu/covid19/"),

      textOutput("info")
    ),



    # Show a plot of the generated distribution
    mainPanel(
      width = 9,
      add_busy_spinner(spin = "fading-circle", color = "#F3FFBD"),
      navbarPage("SARS-COV2",
        
        tabPanel(
          "Países",
          fluidRow(
            column(
              10, 
              plotlyOutput("mapaWorld", height = 400)
            ),
            column(
              2,
              br(), br(), br(), br(),
              br(), br(), br(), br(),
              dateInput("datesObsWorld",
                        label = h5("Fecha de Observación"),
                        value = Sys.Date())
              )
            ),
          
          fluidRow(
            column(
              10,
              plotlyOutput("mainPlotPais", height = 400)
            ),
            column(
              2,
              br(), br(), br(), br(),
              br(), br(), br(), br(),
              selectInput("selPAI", h5("Selecciona Países"),
                choices = listaPaises,
                selected = "Spain",
                multiple = TRUE
              )
            )
          )
        ),
        
        
        
        tabPanel(
          "Com. Autónomas",
          
          fluidRow(
            column(
              10, 
              plotlyOutput("mapaCom", height = 400)
            ),
            column(
              2,
              br(), br(), br(), br(),
              br(), br(), br(), br(),
              dateInput("datesObsCom",
                        label = h5("Fecha de Observación"),
                        value = Sys.Date())
            )
          ),
          
          fluidRow(
            column(
              10,
              plotlyOutput("mainPlotCom", height = 400)
            ),
            column(
              2,
              br(), br(), br(), br(),
              br(), br(), br(), br(),
              selectInput("selCA", h6("Selecciona C.A.:"),
                choices = listaComunidades,
                multiple = TRUE,
                selected = "Comunidad Valenciana"
              ),
            )
          )
        ),
        
        
        
        tabPanel(
          "Provincias",
          
          fluidRow(
            column(
              10, 
              plotlyOutput("mapaProv", height = 400)
            ),
            column(
              2,
              br(), br(), br(), br(),
              br(), br(), br(), br(),
              dateInput("datesObsProv",
                        label = h5("Fecha de Observación"),
                        value = Sys.Date())
            )
          ),
          
          fluidRow(
            column(
              10,
              plotlyOutput("mainPlotProv", height = 400)
            ),
            column(
              2,
              br(), br(), br(), br(),
              br(), br(), br(), br(),
              selectInput("selPO", h6("Selecciona Prov.:"),
                choices = listaProvincias,
                multiple = TRUE,
                selected = "Valencia"
              )
            )
          )
        )
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
      as.Date(date) >= DATE_START &
      as.Date(date) <= Sys.Date() + input$diasPredict)
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
      as.Date(date) >= DATE_START &
      as.Date(date) <= as.Date(Sys.Date()) + input$diasPredict)
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
      as.Date(date) >= DATE_START &
      as.Date(date) <= as.Date(Sys.Date()) + input$diasPredict)
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


  # cálculo de mapas x paises
  #----------------------------------------------------------------------------
  datMapaWorld <- reactive({
    
    variable <- input$variable
    tasas <- input$tasas
    
    # seleccionamos datos a fecha de observacion
    fechas <- datosMapWorld %>% 
      group_by(id) %>% 
      summarise(maxFec = max(date),
                obsFec = min(input$datesObsWorld, maxFec))
    
    datosMapWorldAux <- merge(x = datosMapWorld, y = fechas, by = "id")
    
    datosMapWorldAux <- datosMapWorldAux %>% 
      filter(date == obsFec)
    
    # añadimos a los datos la geometria para SF
    world <- merge(x = world_SF, y = datosMapWorldAux, by = "id")
    
    dat <- world %>%
      filter(date <= as.character(input$datesObsWorld)) %>%
      arrange(name, date) %>%
      group_by(name) %>%
      summarise_all(function(x) tail(x, 1)) %>%
      select("name", variable, "population", "geometry")

    names(dat) <- c("name", "variable", "population", "geometry")
    dat[is.na(dat)] <- 0

    # tranformación de johnson para normalizar la variable. Esta variable
    # transformada será la que definirá el color del area geografica
    tryCatch(
      {
        y <- dat$variable
        if (!grepl("rat", variable) & tasas) {
          y <- dat$variable / dat$population * 1e5
        }
        isfinite <- sapply(y, is.finite)
        y[isfinite] <- jtrans(y[isfinite])$transformed
        nueva_col <- y
      },
      error = function(e) {
        nueva_col <<- dat$variable
      }
    )


    # este es el valor numerico que se representa en el text del plotly
    nueva_var <- dat$variable
    if (grepl("rat", variable)) {
      nueva_var <- dat$variable
    }
    if (!grepl("rat", variable) & tasas) {
      nueva_var <- dat$variable / dat$population * 1e5
    }

    # asignamos la variable Z-score
    dat$Z_score <- nueva_col
    dat$variable <- nueva_var

    dat$Z_score[!is.finite(dat$Z_score)] <- NA
    dat
  })
  #----------------------------------------------------------------------------

  # cálculo de mapa x comunidades
  #----------------------------------------------------------------------------
  datMapaComunidades <- reactive({
    variable <- input$variable
    tasas <- input$tasas
    
    # seleccionamos datos a fecha de observacion
    fechas <- datosMapCom %>% 
      group_by(administrative_area_level_2) %>% 
      summarise(maxFec = max(date),
                obsFec = min(input$datesObsCom, maxFec))
    
    datosMapComAux <- merge(x = datosMapCom, y = fechas, by = "administrative_area_level_2")
    
    datosMapComAux <- datosMapComAux %>% 
      filter(date == obsFec)
    
    # añadimos a los datos la geometria para SF
    com <- merge(x = comunidades_SF, y = datosMapComAux, 
                 by.x = "name", by.y = "administrative_area_level_2")
    
    dat <- com %>%
      filter(date <= as.character(input$datesObsCom)) %>%
      arrange(name, date) %>%
      group_by(name) %>%
      summarise_all(function(x) tail(x, 1)) %>%
      select("name", variable, "population", "geometry")
    
    names(dat) <- c("name", "variable", "population", "geometry")
    dat[is.na(dat)] <- 0
    
    # tranformación de johnson para normalizar la variable. Esta variable
    # transformada será la que definirá el color del area geografica
    tryCatch(
      {
        y <- dat$variable
        if (!grepl("rat", variable) & tasas) {
          y <- dat$variable / dat$population * 1e5
        }
        isfinite <- sapply(y, is.finite)
        y[isfinite] <- jtrans(y[isfinite])$transformed
        nueva_col <- y
      },
      error = function(e) {
        nueva_col <<- dat$variable
      }
    )
    
    
    # este es el valor numerico que se representa en el text del plotly
    nueva_var <- dat$variable
    if (grepl("rat", variable)) {
      nueva_var <- dat$variable
    }
    if (!grepl("rat", variable) & tasas) {
      nueva_var <- dat$variable / dat$population * 1e5
    }
    
    # asignamos la variable Z-score
    dat$Z_score <- nueva_col
    dat$variable <- nueva_var
    
    dat$Z_score[!is.finite(dat$Z_score)] <- NA
    dat
  })
  #----------------------------------------------------------------------------


  # cálculo de mapa x provincias
  #----------------------------------------------------------------------------
  datMapaProvincias <- reactive({
    variable <- input$variable
    tasas <- input$tasas
    
    # seleccionamos datos a fecha de observacion
    fechas <- datosMapProv %>% 
      group_by(administrative_area_level_3) %>% 
      summarise(maxFec = max(date),
                obsFec = min(input$datesObsProv, maxFec))
    
    datosMapProvAux <- merge(x = datosMapProv, y = fechas, by = "administrative_area_level_3")
    
    datosMapProvAux <- datosMapProvAux %>% 
      filter(date == obsFec)
    
    # añadimos a los datos la geometria para SF
    prov <- merge(x = provincias_SF, y = datosMapProvAux, 
                  by.x = "name", by.y = "administrative_area_level_3")
    
    dat <- prov %>%
      filter(date <= as.character(input$datesObsProv)) %>%
      arrange(name, date) %>%
      group_by(name) %>%
      summarise_all(function(x) tail(x, 1)) %>%
      select("name", variable, "population", "geometry")
    
    names(dat) <- c("name", "variable", "population", "geometry")
    dat[is.na(dat)] <- 0
    
    # tranformación de johnson para normalizar la variable. Esta variable
    # transformada será la que definirá el color del area geografica
    tryCatch(
      {
        y <- dat$variable
        if (!grepl("rat", variable) & tasas) {
          y <- dat$variable / dat$population * 1e5
        }
        isfinite <- sapply(y, is.finite)
        y[isfinite] <- jtrans(y[isfinite])$transformed
        nueva_col <- y
      },
      error = function(e) {
        nueva_col <<- dat$variable
      }
    )
    
    
    # este es el valor numerico que se representa en el text del plotly
    nueva_var <- dat$variable
    if (grepl("rat", variable)) {
      nueva_var <- dat$variable
    }
    if (!grepl("rat", variable) & tasas) {
      nueva_var <- dat$variable / dat$population * 1e5
    }
    
    # asignamos la variable Z-score
    dat$Z_score <- nueva_col
    dat$variable <- nueva_var
    
    dat$Z_score[!is.finite(dat$Z_score)] <- NA
    dat
  })
  #----------------------------------------------------------------------------



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
        ylab(paste(tipoVar[[input$variable]]$unidad, 
                   ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
        labs(color = "") +
        ggtitle(paste(tipoVar[[input$variable]]$descripcion, 
                      ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
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
        ylab(paste(tipoVar[[input$variable]]$unidad, 
                   ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
        labs(color = "") +
        ggtitle(paste(tipoVar[[input$variable]]$descripcion, 
                      ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
        scale_x_date(date_breaks = "1 month")
    }

    ggplotly(p)
  })
  #----------------------------------------------------------------------------

  # gráficas comunidades
  #----------------------------------------------------------------------------
  output$mainPlotCom <- renderPlotly({
    dat <- mainCalcCom()
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
        geom_smooth(span = input$parmSuavizado / 100, se = FALSE, size = 1) +
        custom_theme +
        xlab("Fecha") +
        ylab(paste(tipoVar[[input$variable]]$unidad, 
                   ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
        labs(color = "") +
        ggtitle(paste(tipoVar[[input$variable]]$descripcion, 
                      ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
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
        ylab(paste(tipoVar[[input$variable]]$unidad, 
                   ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
        labs(color = "") +
        ggtitle(paste(tipoVar[[input$variable]]$descripcion, 
                      ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
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
        geom_smooth(span = input$parmSuavizado / 100, se = FALSE, size = 1) +
        custom_theme +
        xlab("Fecha") +
        ylab(paste(tipoVar[[input$variable]]$unidad, 
                   ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
        labs(color = "") +
        ggtitle(paste(tipoVar[[input$variable]]$descripcion,
                      ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
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
        ylab(paste(tipoVar[[input$variable]]$unidad, 
                   ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
        ylab("n") +
        labs(color = "") +
        ggtitle(paste(tipoVar[[input$variable]]$descripcion, 
                      ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", ""))) +
        scale_x_date(date_breaks = "1 month")
    }

    ggplotly(p)
    # %>% layout(
    #   xaxis = list(
    #     rangeselector = list(
    #       buttons = list(
    #         list(
    #           count = 3,
    #           label = "3 mo",
    #           step = "month",
    #           stepmode = "backward"),
    #         list(
    #           count = 6,
    #           label = "6 mo",
    #           step = "month",
    #           stepmode = "backward"),
    #         list(
    #           count = 1,
    #           label = "1 yr",
    #           step = "year",
    #           stepmode = "backward"),
    #         list(
    #           count = 1,
    #           label = "YTD",
    #           step = "year",
    #           stepmode = "todate"),
    #         list(step = "all"))),
    #
    #     rangeslider = list(type = "date")))
  })
  #----------------------------------------------------------------------------


  #----------------------------------------------------------------------------
  # MAPAS
  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------
  # pinta mapa del mundo
  #----------------------------------------------------------------------------
  # output$mapaWorld <- renderPlotly({
  #   get(paste0("map_World", nameMetric()))
  # })
  #

  output$mapaWorld <- renderPlotly({
    plot_ly(
      showlegend = FALSE,
      size = 8,
      line = list(
        color = rgb(0, 0, 0, maxColorValue = 256),
        width = 0.5
      )
    ) %>%
      layout(
        plot_bgcolor = rgb(39, 43, 48, maxColorValue = 256),
        paper_bgcolor = rgb(39, 43, 48, maxColorValue = 256),
        title = list(
          text = paste(tipoVar[[input$variable]]$descripcion, 
                       ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", "")),
          font = list(
            color = "#8B9BA8",
            size = 14
          )
        ),
        margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
      ) %>%
      add_sf(
        data = datMapaWorld(),
        split = ~name,
        color = ~Z_score,
        colors = c(palette$mint, palette$white, palette$red),
        alpha = 0.8,
        hoveron = "fills",
        hoverinfo = "text",
        text = ~ paste0(
          name, ": ",
          round(variable, 2),
          ifelse(input$tasas & !grepl("rat", input$variable), " x 1e5 hab", "")
        ),
        hoveron = "fills",
        hoverinfo = "text"
      )
  })



  # observeEvent(input$updateMapa, {
  #   plotlyProxy("mapaWorld", session) %>%
  #     plotlyProxyInvoke(
  #       "update",
  #       list(
  #         data = datMapaWorld(),
  #         split = ~name,
  #         color = ~Z_score,
  #         colors = c(palette$mint, palette$white, palette$red),
  #         alpha = 0.8,
  #         hoveron = "fills",
  #         hoverinfo = "text",
  #         text = ~ paste0(
  #           name, ": ",
  #           round(variable, 2),
  #           ifelse(tasas & !grepl("rat", variable), " x 1e5 hab", "")
  #         ),
  #         hoveron = "fills",
  #         hoverinfo = "text"
  #       )
  #     )
  # })
  #----------------------------------------------------------------------------


  # Mapas comunidades
  #----------------------------------------------------------------------------
  output$mapaCom <- renderPlotly({
    plot_ly(
      showlegend = FALSE,
      size = 8,
      line = list(
        color = rgb(1, 1, 1, maxColorValue = 256),
        width = 0.5
      )
    ) %>%
      layout(
        plot_bgcolor = rgb(39, 43, 48, maxColorValue = 256),
        paper_bgcolor = rgb(39, 43, 48, maxColorValue = 256),
        title = list(
          text = paste(tipoVar[[input$variable]]$descripcion, 
                       ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", "")),
          font = list(
            color = "#8B9BA8",
            size = 14
          )
        ),
        margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
      ) %>%
      add_sf(
        data = datMapaComunidades(),
        split = ~name,
        color = ~Z_score,
        colors = c(palette$mint, palette$white, palette$red),
        alpha = 0.8,
        hoveron = "fills",
        hoverinfo = "text",
        text = ~ paste0(
          name, ": ",
          round(variable, 2),
          ifelse(input$tasas & !grepl("rat", input$variable), " x 1e5 hab", "")
        ),
        hoveron = "fills",
        hoverinfo = "text"
      )
  })



  observeEvent(input$update, {
    plotlyProxy("mapaCom", session) %>%
      plotlyProxyInvoke(
        "update",
        list(
          data = datMapaComunidades(),
          split = ~name,
          color = ~Z_score,
          colors = c(palette$mint, palette$white, palette$red),
          alpha = 0.8,
          hoveron = "fills",
          hoverinfo = "text",
          text = ~ paste0(
            name, ": ",
            round(variable, 2),
            ifelse(tasas & !grepl("rat", variable), " x 1e5 hab", "")
          ),
          hoveron = "fills",
          hoverinfo = "text"
        )
      )
  })
  #----------------------------------------------------------------------------


  # Mapas provincias
  #----------------------------------------------------------------------------
  output$mapaProv <- renderPlotly({
    plot_ly(
      showlegend = FALSE,
      size = 8,
      line = list(
        color = rgb(1, 1, 1, maxColorValue = 256),
        width = 0.5
      )
    ) %>%
      layout(
        plot_bgcolor = rgb(39, 43, 48, maxColorValue = 256),
        paper_bgcolor = rgb(39, 43, 48, maxColorValue = 256),
        title = list(
          text = paste(tipoVar[[input$variable]]$descripcion, 
                       ifelse(input$tasas & tipoVar[[input$variable]]$tipo == "numerico" , "x 1e5 hab.", "")),
          font = list(
            color = "#8B9BA8",
            size = 14
          )
        ),
        margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
      ) %>%
      add_sf(
        data = datMapaProvincias(),
        split = ~name,
        color = ~Z_score,
        colors = c(palette$mint, palette$white, palette$red),
        alpha = 0.8,
        hoveron = "fills",
        hoverinfo = "text",
        text = ~ paste0(
          name, ": ",
          round(variable, 2),
          ifelse(input$tasas & !grepl("rat", input$variable), " x 1e5 hab", "")
        ),
        hoveron = "fills",
        hoverinfo = "text"
      )
  })



  observeEvent(input$update, {
    plotlyProxy("mapaProv", session) %>%
      plotlyProxyInvoke(
        "update",
        list(
          data = datMapaProvincias(),
          split = ~name,
          color = ~Z_score,
          colors = c(palette$mint, palette$white, palette$red),
          alpha = 0.8,
          hoveron = "fills",
          hoverinfo = "text",
          text = ~ paste0(
            name, ": ",
            round(Z_score, 2),
            ifelse(tasas & !grepl("rat", variable), " x 1e5 hab", "")
          ),
          hoveron = "fills",
          hoverinfo = "text"
        )
      )
  })
  #----------------------------------------------------------------------------
}

# Run the application
shinyApp(ui = ui, server = server)
