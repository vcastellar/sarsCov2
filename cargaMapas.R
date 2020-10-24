# fichero que carga los mapas
# https://gadm.org/data.html


library("rnaturalearth")
library("sf")
library("scales")
library("jtrans")
options(encoding="UTF-8")


# en un primer momento se optó por pintar los mapas con ggplot2 y luego
# convertirlos a plotly:
# mapa mundial tasas de casos acumulados últimos 14 días
# pWorldInc_14d <- ggplot(data = world, aes(text = paste("Contry:", name, "<br>", 
#                                                        "IA x 1e5 hab.:", round(inc_14by100hab)))) +
#   geom_sf(aes(fill = inc_14by100hab)) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   ggtitle("Inicidencia acumulada últimos 14 días por 100.000 hab.",
#     subtitle = paste0("(", length(unique(world$sovereignt)), " países) fecha de actualización: ", max(world$date))
#   ) +
#   custom_theme +
#   theme(panel.grid.major = element_blank()) +
#   scale_fill_gradientn(trans = FUN_TRANS, colours = custom_palette[c(9, 5)])
# pWorldInc_14d <- ggplotly(pWorldInc_14d, width = 1200)


# se define la siguiente función con la que pintar mapas con plotly
#------------------------------------------------------------------------------
mapPlot <- function(x, variable, tasas) {
  dat <- x %>% 
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
  
  dat <- dat %>% 
    filter(is.finite(Z_score))
  
  mapaWorld <-
    plot_ly(dat,  
            split = ~name,
            color = ~Z_score,
            colors = c("#21D19F", "#FFFFFF", "#FC3C0C"),
            alpha = 0.8,
            showlegend = FALSE,
            size = 8,
            line = list(
              color = rgb(1, 1, 1, maxColorValue = 256),
              width = 0.5),
            text = ~paste0(name, ": ", 
                           round(variable, 2),
                           ifelse(tasas & !grepl("rat", variable), " x 1e5 hab", "")),
            hoveron = "fills",
            hoverinfo = "text",
            width = 1200
    ) %>%
    layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
           paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
           title = list(text = paste(variable),
                        font = list(color = "#8B9BA8",
                                    size = 14)),
           margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
    )
  
  plotly_build(mapaWorld)
  # si queremos ocultar la barra de color: hide_colorbar()
}
#------------------------------------------------------------------------------




#------------------------------------------------------------------------------
# parámetros para pintar los mapas: variables x tasas
parm <- expand.grid(
  variable = c("confirmed",
               "daily_confirmed",
               "inc_14d",
               "rat_inc_14d",
               "deaths",
               "daily_deaths",
               "inc_14d_deaths",
               "rat_inc_14d_deaths",
               "rat_acum_confirmed_vs_deaths",
               "rat_inc_14d_acum_confirmed_vs_deaths"),
  tasas = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# mapas del mundo
#------------------------------------------------------------------------------


for (i in 1:nrow(parm)) {
  variable <- parm$variable[i]
  tasas    <- parm$tasas[i]
  mapName  <- paste0("map_World", variable, ifelse(tasas, "Rat", ""))
  
  assign(mapName, mapPlot(x = world, variable = variable, tasas = tasas))
  
}
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# mapa de comunidades
#------------------------------------------------------------------------------
for (i in 1:2) {
  variable <- parm$variable[i]
  tasas    <- parm$tasas[i]
  mapName  <- paste0("map_Com", variable, ifelse(tasas, "Rat", ""))
  if (variable %in% names(mapComunidades)) {
    assign(mapName, mapPlot(x = mapComunidades, variable = variable, tasas = tasas))
  }
  
}

#------------------------------------------------------------------------------
# mapa de provincias
#------------------------------------------------------------------------------
# for (i in 1:2) {
#   variable <- parm$variable[i]
#   tasas    <- parm$tasas[i]
#   mapName  <- paste0("map_Prov", variable, ifelse(tasas, "Rat", ""))
#   if (variable %in% names(mapComunidades)) {
#     assign(mapName, mapPlot(x = mapProvincias, variable = variable, tasas = tasas))
#   }
#   
# }


save(list = ls(pattern = "map_"),file = "./data/mapas.RData")

