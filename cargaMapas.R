# fichero que carga los mapas
# https://gadm.org/data.html


library("rnaturalearth")
library("sf")
library("scales")
library("tidyr")

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
    select(name, variable, population, geometry)
  
  names(dat) <- c("name", "variable", "population", "geometry")
  dat[is.na(dat)] <- 0
  
  dat <- dat %>% 
    mutate(variable_col =   
      if (grepl("rat", variable)) {
        sqrt(abs(variable)) 
      } else if (tasas) {
        sqrt(abs(variable) / population * 1e5)
      } else {
        sqrt(abs(variable ))
        
      }
    , variable = 
      if (grepl("rat", variable)) {
        variable
      } else if (tasas) {
        variable / population * 1e5
      } else {
        variable
      }
    
    ) %>% 
    filter(is.finite(variable_col))


  
  mapaWorld <-
    plot_ly(dat,  
            split = ~name,
            color = ~variable_col,
            colors = c("#21D19F", "#FC3C0C"),
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
  
  plotly_build(hide_colorbar(mapaWorld))
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
               "rat_acum_confirmed_vs_deaths"),
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
FUN_TRANS <- "sqrt"
mapEsp_2 <- readRDS("./data/gadm36_ESP_1_sf.rds")

kk <- datosESP2 %>%
  filter(date == max(date)) %>%
  mutate(inc_14by100hab = inc_14d / population * 100000)
mapEsp_2 <- merge(x = mapEsp_2, y = kk, by.x = "NAME_1", by.y = "administrative_area_level_2")

pMapEsp_2Inc_14d <- ggplot(
  data = mapEsp_2,
  aes(text = paste("C.A.::", NAME_1, "<br>", "IA x 1e5 hab.:", round(inc_14by100hab)))
) +
  geom_sf(aes(fill = inc_14by100hab)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Inicidencia acumulada últimos 14 días por 100.000 hab.",
    subtitle = paste0("(", length(unique(mapEsp_2$NAME_1)), " Comunidades autónomas)")
  ) +
  scale_fill_gradientn(trans = FUN_TRANS, colours = custom_palette[c(9, 5)]) +
  custom_theme +
  theme(panel.grid.major = element_blank())

 pMapEsp_2Inc_14d <- ggplotly(pMapEsp_2Inc_14d, width = 1200)

#------------------------------------------------------------------------------
# mapa de provincias
#------------------------------------------------------------------------------
mapEsp_3 <- readRDS("./data/gadm36_ESP_2_sf.rds")
mapEsp_3 <- mapEsp_3 %>% mutate(
  NAME_2 = gsub("Lleida", "Lérida", NAME_2),
  NAME_2 = gsub("A Coruña", "La Coruña", NAME_2),
  NAME_2 = gsub("Girona", "Gerona", NAME_2),
  NAME_2 = gsub("Ourense", "Orense", NAME_2)
)
mapEsp_3 <- mapEsp_3 %>% filter(NAME_1 != "Islas Canarias")

kk <- datosESP3 %>%
  filter(date == max(date)) %>%
  mutate(inc_14by100hab = inc_14d / population * 100000)
mapEsp_3 <- merge(x = mapEsp_3, y = kk, by.x = "NAME_2", by.y = "administrative_area_level_3")

pMapEsp_3Inc_14d <- ggplot(
  data = mapEsp_3,
  aes(text = paste("Provincia.::", NAME_2, "<br>", "IA x 1e5 hab.:", round(inc_14by100hab)))
) +
  geom_sf(aes(fill = inc_14by100hab)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Inicidencia acumulada últimos 14 días por 100.000 hab.",
    subtitle = paste0("(", length(unique(mapEsp_3$NAME_2)), " Provincias)")
  ) +
  scale_fill_gradientn(trans = FUN_TRANS, colours = custom_palette[c(9, 5)]) +
  custom_theme +
  theme(panel.grid.major = element_blank())

 pMapEsp_3Inc_14d <- ggplotly(pMapEsp_3Inc_14d, width = 1200)



save(list = ls(pattern = "map_"),
  pMapEsp_2Inc_14d, 
     pMapEsp_3Inc_14d, file = "./data/mapas.RData")

