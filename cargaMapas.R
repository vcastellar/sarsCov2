# fichero que carga los mapas
# https://gadm.org/data.html


library("rnaturalearth")
library("sf")
library("scales")

options(encoding="UTF-8")

# función empleada en las transiciones del gradiente de colores en los mapas
norm_trans <- function(){
  boxcox_trans(p = 2, offset = 0)
}
FUN_TRANS <- "sqrt"



#------------------------------------------------------------------------------
# mapa del mundo
#------------------------------------------------------------------------------
kk <- datosESP1 %>%
  filter(pred == FALSE) %>%
  filter(!is.na(confirmed)) %>%
  group_by(id) %>%
  arrange(date) %>%
  summarise(
    date = max(date),
    confirmed = tail(confirmed, 1),
    population = tail(population, 1),
    inc_14d = tail(inc_14d, 1),
    inc_14by100hab = inc_14d / population * 1e5,
    rat_inc_14d = tail(rat_inc_14d, 1),
    deaths = tail(deaths, 1),
    tasa_deaths = round(deaths / population * 1e5, 2),
    inc_14d_deaths = tail(inc_14d_deaths, 1),
    rat_inc_14d_deaths = tail(rat_inc_14d_deaths, 1),
    inc_14by100hab_deaths = inc_14d_deaths / population * 1e5,
    rat_acum_confirmed_vs_deaths = tail(rat_acum_confirmed_vs_deaths, 1)
  )
world <- ne_countries(returnclass = "sf")

world <- merge(x = world, y = kk, by.x = "adm0_a3_is", by.y = "id")

# mapa mundial tasas de casos acumulados últimos 14 días
# pWorldInc_14d <- ggplot(data = world, aes(text = paste("Contry:", name, "<br>", "IA x 1e5 hab.:", round(inc_14by100hab)))) +
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

# experimento mapa con plotly
pWorldInc_14d <-
  plot_ly(world,  
          split = ~name,
          color = ~sqrt(abs(inc_14by100hab)),
          colors = c("#21D19F", "#FC3C0C"),
          alpha = 0.8,
          showlegend = FALSE,
          size = 8,
          line = list(
            color = rgb(1, 1, 1, maxColorValue = 256),
            width = 0.5),
          text = ~paste(name, ": ", round(inc_14by100hab, 2), " x 1e5 hab"),
          hoveron = "fills",
          hoverinfo = "text",
          width = 1200
  ) %>%
  layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
         paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
         title = list(text = paste("Inicidencia acumulada últimos 14 días por 100.000 hab."),
                      font = list(color = "#8B9BA8",
                                  size = 14)),
         margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
  )

pWorldInc_14d <- hide_colorbar(pWorldInc_14d)

# mapa mundial razón de tasas muertes últimos 14 días
pWorldrat_inc_14d <-
  plot_ly(world %>% filter(is.finite(rat_inc_14d)),  
          split = ~name,
          color = ~sqrt(abs(rat_inc_14d)),
          colors = c("#21D19F", "#FC3C0C"),
          alpha = 0.8,
          showlegend = FALSE,
          size = 8,
          line = list(
            color = rgb(1, 1, 1, maxColorValue = 256),
            width = 0.5),
          text = ~paste(name, ": ", round(rat_inc_14d, 2)),
          hoveron = "fills",
          hoverinfo = "text",
          width = 1200
  ) %>%
  layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
         paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
         title = list(text = paste("razón de tasas de casos diagnosticasos últimos 14 días"),
                      font = list(color = "#8B9BA8",
                                  size = 14)),
         margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
  )

pWorldrat_inc_14d <- hide_colorbar(pWorldrat_inc_14d)




# mapa mundial tasa muertes últimos 14 días
pWorldrat_Inc_14d_fallecidos <-
  plot_ly(world,  
          split = ~name,
          color = ~sqrt(inc_14by100hab_deaths),
          colors = c("#21D19F", "#FC3C0C"),
          alpha = 0.8,
          showlegend = FALSE,
          size = 8,
          line = list(
            color = rgb(1, 1, 1, maxColorValue = 256),
            width = 0.5),
          text = ~paste(name, ": ", round(inc_14by100hab_deaths, 2), " x 1e5 hab"),
          hoveron = "fills",
          hoverinfo = "text",
          width = 1200
  ) %>%
  layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
         paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
         title = list(text = paste("Inicidencia acumulada fallecidos últimos 14 días por 100.000 hab."),
                      font = list(color = "#8B9BA8",
                                  size = 14)),
         margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
  )

pWorldrat_Inc_14d_fallecidos <- hide_colorbar(pWorldrat_Inc_14d_fallecidos)

# mapa mundial tasa muertes últimos 14 días
pWorldInc_14d_fallecidos <-
  plot_ly(world,  
          split = ~name,
          color = ~sqrt(inc_14d_deaths),
          colors = c("#21D19F", "#FC3C0C"),
          alpha = 0.8,
          showlegend = FALSE,
          size = 8,
          line = list(
            color = rgb(1, 1, 1, maxColorValue = 256),
            width = 0.5),
          text = ~paste(name, ": ", round(inc_14by100hab_deaths, 2)),
          hoveron = "fills",
          hoverinfo = "text",
          width = 1200
  ) %>%
  layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
         paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
         title = list(text = paste("Inicidencia acumulada fallecidos últimos 14 días"),
                      font = list(color = "#8B9BA8",
                                  size = 14)),
         margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
  )

pWorldInc_14d_fallecidos <- hide_colorbar(pWorldInc_14d_fallecidos)


# mapa mundial casos totales
pWorldcasos <-
  plot_ly(world,  
          split = ~name,
          color = ~sqrt(confirmed),
          colors = c("#21D19F", "#FC3C0C"),
          alpha = 0.8,
          showlegend = FALSE,
          size = 8,
          line = list(
            color = rgb(1, 1, 1, maxColorValue = 256),
            width = 0.5),
          text = ~paste(name, ": ", round(confirmed, 2), " x 1e5 hab"),
          hoveron = "fills",
          hoverinfo = "text",
          width = 1200
  ) %>%
  layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
         paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
         title = list(text = paste("total casos"),
                      font = list(color = "#8B9BA8",
                                  size = 14)),
         margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
  )

pWorldcasos <- hide_colorbar(pWorldcasos)

# mapa mundial total fallecidos
pWorldfallecidos <-
  plot_ly(world,  
          split = ~name,
          color = ~sqrt(deaths),
          colors = c("#21D19F", "#FC3C0C"),
          alpha = 0.8,
          showlegend = FALSE,
          size = 8,
          line = list(
            color = rgb(1, 1, 1, maxColorValue = 256),
            width = 0.5),
          text = ~paste(name, ": ", round(deaths, 2)),
          hoveron = "fills",
          hoverinfo = "text",
          width = 1200
  ) %>%
  layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
         paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
         title = list(text = paste("total fallecidos."),
                      font = list(color = "#8B9BA8",
                                  size = 14)),
         margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
  )

pWorldfallecidos <- hide_colorbar(pWorldfallecidos)

# mapa mundial tasa de fallecidos
pWorldTasafallecidos <-
  plot_ly(world,  
          split = ~name,
          color = ~sqrt(inc_14by100hab_deaths),
          colors = c("#21D19F", "#FC3C0C"),
          alpha = 0.8,
          showlegend = FALSE,
          size = 8,
          line = list(
            color = rgb(1, 1, 1, maxColorValue = 256),
            width = 0.5),
          text = ~paste(name, ": ", round(inc_14by100hab_deaths, 2), " x 1e5 hab"),
          hoveron = "fills",
          hoverinfo = "text",
          width = 1200
  ) %>%
  layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
         paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
         title = list(text = paste("tasa fallecidos."),
                      font = list(color = "#8B9BA8",
                                  size = 14)),
         margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
  )

pWorldTasafallecidos <- hide_colorbar(pWorldTasafallecidos)

# mapa mundial rat_acum_confirmed_vs_deaths
pWorldrat_acum_confirmed_vs_deaths <-
  plot_ly(world,  
          split = ~name,
          color = ~sqrt(rat_acum_confirmed_vs_deaths),
          colors = c("#21D19F", "#FC3C0C"),
          alpha = 0.8,
          showlegend = FALSE,
          size = 8,
          line = list(
            color = rgb(1, 1, 1, maxColorValue = 256),
            width = 0.5),
          text = ~paste(name, ": ", round(rat_acum_confirmed_vs_deaths, 2)),
          hoveron = "fills",
          hoverinfo = "text",
          width = 1200
  ) %>%
  layout(plot_bgcolor = rgb(39,43,48, maxColorValue = 256),
         paper_bgcolor = rgb(39,43,48, maxColorValue = 256),
         title = list(text = paste("razón casos vs fallecidos fallecidos."),
                      font = list(color = "#8B9BA8",
                                  size = 14)),
         margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0)
  )

pWorldrat_acum_confirmed_vs_deaths <- hide_colorbar(pWorldrat_acum_confirmed_vs_deaths)




#------------------------------------------------------------------------------
# mapa de comunidades
#------------------------------------------------------------------------------

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



save(pMapEsp_2Inc_14d, 
     pWorldInc_14d, pWorldTasafallecidos, pWorldfallecidos, pWorldcasos, 
     pWorldInc_14d_fallecidos, pWorldrat_Inc_14d_fallecidos, pWorldrat_acum_confirmed_vs_deaths, 
     pWorldrat_inc_14d, 
     pMapEsp_3Inc_14d, file = "./data/mapas.RData")

