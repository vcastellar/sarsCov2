#------------------------------------------------------------------------------
# Recopilación de datos por comunidades
#------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(openxlsx)


# listado de comunidades
#------------------------------------------------------------------------------
# unique(datosESP2$administrative_area_level_2)
# [1]  "Andalucía"                  "Aragón"                     "Canarias"                   "Cantabria"                 
# [5]  "Castilla y León"            "Castilla-La Mancha"         "Cataluña"                   "Ceuta"                     
# [9]  "Comunidad de Madrid"        "Comunidad Foral de Navarra" "Comunidad Valenciana"       "Extremadura"               
# [13] "Galicia"                    "Islas Baleares"             "La Rioja"                   "Melilla"                   
# [17] "País Vasco"                 "Principado de Asturias"     "Región de Murcia" 
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# unique(datosESP3$administrative_area_level_3)
# lista de provincias
# [1]  "Álava"                  "Albacete"               "Alicante"               "Almería"               
# [5]  "Asturias"               "Ávila"                  "Badajoz"                "Baleares"              
# [9]  "Barcelona"              "Burgos"                 "Cáceres"                "Cádiz"                 
# [13] "Cantabria"              "Castellón"              "Ceuta"                  "Ciudad Real"           
# [17] "Córdoba"                "Cuenca"                 "Gerona"                 "Granada"               
# [21] "Guadalajara"            "Guipúzcoa"              "Huelva"                 "Huesca"                
# [25] "Jaén"                   "La Coruña"              "La Rioja"               "Las Palmas"            
# [29] "León"                   "Lérida"                 "Lugo"                   "Madrid"                
# [33] "Málaga"                 "Melilla"                "Murcia"                 "Navarra"               
# [37] "Orense"                 "Palencia"               "Pontevedra"             "Salamanca"             
# [41] "Santa Cruz de Tenerife" "Segovia"                "Sevilla"                "Soria"                 
# [45] "Tarragona"              "Teruel"                 "Toledo"                 "Valencia"              
# [49] "Valladolid"             "Vizcaya"                "Zamora"                 "Zaragoza"  
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# COMUNIDADES
#------------------------------------------------------------------------------

# defunciones
#------------------------------------------------------------------------------

comDefunciones <- read.xlsx("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Fallecidos_COVID19.xlsx",
                            detectDates = TRUE)
detectDates <- nchar(comDefunciones[,1]) == 10
comDefunciones <- comDefunciones[detectDates, ] %>% 
  rename(date = 'Fecha./.CCAA',
         "Principado de Asturias" = "Asturias",
         "Islas Baleares" = "Baleares",
         "Castilla-La Mancha" = "Castilla.La.Mancha",
         "Castilla y León" = "Castilla.y.León",
         "Comunidad Valenciana" = "C..Valenciana",
         "Comunidad de Madrid" = "Madrid",
         "Comunidad Foral de Navarra" = "Navarra", 
         "País Vasco" = "País.Vasco",
         "La Rioja" = "La.Rioja",
         "Región de Murcia" = "Murcia")
comDefunciones$España <- NULL
comDefunciones <- comDefunciones %>% 
  pivot_longer(cols = c("Andalucía", "Aragón", "Canarias", "Cantabria",                
                        "Castilla y León", "Castilla-La Mancha", "Cataluña", "Ceuta",                    
                        "Comunidad de Madrid", "Comunidad Foral de Navarra", "Comunidad Valenciana", "Extremadura",               
                        "Galicia", "Islas Baleares", "La Rioja", "Melilla",                   
                        "País Vasco", "Principado de Asturias", "Región de Murcia" )
               ) %>% 
  rename(administrative_area_level_2 = name,
         daily_deaths = value) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(administrative_area_level_2, date) %>% 
  group_by(administrative_area_level_2) %>% 
  mutate(deaths = cumsum(daily_deaths))
  