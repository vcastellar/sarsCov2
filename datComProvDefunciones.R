#------------------------------------------------------------------------------
# Recopilación de datos por comunidades
#------------------------------------------------------------------------------
library(tidyr)
library(dplyr)


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
# COMUNIDAD VALENCIANA
#------------------------------------------------------------------------------

# defunciones
#------------------------------------------------------------------------------

CV_defunciones <- 
  read.csv("https://dadesobertes.gva.es/ca/datastore/dump/69c32771-3d18-4654-8c3c-cb423fcfa652?bom=True") 
CV_defunciones <- CV_defunciones %>% 
  select(names(CV_defunciones)[!grepl("X_id|Homes|Dones|DEPARTAMENT", names(CV_defunciones))]) 
names(CV_defunciones) <- c("date", "Comunidad Valenciana", "Alicante", "Castellón", "Valencia")
CV_defunciones <- CV_defunciones %>% 
  pivot_longer(cols = c("Comunidad Valenciana", "Alicante", "Castellón", "Valencia"))

CV <- CV_defunciones %>%  
  filter(name == "Comunidad Valenciana") %>% 
  rename(administrative_area_level_2 = name,
         daily_deaths = value) %>% 
  group_by(administrative_area_level_2) %>% 
  mutate(date = as.Date(substr(date, 1, 10)),
         deaths = cumsum(daily_deaths)) 


CV_prov <- CV_defunciones %>%  
  filter(name != "Comunidad Valenciana") %>% 
  rename(administrative_area_level_3 = name,
         daily_deaths = value) %>% 
  group_by(administrative_area_level_3) %>% 
  mutate(date = as.Date(substr(date, 1, 10)),
         deaths = cumsum(daily_deaths))


# juntamos los datos de todas las comunidades y provincias
comDefunciones <- CV
provDefunciones <- CV_prov