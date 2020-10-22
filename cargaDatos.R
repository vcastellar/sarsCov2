library(COVID19)
library(dplyr)
library(zoo)
library(forecast)
library(purrr)
library(ggplot2)
library(plotly)

options(encoding="UTF-8")

source("customTheme.R")
# constantes
DIAS_PREDICT <- 28

# funcion de prediccion a 14 dias
predict.fun <- function(x, frequency) {
  # predicción confirmados diarios
  tryCatch(
    {
      y <- x$daily_confirmed
      y <- na.approx(y)
      y[y <= 0] <- 1e-8
      x.ts <- ts(data = log(y), frequency = frequency)
      x.ts[is.na(x.ts) | is.infinite(x.ts)] <- 0
      ar <- arima(x.ts, order = c(1, 1, 1), seasonal = c(0, 1, 0))

      fc_confirmed <- round(exp(forecast(ar, DIAS_PREDICT)$mean), 2)
    },
    error = function(e) {
      fc_confirmed <<- rep(NA, DIAS_PREDICT)
    }
  )

  # predicción fallecidos diarios
  tryCatch(
    {
      y <- x$daily_deaths
      y <- na.approx(y)
      y[is.na(y)] <- 0
      y[y <= 0] <- 1e-8
      x.ts <- ts(data = log(y), frequency = frequency)
      ar <- arima(x.ts, order = c(1, 1, 1), seasonal = c(0, 1, 0))
      fc_deaths <- round(exp(forecast(ar, DIAS_PREDICT)$mean), 2)
    },
    error = function(e) {
      fc_deaths <<- rep(NA, DIAS_PREDICT)
    }
  )


  fc.df <- data.frame(
    id = max(x$id),
    date = seq.Date(from = tail(x$date, 1), length.out = DIAS_PREDICT + 1, by = "day")[-1],
    administrative_area_level_1 = max(x$administrative_area_level_1),
    continent = max(x$continent),
    daily_confirmed = fc_confirmed,
    daily_deaths = fc_deaths,
    population = max(x$population)
  )
  return(fc.df)
}

pobComAut <- read.csv2("./data/poblacionCom.csv", fileEncoding = "LATIN1", stringsAsFactors = FALSE)
pobProv <- read.csv2("./data/poblacionProv.csv", fileEncoding = "LATIN1", stringsAsFactors = FALSE)

# datos agregados por paises
datosESP1 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv", 
                      na.strings = "", 
                      fileEncoding = "UTF-8-BOM",
                      stringsAsFactors = FALSE)
datosESP1 <- datosESP1 %>%
  mutate(
    id = countryterritoryCode,
    date = as.Date(paste0(year, "-", month, "-", day)),
    administrative_area_level_1 = countriesAndTerritories,
    continent = continentExp,
    daily_confirmed = cases,
    daily_deaths = deaths,
    population = popData2019
  ) %>%
  select(id, date, administrative_area_level_1, continent, daily_confirmed, daily_deaths, population) %>%
  arrange(id, date)


# no todos los paises tienen suficiente calidad de datos:
datosESP1 <-
  datosESP1 %>% filter(administrative_area_level_1 %in%
    (datosESP1 %>%
      group_by(administrative_area_level_1) %>%
      summarise(
        N = n(),
        n = sum(!is.na(daily_confirmed)),
        freq = n / N,
        sel = ifelse(freq < 0.15, FALSE, TRUE)
      ) %>%
      filter(sel == TRUE))$administrative_area_level_1)




datosESP1 <- datosESP1 %>%
  mutate(
    daily_confirmed = as.numeric(daily_confirmed),
    daily_deaths = as.numeric(daily_deaths)
  )

# calculo de predicciones de casos observados
pred <- datosESP1 %>%
  split(.$administrative_area_level_1) %>%
  map_dfr(~ predict.fun(x = ., frequency = 7)) %>%
  as.data.frame()
pred <- pred %>%
  mutate(pred = TRUE)
datosESP1 <- datosESP1 %>%
  mutate(pred = FALSE)
datosESP1 <- bind_rows(datosESP1, pred) %>%
  data.frame() %>%
  arrange(administrative_area_level_1, date)
datosESP1 <- datosESP1 %>%
  arrange(date) %>%
  group_by(administrative_area_level_1) %>%
  mutate(
    confirmed = cumsum(daily_confirmed),
    inc_14d = rollapplyr(daily_confirmed, width = 14, FUN = sum, fill = 0),
    rat_inc_14d = c(rep(0, 7), diff(log(inc_14d), lag = 7) + 1),
    deaths= cumsum(daily_deaths),
    inc_14d_deaths = rollapplyr(daily_deaths, width = 14, FUN = sum, fill = 0),
    rat_inc_14d_deaths = c(rep(0, 7), diff(log(inc_14d_deaths), lag = 7) + 1),
    rat_acum_confirmed_vs_deaths = round(100 * deaths / confirmed, 2)
  ) %>%
  arrange(administrative_area_level_1, date)


#------------------------------------------------------------------------------
# datos por comunidades
#------------------------------------------------------------------------------

datosESP2 <- covid19(country = "ESP", level = 2, verbose = TRUE)
# atención: eventualmente, mientras no se cambie la fuente de datos, o la actual
#           solucione el problema, como La Rioja no tiene datos actualizados
#           se pone los confirmados a NA
datosESP2$confirmed[datosESP2$administrative_area_level_2 == "La Rioja"] <- NA

datosESP2 <- datosESP2 %>%
  select(date, confirmed, administrative_area_level_2) %>%
  arrange(date) %>%
  group_by(administrative_area_level_2) %>%
  mutate(
    daily_confirmed = c(0, diff(confirmed)),
    avg_7d_daily_confirmed = rollapplyr(daily_confirmed, width = 7, FUN = mean, fill = 0),
    inc_14d = rollapplyr(daily_confirmed, width = 14, FUN = sum, fill = 0),
    rat_inc_14d = c(rep(0, 7), diff(log(inc_14d), lag = 7) + 1),
    pred = FALSE
  )
datosESP2 <- merge(x = datosESP2, y = pobComAut, by = "administrative_area_level_2") %>%
  arrange(administrative_area_level_2, date)





#------------------------------------------------------------------------------
# datos por provincias
#------------------------------------------------------------------------------

datosESP3 <- covid19(country = "ESP", level = 3, verbose = TRUE)
# atención: eventualmente, mientras no se cambie la fuente de datos, o la actual
#           solucione el problema, como La Rioja no tiene datos actualizados
#           se pone los confirmados a NA
datosESP3$confirmed[datosESP3$administrative_area_level_3 == "La Rioja"] <- NA

datosESP3 <- datosESP3 %>%
  group_by(administrative_area_level_3) %>%
  select(date, confirmed, administrative_area_level_3) %>%
  mutate(
    daily_confirmed = c(0, diff(confirmed)),
    avg_7d_daily_confirmed = rollapplyr(daily_confirmed, width = 7, FUN = mean, fill = 0),
    inc_14d = rollapplyr(daily_confirmed, width = 14, FUN = sum, fill = 0),
    rat_inc_14d = c(rep(0, 7), diff(log(inc_14d), lag = 7) + 1),
    pred = FALSE
  )
datosESP3 <- merge(x = datosESP3, y = pobProv, by = "administrative_area_level_3") %>%
  arrange(administrative_area_level_3, date)



listaPaises <- unique(datosESP1$administrative_area_level_1)
listaComunidades <- unique(datosESP2$administrative_area_level_2)
listaProvincias <- unique(datosESP3$administrative_area_level_3)

ISOcod <- read.csv("./data/ISOcod.csv")

save(DIAS_PREDICT, datosESP1, datosESP2, datosESP3,
  listaPaises, listaComunidades, listaProvincias,
  ISOcod,
  file = "./data/datosESP.RData"
)

source("cargaMapas.R")
