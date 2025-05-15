# 02_clean_data.R - versão simplificada

# Carregar pacotes
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)

# Criar diretório para dados processados
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# 1. Carregar dados
bike_raw <- read_csv("data/raw/raw_bike_sharing_systems.csv", show_col_types = FALSE)
weather_raw <- read_csv("data/raw/raw_cities_weather_forecast.csv", show_col_types = FALSE)
world_cities <- read_csv("data/raw/raw_worldcities.csv", show_col_types = FALSE)
seoul_bike <- read_csv("data/raw/SeoulBikeData.csv", locale = locale(encoding = "ISO-8859-1"), 
                       show_col_types = FALSE)

# 2. Limpar dados de partilha de bicicletas
bike_clean <- bike_raw %>%
  clean_names() %>%
  mutate(across(where(is.character), str_squish)) %>%
  mutate(across(where(is.character), ~str_remove_all(., "<.*?>"))) %>%
  mutate(
    bicycles = parse_number(str_extract(system, "\\d[\\d,]*\\s*(bicycles|bikes)")),
    stations = parse_number(str_extract(system, "\\d[\\d,]*\\s*(stations)")),
    system_clean = str_remove_all(system, "\\[\\d+\\]")
  )

# 3. Limpar dados de Seoul
seoul_bike_clean <- seoul_bike %>%
  clean_names() %>%
  mutate(
    date = dmy(date),
    hour = as.factor(hour),
    seasons = as.factor(seasons),
    holiday = as.factor(holiday),
    functioning_day = as.factor(functioning_day)
  )

# 4. Limpar dados meteorológicos
weather_clean <- weather_raw %>%
  clean_names() %>%
  mutate(
    dt_txt = ymd_hms(dt_txt),
    date = as_date(dt_txt),
    hour = hour(dt_txt),
    temperature_c = main_temp,
    humidity_percent = main_humidity,
    wind_speed_m_s = wind_speed,
    visibility_10m = ifelse(is.na(visibility), 0, visibility / 10),
    dew_point_temperature_c = main_temp - ((100 - main_humidity) / 5),
    solar_radiation_mj_m2 = 0
  )

# Adicionar colunas rainfall_mm e snowfall_cm de forma segura
if ("rain_3h" %in% colnames(weather_clean)) {
  weather_clean <- weather_clean %>%
    mutate(rainfall_mm = ifelse(is.na(rain_3h), 0, rain_3h))
} else {
  weather_clean <- weather_clean %>%
    mutate(rainfall_mm = 0)
}

if ("snow_3h" %in% colnames(weather_clean)) {
  weather_clean <- weather_clean %>%
    mutate(snowfall_cm = ifelse(is.na(snow_3h), 0, snow_3h))
} else {
  weather_clean <- weather_clean %>%
    mutate(snowfall_cm = 0)
}

# 5. Corrigir world_cities
cities_clean <- world_cities %>%
  clean_names()

# 6. Guardar ficheiros limpos
write_csv(bike_clean, "data/processed/bike_sharing_systems.csv")
write_csv(seoul_bike_clean, "data/processed/seoul_bike_sharing.csv")
write_csv(weather_clean, "data/processed/weather_forecast.csv")
write_csv(cities_clean, "data/processed/world_cities.csv")

cat("Limpeza de dados concluída.\n")