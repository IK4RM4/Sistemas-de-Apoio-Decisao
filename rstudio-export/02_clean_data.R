
# 02_clean_data.R

# Instalar e carregar pacotes necessários
# install.packages(c("tidyverse", "janitor", "stringr", "lubridate"))
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)

# Criar diretório de saída
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# 1. Carregar dados
bike_raw <- read_csv("data/raw/raw_bike_sharing_systems.csv")
weather_raw <- read_csv("data/raw/raw_cities_weather_forecast.csv")
world_cities <- read_csv("data/raw/raw_worldcities.csv")
seoul_bike <- read_csv("data/raw/SeoulBikeData.csv", locale = locale(encoding = "ISO-8859-1"))

# 2. Limpar e normalizar os dados
bike_clean <- bike_raw %>%
  clean_names() %>%
  mutate(across(everything(), ~str_squish(as.character(.)))) %>%
  mutate(across(everything(), ~str_remove_all(., "<.*?>"))) # remover HTML tags

seoul_bike_clean <- seoul_bike %>%
  clean_names() %>%
  mutate(date = dmy(date),
         hour = as.factor(hour),
         seasons = as.factor(seasons),
         holiday = as.factor(holiday),
         functioning_day = as.factor(functioning_day))

# Corrigir nomes de colunas no weather
weather_clean <- weather_raw %>%
  clean_names() %>%
  mutate(dt_txt = suppressWarnings(ymd_hms(dt_txt))) %>%  # Silenciar o warning
  filter(!is.na(dt_txt))  # Remover as 25 linhas mal formatadas


# Corrigir world cities
cities_clean <- world_cities %>%
  clean_names() %>%
  mutate(city = str_to_title(city))

# 3. Guardar ficheiros limpos
write_csv(bike_clean, "data/processed/bike_sharing_systems.csv")
write_csv(seoul_bike_clean, "data/processed/seoul_bike_sharing.csv")
write_csv(weather_clean, "data/processed/weather_forecast.csv")
write_csv(cities_clean, "data/processed/world_cities.csv")

cat("Limpeza de dados concluída.")
