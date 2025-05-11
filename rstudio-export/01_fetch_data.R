
# 01_fetch_data.R – versão final corrigida

# Instalar e carregar pacotes necessários
# install.packages(c("httr", "jsonlite", "rvest", "readr", "dplyr"))
library(httr)
library(jsonlite)
library(rvest)
library(readr)
library(dplyr)

# Criar diretórios
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# 1. Obter dados da Wikipedia (sistemas de bike sharing)
url_bike_wiki <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
bike_html <- read_html(url_bike_wiki)
bike_table <- bike_html %>%
  html_node(xpath = "(//table[contains(@class, 'wikitable')])[1]") %>%
  html_table(fill = TRUE)
write_csv(bike_table, "data/raw/raw_bike_sharing_systems.csv")

# 2. Obter dados de cidades do mundo (fonte alternativa gratuita)
url_cities <- "https://people.sc.fsu.edu/~jburkardt/data/csv/cities.csv"
download.file(url_cities, destfile = "data/raw/raw_worldcities.csv", mode = "wb")

# 3. Obter previsão do tempo para 5 dias via API OpenWeather
api_key <- "6b392355de89496bf9c27c3605a72c3d"  # Substitui aqui pela tua chave pessoal

cities <- c("Seoul,KR", "New York,US", "Paris,FR", "Suzhou,CN", "London,GB")

weather_data <- lapply(cities, function(city) {
  url <- paste0(
    "https://api.openweathermap.org/data/2.5/forecast?q=",
    URLencode(city), "&appid=", api_key, "&units=metric"
  )
  res <- GET(url)
  json_txt <- content(res, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(fromJSON(json_txt, flatten = TRUE), error = function(e) NULL)
  
  if (!is.null(parsed) && parsed$cod == "200") {
    df <- parsed$list
    df$city <- sub(",.*$", "", city)
    return(df)
  } else {
    warning(paste("Erro ao processar:", city, "→", substr(json_txt, 1, 80)))
    return(NULL)
  }
})

weather_df <- bind_rows(weather_data)
write_csv(weather_df, "data/raw/raw_cities_weather_forecast.csv")

# 4. Obter dados históricos do sistema de Seul (nova URL funcional)
url_seoul_csv <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00560/SeoulBikeData.csv"
download.file(url_seoul_csv, destfile = "data/raw/SeoulBikeData.csv", mode = "wb")



cat("Recolha de dados concluída com sucesso.")