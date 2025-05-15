# 01b_fetch_data.R - Parte 2

# Carregar pacotes
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(httr, jsonlite, readr, dplyr, tidyr, purrr)

# 3. Obter previsão do tempo para 5 dias via API OpenWeather
# A chave deve ser armazenada num arquivo separado ou variável de ambiente
api_key_file <- "api_keys.txt"

if (file.exists(api_key_file)) {
  api_key <- readLines(api_key_file, n = 1)
} else {
  api_key <- "6b392355de89496bf9c27c3605a72c3d"  # Substituir pela chave real
  cat("⚠️ Usando chave API padrão. Recomenda-se criar um arquivo api_keys.txt\n")
}

# Cidades alvo para o projeto
cities <- c("Seoul,KR", "New York,US", "Paris,FR", "Suzhou,CN", "London,GB")

weather_data <- lapply(cities, function(city) {
  cat(paste0("Coletando dados meteorológicos para: ", city, "\n"))
  
  url <- paste0(
    "https://api.openweathermap.org/data/2.5/forecast?q=",
    URLencode(city), "&appid=", api_key, "&units=metric"
  )
  
  res <- RETRY("GET", url, times = 3, pause_min = 1, pause_base = 2)
  
  if (http_status(res)$category != "Success") {
    warning(paste("Erro ao obter dados para:", city, "-", http_status(res)$message))
    cat(paste0("ERRO ao coletar dados de ", city, ": ", http_status(res)$message, "\n"), 
        file = "logs/data_collection.log", append = TRUE)
    return(NULL)
  }
  
  json_txt <- content(res, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(fromJSON(json_txt, flatten = TRUE), error = function(e) NULL)
  
  if (!is.null(parsed) && parsed$cod == "200") {
    df <- parsed$list
    
    # Adicionar informações da cidade
    df$city <- sub(",.*$", "", city)
    df$country_code <- sub(".*,", "", city)
    df$lat <- parsed$city$coord$lat
    df$lon <- parsed$city$coord$lon
    
    cat(paste0("✓ Dados coletados para ", city, " (", nrow(df), " registros)\n"), 
        file = "logs/data_collection.log", append = TRUE)
    
    return(df)
  } else {
    warning(paste("Erro ao processar:", city, "→", substr(json_txt, 1, 80)))
    cat(paste0("ERRO ao processar dados de ", city, "\n"), 
        file = "logs/data_collection.log", append = TRUE)
    return(NULL)
  }
})

# Combinar todos os dados meteorológicos
weather_df <- bind_rows(weather_data)
write_csv(weather_df, "data/raw/raw_cities_weather_forecast.csv")

# 4. Obter dados históricos do sistema de Seul
url_seoul_csv <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00560/SeoulBikeData.csv"

tryCatch({
  download.file(url_seoul_csv, destfile = "data/raw/SeoulBikeData.csv", mode = "wb")
  cat("✓ Dados históricos de Seoul baixados com sucesso\n", 
      file = "logs/data_collection.log", append = TRUE)
}, error = function(e) {
  cat(paste0("ERRO na coleta de dados de Seoul: ", e$message, "\n"), 
      file = "logs/data_collection.log", append = TRUE)
  
  # URL alternativa caso a primeira falhe
  alt_url <- "https://raw.githubusercontent.com/datasets-br/bicycle-sharing-systems/master/data/SeoulBikeData.csv"
  tryCatch({
    download.file(alt_url, destfile = "data/raw/SeoulBikeData.csv", mode = "wb")
    cat("✓ Dados históricos de Seoul baixados da fonte alternativa\n", 
        file = "logs/data_collection.log", append = TRUE)
  }, error = function(e2) {
    cat(paste0("ERRO também na fonte alternativa: ", e2$message, "\n"), 
        file = "logs/data_collection.log", append = TRUE)
  })
})

cat(paste0("Recolha de dados concluída: ", Sys.time(), "\n"), 
    file = "logs/data_collection.log", append = TRUE)