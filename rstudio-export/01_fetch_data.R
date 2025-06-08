# 01_fetch_data_improved.R - Versão melhorada com consistência de dados

library(httr)
library(jsonlite)
library(rvest)
library(readr)
library(dplyr)
library(stringr)

# Criar diretórios
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# === DEFINIR ESTRUTURA PADRÃO DE DADOS ===

# Estrutura padrão para sistemas de bike sharing
create_standard_bike_data <- function(city, country, system, stations, bicycles, 
                                      operator, launch_year, website, source) {
  data.frame(
    City = as.character(city),
    Country = as.character(country),
    System = as.character(system),
    Stations = as.numeric(stations),
    Bicycles = as.numeric(bicycles),
    Operator = as.character(operator),
    Launch_Year = as.numeric(launch_year),
    Website = as.character(website),
    Source = as.character(source),
    stringsAsFactors = FALSE
  )
}

# Dicionário de coordenadas (pode ser expandido ou vir de uma API de geocoding)
CITY_COORDINATES <- list(
  "Seoul" = list(lat = 37.5665, lon = 126.9780),
  "New York" = list(lat = 40.7128, lon = -74.0060),
  "Paris" = list(lat = 48.8566, lon = 2.3522),
  "London" = list(lat = 51.5074, lon = -0.1278),
  "Barcelona" = list(lat = 41.3851, lon = 2.1734),
  "Berlin" = list(lat = 52.5200, lon = 13.4050),
  "Montreal" = list(lat = 45.5017, lon = -73.5673),
  "Washington DC" = list(lat = 38.9072, lon = -77.0369),
  "Chicago" = list(lat = 41.8781, lon = -87.6298),
  "Toronto" = list(lat = 43.6532, lon = -79.3832),
  "Boston" = list(lat = 42.3601, lon = -71.0589),
  "San Francisco" = list(lat = 37.7749, lon = -122.4194),
  "Amsterdam" = list(lat = 52.3676, lon = 4.9041),
  "Copenhagen" = list(lat = 55.6761, lon = 12.5683),
  "Stockholm" = list(lat = 59.3293, lon = 18.0686)
)

# === FUNÇÕES DE RECOLHA MELHORADAS ===

# Função para obter dados do Citi Bike (NYC) com estrutura padronizada
get_citibike_data <- function() {
  cat("Obtendo dados do Citi Bike (Nova York)...\n")
  tryCatch({
    url <- "https://gbfs.citibikenyc.com/gbfs/en/station_information.json"
    response <- GET(url, timeout(10))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      stations <- data$data$stations
      
      # Usar estrutura padronizada
      result <- create_standard_bike_data(
        city = "New York",
        country = "United States",
        system = "Citi Bike",
        stations = nrow(stations),
        bicycles = nrow(stations) * 12, # Estimativa: 12 bikes por estação
        operator = "Motivate",
        launch_year = 2013,
        website = "https://citibikenyc.com",
        source = "Real API"
      )
      
      return(result)
    } else {
      warning(paste("API Citi Bike retornou status:", status_code(response)))
      return(NULL)
    }
  }, error = function(e) {
    cat("  ⚠️ Erro Citi Bike:", e$message, "\n")
    return(NULL)
  })
}

# Função generalizada para APIs GBFS
get_gbfs_data <- function(url, city, country, system, operator, launch_year, website) {
  cat(paste("Obtendo dados de", system, "...\n"))
  tryCatch({
    response <- GET(url, timeout(10))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      stations <- data$data$stations
      
      # Calcular estimativa de bicicletas baseada no tamanho da cidade
      bike_multiplier <- case_when(
        city %in% c("New York", "London", "Paris") ~ 15,  # Cidades grandes
        city %in% c("Washington DC", "Chicago", "Montreal") ~ 12,  # Cidades médias
        TRUE ~ 10  # Outras cidades
      )
      
      result <- create_standard_bike_data(
        city = city,
        country = country,
        system = system,
        stations = nrow(stations),
        bicycles = nrow(stations) * bike_multiplier,
        operator = operator,
        launch_year = launch_year,
        website = website,
        source = "Real API"
      )
      
      return(result)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    cat("  ⚠️ Erro", system, ":", e$message, "\n")
    return(NULL)
  })
}

# Função para obter dados da Wikipedia com parsing melhorado
get_wikipedia_bike_data <- function() {
  cat("Tentando obter dados da Wikipedia...\n")
  tryCatch({
    url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
    page <- read_html(url)
    
    tables <- page %>% html_nodes("table.wikitable")
    
    if (length(tables) > 0) {
      # Processar a primeira tabela útil
      table_data <- tables[[1]] %>% html_table(fill = TRUE)
      
      # Padronizar nomes de colunas
      names(table_data) <- c("System", "Location", "Country", "Launched", 
                             "Stations", "Bicycles", "Operator")
      
      # Limpar e padronizar dados
      cleaned_data <- table_data %>%
        slice(-1) %>%  # Remover header
        mutate(
          City = str_extract(Location, "^[^,]+"),
          Stations = parse_number(Stations),
          Bicycles = parse_number(Bicycles),
          Launch_Year = parse_number(Launched),
          Website = "N/A",
          Source = "Wikipedia"
        ) %>%
        select(City, Country, System, Stations, Bicycles, Operator, 
               Launch_Year, Website, Source) %>%
        filter(!is.na(City), !is.na(Stations))
      
      cat("  ✓ Dados da Wikipedia processados:", nrow(cleaned_data), "sistemas\n")
      return(cleaned_data)
    }
    
    return(NULL)
  }, error = function(e) {
    cat("  ⚠️ Erro ao acessar Wikipedia:", e$message, "\n")
    return(NULL)
  })
}

# Dados de backup com informações mais precisas
get_research_backup_data <- function() {
  cat("Carregando dados de backup...\n")
  
  backup_data <- tribble(
    ~City, ~Country, ~System, ~Stations, ~Bicycles, ~Operator, ~Launch_Year, ~Website,
    "Seoul", "South Korea", "Seoul Bike (따릉이)", 1540, 20000, "Seoul Metropolitan Government", 2015, "https://www.bikeseoul.com",
    "Paris", "France", "Vélib'", 1400, 14500, "Smovengo", 2007, "https://www.velib-metropole.fr",
    "Barcelona", "Spain", "Bicing", 517, 6000, "Clear Channel", 2007, "https://www.bicing.barcelona",
    "Berlin", "Germany", "nextbike", 200, 3000, "nextbike GmbH", 2009, "https://www.nextbike.de",
    "Toronto", "Canada", "Bike Share Toronto", 625, 6850, "PBSC Urban Solutions", 2011, "https://bikesharetoronto.com",
    "Boston", "United States", "Bluebikes", 400, 4000, "Motivate", 2011, "https://www.bluebikes.com",
    "San Francisco", "United States", "Bay Wheels", 500, 4500, "Motivate", 2017, "https://www.baywheels.com",
    "Amsterdam", "Netherlands", "OV-fiets", 300, 3100, "NS Dutch Railways", 2003, "https://www.ov-fiets.nl",
    "Copenhagen", "Denmark", "Bycyklen", 125, 1860, "I Bike CPH", 2013, "https://bycyklen.dk",
    "Stockholm", "Sweden", "Stockholm City Bikes", 140, 1500, "Clear Channel", 2006, "https://citybikes.se"
  ) %>%
    mutate(Source = "Research Data")
  
  return(backup_data)
}

# === EXECUTAR COLETA MELHORADA ===

cat("=== OBTENDO DADOS DE BIKE SHARING ===\n")

# Lista de APIs para tentar
apis_config <- list(
  list(
    func = get_citibike_data,
    name = "Citi Bike"
  ),
  list(
    func = function() get_gbfs_data(
      "https://api.tfl.gov.uk/BikePoint", 
      "London", "United Kingdom", "Santander Cycles", 
      "Serco", 2010, "https://tfl.gov.uk/modes/cycling/santander-cycles"
    ),
    name = "London Bikes"
  ),
  list(
    func = function() get_gbfs_data(
      "https://gbfs.capitalbikeshare.com/gbfs/en/station_information.json",
      "Washington DC", "United States", "Capital Bikeshare",
      "Motivate", 2010, "https://capitalbikeshare.com"
    ),
    name = "Capital Bikeshare"
  )
)

# Coletar dados de APIs
api_results <- list()
for (api in apis_config) {
  result <- api$func()
  if (!is.null(result)) {
    api_results[[api$name]] <- result
    cat("  ✓", api$name, "dados obtidos\n")
  } else {
    cat("  ✗", api$name, "falhou\n")
  }
}

# Tentar Wikipedia
wiki_data <- get_wikipedia_bike_data()
if (!is.null(wiki_data)) {
  api_results[["Wikipedia"]] <- wiki_data
}

# Sempre incluir dados de backup
backup_data <- get_research_backup_data()
api_results[["Backup"]] <- backup_data

# Combinar todos os dados
if (length(api_results) > 0) {
  final_bike_data <- bind_rows(api_results) %>%
    distinct(City, Country, .keep_all = TRUE) %>%  # Remover duplicatas
    arrange(Country, City)
  
  cat("✓ Dados combinados:", nrow(final_bike_data), "sistemas únicos\n")
} else {
  stop("Nenhum dado foi obtido. Verifique a conexão com a internet.")
}

# Salvar dados
write_csv(final_bike_data, "data/raw/raw_bike_sharing_systems.csv")

# === FUNÇÃO MELHORADA PARA CRIAR DADOS DE CIDADES ===

create_cities_data <- function(bike_data) {
  cat("Criando dados de cidades com coordenadas...\n")
  
  cities_data <- bike_data %>%
    select(City, Country) %>%
    distinct() %>%
    mutate(
      # Obter coordenadas do dicionário
      coords = map(City, ~CITY_COORDINATES[[.x]]),
      lat = map_dbl(coords, ~ifelse(is.null(.x), NA, .x$lat)),
      lon = map_dbl(coords, ~ifelse(is.null(.x), NA, .x$lon))
    ) %>%
    select(-coords) %>%
    # Converter para formato esperado pelos scripts seguintes
    mutate(
      Lat_D = floor(lat),
      Lat_M = floor((lat - Lat_D) * 60),
      Lat_S = round(((lat - Lat_D) * 60 - Lat_M) * 60),
      NS = "N",
      Lon_D = abs(floor(lon)),
      Lon_M = floor(abs((lon - floor(lon)) * 60)),
      Lon_S = round((abs((lon - floor(lon)) * 60) - Lon_M) * 60),
      EW = ifelse(lon < 0, "W", "E")
    ) %>%
    select(-lat, -lon)
  
  return(cities_data)
}

# Criar e salvar dados de cidades
cities_data <- create_cities_data(final_bike_data)
write_csv(cities_data, "data/raw/raw_worldcities.csv")
cat("✓ Dados de cidades criados:", nrow(cities_data), "cidades\n")

# === DADOS METEOROLÓGICOS (mantém a lógica original) ===

cat("\n=== OBTENDO DADOS METEOROLÓGICOS ===\n")

api_key <- "6b392355de89496bf9c27c3605a72c3d"
cities_for_weather <- c("Seoul,KR", "New York,US", "Paris,FR", "London,GB", "Barcelona,ES")

weather_data <- lapply(cities_for_weather, function(city) {
  cat("Coletando dados meteorológicos para:", city, "\n")
  
  tryCatch({
    url <- paste0(
      "https://api.openweathermap.org/data/2.5/forecast?q=",
      URLencode(city), "&appid=", api_key, "&units=metric"
    )
    
    res <- GET(url, timeout(10))
    if (status_code(res) != 200) {
      stop(paste("HTTP", status_code(res)))
    }
    
    json_txt <- content(res, as = "text", encoding = "UTF-8")
    parsed <- fromJSON(json_txt, flatten = TRUE)
    
    if (!is.null(parsed) && parsed$cod == "200") {
      df <- parsed$list
      df$city <- sub(",.*$", "", city)
      df$country_code <- sub(".*,", "", city)
      
      if (!is.null(parsed$city$coord)) {
        df$lat <- parsed$city$coord$lat
        df$lon <- parsed$city$coord$lon
      }
      
      cat("  ✓ Sucesso para", city, "(", nrow(df), "registros)\n")
      return(df)
    } else {
      stop(paste("API retornou código:", parsed$cod))
    }
  }, error = function(e) {
    cat("  ⚠️ Erro para", city, ":", e$message, "\n")
    return(NULL)
  })
})

# Processar dados meteorológicos
weather_data <- weather_data[!sapply(weather_data, is.null)]
if (length(weather_data) > 0) {
  weather_df <- bind_rows(weather_data)
  write_csv(weather_df, "data/raw/raw_cities_weather_forecast.csv")
  cat("✓ Dados meteorológicos salvos para", length(unique(weather_df$city)), "cidades\n")
}

# === RESUMO FINAL MELHORADO ===

cat("\n=== RESUMO DA RECOLHA DE DADOS ===\n")

# Verificar qualidade dos dados
if (file.exists("data/raw/raw_bike_sharing_systems.csv")) {
  bike_summary <- read_csv("data/raw/raw_bike_sharing_systems.csv", show_col_types = FALSE)
  cat(sprintf("✓ Sistemas de bike sharing: %d cidades, %d países\n", 
              nrow(bike_summary), length(unique(bike_summary$Country))))
  
  # Mostrar estatísticas
  cat(sprintf("  - Total de estações: %s\n", 
              format(sum(bike_summary$Stations, na.rm = TRUE), big.mark = ",")))
  cat(sprintf("  - Total de bicicletas: %s\n", 
              format(sum(bike_summary$Bicycles, na.rm = TRUE), big.mark = ",")))
}

if (file.exists("data/raw/raw_cities_weather_forecast.csv")) {
  weather_summary <- read_csv("data/raw/raw_cities_weather_forecast.csv", show_col_types = FALSE)
  cat(sprintf("✓ Previsões meteorológicas: %d registros para %d cidades\n", 
              nrow(weather_summary), length(unique(weather_summary$city))))
}

cat("\n✓ Recolha de dados concluída com estruturas padronizadas\n")