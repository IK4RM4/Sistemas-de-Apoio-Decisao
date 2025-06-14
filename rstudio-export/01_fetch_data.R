# 01_fetch_data.R - Sistema de Recolha de Dados Simplificado
# Sistemas de Apoio à Decisão 2024/2025

# Carregar bibliotecas necessárias
library(httr)
library(jsonlite)
library(rvest)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

# Criar estrutura de directorias
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("config", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

cat("SISTEMAS DE APOIO À DECISÃO - RECOLHA DE DADOS\n")
cat("=============================================\n")

# === GESTÃO DE CONFIGURAÇÃO ===

create_config <- function() {
  config <- list(
    api = list(
      openweather_key = Sys.getenv("OPENWEATHER_API_KEY", "6b392355de89496bf9c27c3605a72c3d"),
      timeout_seconds = 30,
      retry_attempts = 3,
      retry_delay = 2
    ),
    cities = list(
      target_cities = c("Seoul,KR", "New York,US", "Paris,FR", "London,GB", "Barcelona,ES"),
      forecast_days = 5,
      forecast_interval_hours = 3
    ),
    data_sources = list(
      # Fontes de dados estáticas e web scraping
      bike_systems_csv = "https://raw.githubusercontent.com/NABSA/gbfs/master/systems.csv",
      wiki_bike_url = "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems",
      
      # Apenas OpenWeather API
      weather_endpoints = list(
        current = "https://api.openweathermap.org/data/2.5/weather",
        forecast = "https://api.openweathermap.org/data/2.5/forecast"
      )
    ),
    backup_data = list(
      enable_backup = TRUE,
      seoul_data_months = 4,
      weather_data_months = 4,
      base_temperatures = list(
        "Seoul,KR" = 12,
        "New York,US" = 10,
        "Paris,FR" = 11,
        "London,GB" = 9,
        "Barcelona,ES" = 16
      )
    ),
    units = list(
      temperature = "celsius",
      wind_speed = "kmh",
      precipitation = "mm",
      visibility = "km",
      pressure = "hpa"
    )
  )
  
  # Guardar configuração
  config_file <- "config/data_collection_config.json"
  write(toJSON(config, pretty = TRUE), config_file)
  cat("Configuração criada:", config_file, "\n")
  
  return(config)
}

# Carregar configuração
CONFIG <- create_config()
API_KEY <- CONFIG$api$openweather_key
CITIES_LIST <- CONFIG$cities$target_cities
WEATHER_ENDPOINTS <- CONFIG$data_sources$weather_endpoints

# === FUNÇÕES UTILITÁRIAS ===

validate_api_key <- function(api_key) {
  if (is.null(api_key) || api_key == "" || nchar(api_key) < 10) {
    return(FALSE)
  }
  return(TRUE)
}

convert_temperature <- function(temp_kelvin, target_unit = "celsius") {
  if (target_unit == "celsius") {
    return(round(temp_kelvin - 273.15, 1))
  } else if (target_unit == "fahrenheit") {
    return(round((temp_kelvin - 273.15) * 9/5 + 32, 1))
  }
  return(temp_kelvin)
}

convert_wind_speed <- function(speed_ms, target_unit = "kmh") {
  if (target_unit == "kmh") {
    return(round(speed_ms * 3.6, 1))
  } else if (target_unit == "mph") {
    return(round(speed_ms * 2.237, 1))
  }
  return(speed_ms)
}

convert_visibility <- function(visibility_m, target_unit = "km") {
  if (target_unit == "km") {
    return(round(visibility_m / 1000, 1))
  } else if (target_unit == "miles") {
    return(round(visibility_m / 1609.34, 1))
  }
  return(visibility_m)
}

# === RECOLHA DE DADOS METEOROLÓGICOS (OPENWEATHER API) ===

test_openweather_api <- function() {
  cat("Testando conectividade OpenWeather API...\n")
  
  if (!validate_api_key(API_KEY)) {
    cat("  Chave API inválida ou em falta\n")
    return(FALSE)
  }
  
  tryCatch({
    test_url <- paste0(WEATHER_ENDPOINTS$current, 
                       "?q=London,GB&appid=", API_KEY)
    
    response <- GET(test_url, timeout(CONFIG$api$timeout_seconds))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, as = "text"))
      if (!is.null(data$main$temp)) {
        cat("  API OpenWeather operacional\n")
        return(TRUE)
      }
    } else if (status_code(response) == 401) {
      cat("  Erro de autenticação API (401)\n")
      return(FALSE)
    } else {
      cat("  Status da resposta:", status_code(response), "\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("  Erro de conectividade:", e$message, "\n")
    return(FALSE)
  })
}

get_weather_forecast <- function() {
  cat("Recolhendo previsão meteorológica...\n")
  
  if (!test_openweather_api()) {
    cat("  API indisponível - usando dados de backup\n")
    return(create_backup_weather_data())
  }
  
  all_weather_data <- map_dfr(CITIES_LIST, function(city) {
    cat("  Processando:", city, "\n")
    
    for (attempt in 1:CONFIG$api$retry_attempts) {
      weather_result <- try_get_city_weather(city)
      if (!is.null(weather_result)) {
        cat("    Sucesso na tentativa", attempt, "\n")
        return(weather_result)
      }
      
      if (attempt < CONFIG$api$retry_attempts) {
        cat("    Tentativa", attempt, "falhou - tentando novamente\n")
        Sys.sleep(CONFIG$api$retry_delay)
      }
    }
    
    cat("    Falha após", CONFIG$api$retry_attempts, "tentativas\n")
    return(NULL)
  })
  
  if (is.null(all_weather_data) || nrow(all_weather_data) == 0) {
    cat("  Recolha falhada - usando dados de backup\n")
    return(create_backup_weather_data())
  }
  
  cat("  Dados recolhidos:", nrow(all_weather_data), "registos\n")
  return(all_weather_data)
}

try_get_city_weather <- function(city) {
  tryCatch({
    # Tentar endpoint de previsão primeiro
    forecast_url <- paste0(WEATHER_ENDPOINTS$forecast,
                           "?q=", URLencode(city),
                           "&appid=", API_KEY,
                           "&units=metric")
    
    response <- GET(forecast_url, timeout(CONFIG$api$timeout_seconds))
    
    if (status_code(response) == 200) {
      json_content <- content(response, as = "text", encoding = "UTF-8")
      weather_data <- fromJSON(json_content, flatten = TRUE)
      return(process_forecast_response(weather_data, city))
    }
    
    # Se previsão falhar, tentar dados atuais
    current_url <- paste0(WEATHER_ENDPOINTS$current,
                          "?q=", URLencode(city),
                          "&appid=", API_KEY,
                          "&units=metric")
    
    response <- GET(current_url, timeout(CONFIG$api$timeout_seconds))
    
    if (status_code(response) == 200) {
      json_content <- content(response, as = "text", encoding = "UTF-8")
      weather_data <- fromJSON(json_content, flatten = TRUE)
      return(process_current_response(weather_data, city))
    }
    
    return(NULL)
    
  }, error = function(e) {
    cat("    Erro para", city, ":", e$message, "\n")
    return(NULL)
  })
}

process_forecast_response <- function(weather_data, city) {
  if (is.null(weather_data$list)) {
    return(NULL)
  }
  
  city_name <- sub(",.*$", "", city)
  country_code <- sub(".*,", "", city)
  
  tryCatch({
    forecast_df <- as.data.frame(weather_data$list, stringsAsFactors = FALSE)
    
    # Processar descrições do tempo
    weather_main <- "Clear"
    weather_description <- "clear sky"
    
    if ("weather" %in% names(forecast_df) && is.list(forecast_df$weather)) {
      weather_main <- sapply(forecast_df$weather, function(x) {
        if (is.list(x) && length(x) > 0 && "main" %in% names(x[[1]])) {
          return(x[[1]]$main)
        } else {
          return("Clear")
        }
      })
      
      weather_description <- sapply(forecast_df$weather, function(x) {
        if (is.list(x) && length(x) > 0 && "description" %in% names(x[[1]])) {
          return(x[[1]]$description)
        } else {
          return("clear sky")
        }
      })
    }
    
    # Processar dados
    forecast_df <- forecast_df %>%
      mutate(
        city_name = city_name,
        country_code = country_code,
        date = as.Date(dt_txt),
        hour = hour(ymd_hms(dt_txt)),
        
        # Variáveis meteorológicas
        temperature_c = ifelse("main.temp" %in% names(.), main.temp, 15),
        feels_like_c = ifelse("main.feels_like" %in% names(.), main.feels_like, temperature_c),
        humidity_percent = ifelse("main.humidity" %in% names(.), main.humidity, 60),
        pressure_hpa = ifelse("main.pressure" %in% names(.), main.pressure, 1013),
        
        # Vento
        wind_speed_ms = ifelse("wind.speed" %in% names(.), wind.speed, 3),
        wind_speed_kmh = convert_wind_speed(wind_speed_ms),
        wind_direction_deg = ifelse("wind.deg" %in% names(.), wind.deg, 180),
        
        # Visibilidade
        visibility_m = ifelse("visibility" %in% names(.), visibility, 10000),
        visibility_km = convert_visibility(visibility_m),
        
        # Precipitação
        rainfall_mm = ifelse("rain.3h" %in% names(.), rain.3h, 0),
        snowfall_mm = ifelse("snow.3h" %in% names(.), snow.3h, 0),
        
        # Nuvens
        cloudiness_percent = ifelse("clouds.all" %in% names(.), clouds.all, 20),
        
        # Descrições
        weather_main = weather_main,
        weather_description = weather_description,
        
        # Metadata
        data_source = "OpenWeather API - Forecast",
        fetch_timestamp = Sys.time()
      ) %>%
      select(
        city_name, country_code, date, hour, dt_txt,
        temperature_c, feels_like_c, humidity_percent, pressure_hpa,
        wind_speed_ms, wind_speed_kmh, wind_direction_deg,
        visibility_m, visibility_km, rainfall_mm, snowfall_mm,
        weather_main, weather_description, cloudiness_percent,
        data_source, fetch_timestamp
      )
    
    return(forecast_df)
    
  }, error = function(e) {
    cat("    Erro no processamento de previsão:", e$message, "\n")
    return(NULL)
  })
}

process_current_response <- function(weather_data, city) {
  if (is.null(weather_data$main)) {
    return(NULL)
  }
  
  city_name <- sub(",.*$", "", city)
  country_code <- sub(".*,", "", city)
  
  tryCatch({
    # Gerar timeline de previsão baseada em dados atuais
    current_time <- Sys.time()
    forecast_hours <- CONFIG$cities$forecast_days * 24 / CONFIG$cities$forecast_interval_hours
    future_times <- current_time + hours(seq(0, forecast_hours * CONFIG$cities$forecast_interval_hours, 
                                             by = CONFIG$cities$forecast_interval_hours))
    
    # Descrição do tempo
    weather_main <- "Clear"
    weather_description <- "clear sky"
    
    if (!is.null(weather_data$weather) && length(weather_data$weather) > 0) {
      if (is.list(weather_data$weather[[1]])) {
        weather_main <- ifelse(!is.null(weather_data$weather[[1]]$main), 
                               weather_data$weather[[1]]$main, "Clear")
        weather_description <- ifelse(!is.null(weather_data$weather[[1]]$description), 
                                      weather_data$weather[[1]]$description, "clear sky")
      }
    }
    
    # Criar previsão extendida
    current_df <- data.frame(
      city_name = city_name,
      country_code = country_code,
      date = as.Date(future_times),
      hour = hour(future_times),
      dt_txt = format(future_times, "%Y-%m-%d %H:%M:%S"),
      
      # Variáveis meteorológicas com variabilidade
      temperature_c = ifelse(!is.null(weather_data$main$temp), 
                             weather_data$main$temp + rnorm(length(future_times), 0, 2), 
                             15 + rnorm(length(future_times), 0, 2)),
      feels_like_c = ifelse(!is.null(weather_data$main$feels_like), 
                            weather_data$main$feels_like + rnorm(length(future_times), 0, 1.5), 
                            15 + rnorm(length(future_times), 0, 1.5)),
      humidity_percent = pmax(20, pmin(100, 
                                       ifelse(!is.null(weather_data$main$humidity), 
                                              weather_data$main$humidity + rnorm(length(future_times), 0, 5), 
                                              60 + rnorm(length(future_times), 0, 5)))),
      pressure_hpa = ifelse(!is.null(weather_data$main$pressure), 
                            weather_data$main$pressure + rnorm(length(future_times), 0, 5), 
                            1013 + rnorm(length(future_times), 0, 5)),
      
      stringsAsFactors = FALSE
    )
    
    # Processar vento
    base_wind_speed <- ifelse(!is.null(weather_data$wind$speed), weather_data$wind$speed, 3)
    wind_speed_ms <- pmax(0, base_wind_speed + rnorm(length(future_times), 0, 1))
    current_df$wind_speed_ms <- wind_speed_ms
    current_df$wind_speed_kmh <- convert_wind_speed(wind_speed_ms)
    current_df$wind_direction_deg <- ifelse(!is.null(weather_data$wind$deg), 
                                            weather_data$wind$deg, 180)
    
    # Processar visibilidade
    visibility_m <- ifelse(!is.null(weather_data$visibility), weather_data$visibility, 10000)
    current_df$visibility_m <- visibility_m
    current_df$visibility_km <- convert_visibility(visibility_m)
    
    # Precipitação
    current_df$rainfall_mm <- pmax(0, rpois(length(future_times), 0.5))
    current_df$snowfall_mm <- pmax(0, ifelse(current_df$temperature_c < 2, 
                                             rpois(length(future_times), 0.2), 0))
    
    # Condições
    current_df$weather_main <- weather_main
    current_df$weather_description <- weather_description
    current_df$cloudiness_percent <- ifelse(!is.null(weather_data$clouds$all), 
                                            weather_data$clouds$all, 50)
    
    # Metadata
    current_df$data_source <- "OpenWeather API - Current Extended"
    current_df$fetch_timestamp <- Sys.time()
    
    return(current_df)
    
  }, error = function(e) {
    cat("    Erro no processamento atual:", e$message, "\n")
    return(NULL)
  })
}

# === DADOS DE BACKUP ===

create_backup_weather_data <- function() {
  cat("Gerando dados meteorológicos de backup...\n")
  
  cities_info <- CONFIG$backup_data$base_temperatures
  start_date <- Sys.Date()
  end_date <- Sys.Date() + months(CONFIG$backup_data$weather_data_months)
  
  backup_weather <- map_dfr(names(cities_info), function(city_key) {
    city_name <- sub(",.*$", "", city_key)
    country_code <- sub(".*,", "", city_key)
    base_temp <- cities_info[[city_key]]
    
    # Timeline
    dates <- seq(start_date, end_date, by = "day")
    hours <- seq(0, 23, by = CONFIG$cities$forecast_interval_hours)
    
    all_combinations <- expand.grid(
      date = dates,
      hour = hours,
      stringsAsFactors = FALSE
    )
    all_combinations$date <- as.Date(all_combinations$date)
    
    # Padrões climatológicos
    day_of_year <- yday(all_combinations$date)
    seasonal_temp <- base_temp + 15 * sin(2 * pi * (day_of_year - 80) / 365)
    daily_temp <- 4 * sin(2 * pi * all_combinations$hour / 24)
    
    all_combinations$city_name <- city_name
    all_combinations$country_code <- country_code
    all_combinations$dt_txt <- paste(all_combinations$date, sprintf("%02d:00:00", all_combinations$hour))
    
    # Variáveis meteorológicas
    all_combinations$temperature_c <- seasonal_temp + daily_temp + rnorm(nrow(all_combinations), 0, 2.5)
    all_combinations$feels_like_c <- all_combinations$temperature_c + rnorm(nrow(all_combinations), 0, 1)
    all_combinations$humidity_percent <- pmax(25, pmin(95, rnorm(nrow(all_combinations), 65, 15)))
    all_combinations$pressure_hpa <- rnorm(nrow(all_combinations), 1013, 10)
    
    # Vento
    wind_speed_ms <- pmax(0, rnorm(nrow(all_combinations), 3.5, 1.5))
    all_combinations$wind_speed_ms <- wind_speed_ms
    all_combinations$wind_speed_kmh <- convert_wind_speed(wind_speed_ms)
    all_combinations$wind_direction_deg <- sample(0:359, nrow(all_combinations), replace = TRUE)
    
    # Visibilidade
    visibility_m <- round(rnorm(nrow(all_combinations), 12000, 3000))
    all_combinations$visibility_m <- visibility_m
    all_combinations$visibility_km <- convert_visibility(visibility_m)
    
    # Precipitação
    all_combinations$rainfall_mm <- pmax(0, rpois(nrow(all_combinations), 0.6))
    all_combinations$snowfall_mm <- pmax(0, ifelse(all_combinations$temperature_c < 2, 
                                                   rpois(nrow(all_combinations), 0.3), 0))
    
    # Condições
    weather_types <- sample(c("Clear", "Clouds", "Rain", "Snow"), nrow(all_combinations), 
                            replace = TRUE, prob = c(0.4, 0.35, 0.2, 0.05))
    all_combinations$weather_main <- weather_types
    all_combinations$weather_description <- ifelse(weather_types == "Clear", "clear sky",
                                                   ifelse(weather_types == "Clouds", "few clouds",
                                                          ifelse(weather_types == "Rain", "light rain", "light snow")))
    
    all_combinations$cloudiness_percent <- ifelse(weather_types == "Clear", 
                                                  sample(0:25, nrow(all_combinations), replace = TRUE),
                                                  ifelse(weather_types == "Clouds", 
                                                         sample(25:75, nrow(all_combinations), replace = TRUE),
                                                         sample(75:100, nrow(all_combinations), replace = TRUE)))
    
    # Metadata
    all_combinations$data_source <- "Backup Weather Generator"
    all_combinations$fetch_timestamp <- Sys.time()
    
    return(all_combinations)
  })
  
  cat("  Dados de backup gerados:", nrow(backup_weather), "registos\n")
  return(backup_weather)
}

# === RECOLHA DE SISTEMAS DE BICICLETAS ===

get_bike_sharing_systems <- function() {
  cat("Recolhendo dados de sistemas de bicicletas...\n")
  
  # Tentar primeiro CSV do GBFS
  gbfs_data <- try_gbfs_csv()
  if (!is.null(gbfs_data) && nrow(gbfs_data) > 0) {
    return(gbfs_data)
  }
  
  # Tentar web scraping da Wikipedia
  wiki_data <- try_wikipedia_scraping()
  if (!is.null(wiki_data) && nrow(wiki_data) > 0) {
    return(wiki_data)
  }
  
  # Fallback para dados curados
  cat("  Usando dados curados como fallback\n")
  return(create_curated_bike_systems())
}

try_gbfs_csv <- function() {
  cat("  Tentando CSV oficial GBFS...\n")
  
  tryCatch({
    gbfs_url <- CONFIG$data_sources$bike_systems_csv
    systems <- read_csv(gbfs_url, show_col_types = FALSE)
    
    if (nrow(systems) > 0) {
      cat("    Sucesso - GBFS CSV:", nrow(systems), "sistemas\n")
      
      # Processar dados GBFS
      processed_systems <- systems %>%
        mutate(
          data_source = "GBFS Official CSV",
          collection_date = Sys.Date()
        ) %>%
        select(any_of(c("Name", "Location", "Country", "System ID", "URL", 
                        "data_source", "collection_date"))) %>%
        rename_with(~ case_when(
          . == "Name" ~ "system_name",
          . == "Location" ~ "city",
          . == "Country" ~ "country",
          . == "System ID" ~ "system_id",
          . == "URL" ~ "api_url",
          TRUE ~ .
        ))
      
      return(processed_systems)
    }
    
    return(NULL)
    
  }, error = function(e) {
    cat("    Erro ao carregar GBFS CSV:", e$message, "\n")
    return(NULL)
  })
}

try_wikipedia_scraping <- function() {
  cat("  Tentando web scraping da Wikipedia...\n")
  
  tryCatch({
    page <- read_html(CONFIG$data_sources$wiki_bike_url)
    tables <- page %>% html_nodes("table.wikitable, table.sortable")
    
    if (length(tables) == 0) {
      cat("    Nenhuma tabela encontrada\n")
      return(NULL)
    }
    
    for (i in seq_along(tables)) {
      table_data <- tryCatch({
        tables[[i]] %>% 
          html_table(fill = TRUE) %>%
          as.data.frame()
      }, error = function(e) NULL)
      
      if (is.null(table_data)) next
      
      col_names <- names(table_data) %>% str_to_lower()
      
      if (any(str_detect(col_names, "city|location")) && 
          any(str_detect(col_names, "system|name")) &&
          nrow(table_data) > 10) {
        
        cleaned_data <- table_data %>%
          mutate(
            data_source = "Wikipedia Scraping",
            scrape_date = Sys.Date(),
            table_number = i
          )
        
        cat("    Sucesso - Tabela", i, ":", nrow(cleaned_data), "entradas\n")
        return(cleaned_data)
      }
    }
    
    cat("    Nenhuma tabela adequada encontrada\n")
    return(NULL)
    
  }, error = function(e) {
    cat("    Erro no web scraping:", e$message, "\n")
    return(NULL)
  })
}

create_curated_bike_systems <- function() {
  cat("  Criando dados curados de sistemas de bicicletas...\n")
  
  systems_data <- data.frame(
    city = c("Seoul", "New York", "Paris", "London", "Barcelona"),
    country = c("South Korea", "United States", "France", "United Kingdom", "Spain"),
    system_name = c("Seoul Bike (따릉이)", "Citi Bike", "Vélib' Métropole", "Santander Cycles", "Bicing"),
    operator = c("Seoul Metropolitan Government", "Lyft", "Smovengo", "Serco", "Clear Channel"),
    launch_year = c(2015, 2013, 2007, 2010, 2007),
    stations = c(1540, 1300, 1400, 750, 517),
    bicycles = c(20000, 17000, 14500, 11500, 6000),
    latitude = c(37.5665, 40.7128, 48.8566, 51.5074, 41.3851),
    longitude = c(126.9780, -74.0060, 2.3522, -0.1278, 2.1734),
    status = "Active",
    data_source = "Curated Dataset",
    collection_date = Sys.Date(),
    stringsAsFactors = FALSE
  )
  
  cat("    Dados curados criados:", nrow(systems_data), "sistemas\n")
  return(systems_data)
}

# === DADOS DE CIDADES DO MUNDO ===

create_world_cities_data <- function() {
  cat("Criando dados de cidades do mundo...\n")
  
  cities_from_config <- names(CONFIG$backup_data$base_temperatures)
  
  cities_data <- map_dfr(cities_from_config, function(city_key) {
    city_name <- sub(",.*$", "", city_key)
    country_code <- sub(".*,", "", city_key)
    
    city_details <- switch(city_key,
                           "Seoul,KR" = list(country = "South Korea", lat = 37.5665, lon = 126.9780, population = 9720846),
                           "New York,US" = list(country = "United States", lat = 40.7128, lon = -74.0060, population = 8336817),
                           "Paris,FR" = list(country = "France", lat = 48.8566, lon = 2.3522, population = 2161000),
                           "London,GB" = list(country = "United Kingdom", lat = 51.5074, lon = -0.1278, population = 8982000),
                           "Barcelona,ES" = list(country = "Spain", lat = 41.3851, lon = 2.1734, population = 1620343),
                           list(country = "Unknown", lat = 0, lon = 0, population = 1000000))
    
    data.frame(
      city = city_name,
      country = city_details$country,
      country_code = country_code,
      latitude = city_details$lat,
      longitude = city_details$lon,
      population = city_details$population,
      data_source = "Curated Geographic Dataset",
      created_date = Sys.Date(),
      stringsAsFactors = FALSE
    )
  })
  
  cat("  Dados de cidades criados:", nrow(cities_data), "cidades\n")
  return(cities_data)
}

# === DADOS DE SEOUL BIKE SHARING ===

create_seoul_bike_data <- function() {
  cat("Criando dados de Seoul bike sharing...\n")
  
  months_back <- CONFIG$backup_data$seoul_data_months
  dates <- seq(Sys.Date() - months(months_back), Sys.Date() - days(1), by = "day")
  
  seoul_data <- map_dfr(dates, function(d) {
    data.frame(
      date = format(d, "%d/%m/%Y"),
      hour = 0:23,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        # Padrões de procura realistas
        base_demand = ifelse(hour %in% 7:9, 300,
                             ifelse(hour %in% 17:19, 380,
                                    ifelse(hour %in% 12:14, 180,
                                           ifelse(hour %in% 20:23, 100,
                                                  ifelse(hour %in% 0:5, 25, 120))))),
        
        # Dados meteorológicos
        seoul_base_temp = CONFIG$backup_data$base_temperatures[["Seoul,KR"]],
        temperature_c = seoul_base_temp + 12 * sin(2 * pi * (yday(d) - 80) / 365) + 
          4 * sin(2 * pi * hour / 24) + rnorm(24, 0, 2.5),
        humidity_percent = pmax(30, pmin(90, rnorm(24, 65, 12))),
        
        # Vento
        wind_speed_ms = pmax(0, rnorm(24, 3.5, 1.5)),
        wind_speed_kmh = convert_wind_speed(wind_speed_ms),
        
        # Outras variáveis meteorológicas
        visibility_km = round(rnorm(24, 15, 3), 1),
        dew_point_temperature_c = temperature_c - ((100 - humidity_percent) / 5),
        solar_radiation_mj_m2 = ifelse(hour >= 6 & hour <= 18, 
                                       pmax(0, sin(pi * (hour - 6) / 12) * 2.2), 0),
        rainfall_mm = pmax(0, rpois(24, 0.4)),
        snowfall_cm = pmax(0, rpois(24, 0.1)),
        
        # Calcular procura realista baseada no tempo
        temp_effect = pmax(-40, pmin(40, (temperature_c - 18) * 4)),
        humidity_effect = -abs(humidity_percent - 60) * 1.2,
        wind_effect = -pmax(0, wind_speed_kmh - 12) * 2.5,
        rain_effect = -rainfall_mm * 8,
        snow_effect = -snowfall_cm * 15,
        weekend_effect = ifelse(wday(d) %in% c(1, 7), 20, 0),
        
        # Contagem final de bicicletas
        rented_bike_count = pmax(0, round(
          base_demand + temp_effect + humidity_effect + 
            wind_effect + rain_effect + snow_effect + weekend_effect + 
            rnorm(24, 0, 18), 0
        )),
        
        # Variáveis categóricas
        seasons = ifelse(month(d) %in% c(12, 1, 2), "Winter",
                         ifelse(month(d) %in% c(3, 4, 5), "Spring",
                                ifelse(month(d) %in% c(6, 7, 8), "Summer", "Autumn"))),
        holiday = ifelse(wday(d) %in% c(1, 7), "Holiday", "No Holiday"),
        functioning_day = "Yes",
        
        # Metadata
        data_source = "Generated Seoul Data"
      ) %>%
      # Remover colunas intermediárias
      select(-seoul_base_temp, -base_demand, -temp_effect, -humidity_effect, 
             -wind_effect, -rain_effect, -snow_effect, -weekend_effect)
  })
  
  cat("  Dados Seoul gerados:", nrow(seoul_data), "registos\n")
  cat("  Período:", min(seoul_data$date), "a", max(seoul_data$date), "\n")
  return(seoul_data)
}

# === EXECUÇÃO PRINCIPAL ===

cat("\n=== FASE 1: VALIDAÇÃO DE CONFIGURAÇÃO ===\n")
cat("Chave API:", ifelse(validate_api_key(API_KEY), "Válida", "Inválida/Em falta"), "\n")
cat("Cidades alvo:", length(CITIES_LIST), "\n")
cat("Dias de previsão:", CONFIG$cities$forecast_days, "\n")

cat("\n=== FASE 2: RECOLHA DE DADOS METEOROLÓGICOS ===\n")
weather_data <- get_weather_forecast()

cat("\n=== FASE 3: RECOLHA DE SISTEMAS DE BICICLETAS ===\n")
bike_systems_data <- get_bike_sharing_systems()

cat("\n=== FASE 4: DADOS DE CIDADES ===\n")
cities_data <- create_world_cities_data()

cat("\n=== FASE 5: DADOS DE SEOUL BIKE SHARING ===\n")
seoul_data <- create_seoul_bike_data()

# === GUARDAR DATASETS ===

save_dataset <- function(data, filename, description) {
  if (!is.null(data) && nrow(data) > 0) {
    write_csv(data, filename)
    cat("Guardado:", description, "-", filename, "(", nrow(data), "registos)\n")
    return(TRUE)
  } else {
    cat("Falha ao guardar:", description, "- sem dados\n")
    return(FALSE)
  }
}

cat("\n=== GUARDAR DATASETS ===\n")

files_saved <- 0
files_saved <- files_saved + save_dataset(weather_data, "data/raw/raw_cities_weather_forecast.csv", "Dados Meteorológicos")
files_saved <- files_saved + save_dataset(bike_systems_data, "data/raw/raw_bike_sharing_systems.csv", "Sistemas de Bicicletas")
files_saved <- files_saved + save_dataset(cities_data, "data/raw/raw_worldcities.csv", "Cidades do Mundo")
files_saved <- files_saved + save_dataset(seoul_data, "data/raw/raw_seoul_bike_sharing.csv", "Dados Seoul")

# Guardar resumo de execução
execution_summary <- list(
  execution_time = Sys.time(),
  api_status = ifelse(validate_api_key(API_KEY), "Valid", "Invalid"),
  cities_processed = CITIES_LIST,
  data_sources_used = list(
    weather = ifelse(!is.null(weather_data) && 
                       any(str_detect(weather_data$data_source, "OpenWeather")), 
                     "OpenWeather API", "Backup Generator"),
    bike_systems = ifelse(!is.null(bike_systems_data) && 
                            any(str_detect(bike_systems_data$data_source, "GBFS|Wikipedia")), 
                          "External Source", "Curated Dataset")
  ),
  files_generated = files_saved,
  total_records = list(
    weather = ifelse(!is.null(weather_data), nrow(weather_data), 0),
    bike_systems = ifelse(!is.null(bike_systems_data), nrow(bike_systems_data), 0),
    cities = ifelse(!is.null(cities_data), nrow(cities_data), 0),
    seoul = ifelse(!is.null(seoul_data), nrow(seoul_data), 0)
  )
)

write(toJSON(execution_summary, pretty = TRUE), "logs/execution_summary.json")

# === RELATÓRIO FINAL ===

cat("\n=== RELATÓRIO DE RECOLHA DE DADOS ===\n")
cat("====================================\n")

# Validação de dados meteorológicos
if (!is.null(weather_data) && nrow(weather_data) > 0) {
  cat("DADOS METEOROLÓGICOS:\n")
  cat("  Registos:", format(nrow(weather_data), big.mark = ","), "\n")
  cat("  Cidades:", length(unique(weather_data$city_name)), "\n")
  cat("  Período:", min(weather_data$date), "a", max(weather_data$date), "\n")
  cat("  Fonte:", unique(weather_data$data_source)[1], "\n")
  
  temp_range <- range(weather_data$temperature_c, na.rm = TRUE)
  cat("  Temperaturas:", round(temp_range[1], 1), "°C a", round(temp_range[2], 1), "°C\n")
} else {
  cat("DADOS METEOROLÓGICOS: FALHA\n")
}

# Validação de dados Seoul
if (!is.null(seoul_data) && nrow(seoul_data) > 0) {
  cat("\nDADOS SEOUL BIKE SHARING:\n")
  cat("  Registos:", format(nrow(seoul_data), big.mark = ","), "\n")
  cat("  Período:", min(seoul_data$date), "a", max(seoul_data$date), "\n")
  cat("  Procura média:", round(mean(seoul_data$rented_bike_count), 1), "bicicletas/hora\n")
  cat("  Procura máxima:", format(max(seoul_data$rented_bike_count), big.mark = ","), "bicicletas\n")
} else {
  cat("\nDADOS SEOUL BIKE SHARING: FALHA\n")
}

# Validação de sistemas de bicicletas
if (!is.null(bike_systems_data) && nrow(bike_systems_data) > 0) {
  cat("\nSISTEMAS DE BICICLETAS:\n")
  cat("  Sistemas:", nrow(bike_systems_data), "\n")
  if ("country" %in% colnames(bike_systems_data)) {
    cat("  Países:", length(unique(bike_systems_data$country)), "\n")
  }
  if ("bicycles" %in% colnames(bike_systems_data)) {
    total_bikes <- sum(as.numeric(bike_systems_data$bicycles), na.rm = TRUE)
    if (total_bikes > 0) {
      cat("  Total de bicicletas:", format(total_bikes, big.mark = ","), "\n")
    }
  }
} else {
  cat("\nSISTEMAS DE BICICLETAS: FALHA\n")
}

# Validação de dados de cidades
if (!is.null(cities_data) && nrow(cities_data) > 0) {
  cat("\nDADOS DE CIDADES:\n")
  cat("  Cidades:", nrow(cities_data), "\n")
  cat("  Países:", length(unique(cities_data$country)), "\n")
} else {
  cat("\nDADOS DE CIDADES: FALHA\n")
}

# Avaliação geral
success_rate <- round((files_saved / 4) * 100, 1)
cat("\nTAXA DE SUCESSO GERAL:", success_rate, "%\n")
cat("Ficheiros guardados:", files_saved, "/ 4\n")

if (files_saved >= 3) {
  cat("\nESTADO: RECOLHA CONCLUÍDA COM SUCESSO\n")
  cat("Próximo passo: Executar 02_clean_data.R\n")
} else if (files_saved >= 2) {
  cat("\nESTADO: SUCESSO PARCIAL\n")
  cat("Alguns datasets em falta - análise pode prosseguir\n")
} else {
  cat("\nESTADO: REQUER ATENÇÃO\n")
  cat("Múltiplas fontes falharam - verificar configuração\n")
}

cat("\nFONTES DE DADOS UTILIZADAS:\n")
if (!is.null(weather_data)) {
  weather_source <- unique(weather_data$data_source)[1]
  cat("- Meteorologia:", weather_source, "\n")
}
if (!is.null(bike_systems_data)) {
  bike_source <- unique(bike_systems_data$data_source)[1]
  cat("- Sistemas de bicicletas:", bike_source, "\n")
}
cat("- Cidades: Dataset curado\n")
cat("- Seoul: Dados gerados\n")

cat("\n====================================\n")
cat("RECOLHA DE DADOS CONCLUÍDA!\n")
cat("====================================\n")