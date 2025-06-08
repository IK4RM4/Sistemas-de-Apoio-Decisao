# fix_common_issues.R - Correção de Problemas Comuns
# Projeto: Previsão de Partilha de Bicicletas com Dados Meteorológicos

# ==============================================================================
# CORREÇÕES BASEADAS NOS ERROS IDENTIFICADOS
# ==============================================================================

cat("🔧 INICIANDO CORREÇÃO DE PROBLEMAS COMUNS\n")
cat("Baseado nos erros identificados no dashboard e execução\n\n")

library(tidyverse)
library(lubridate)

# ==============================================================================
# PROBLEMA 1: COLUNA 'snow_3h' NÃO ENCONTRADA
# ==============================================================================

fix_weather_data_columns <- function() {
  cat("🌨️ Corrigindo problema com coluna 'snow_3h'...\n")
  
  weather_file <- "data/processed/weather_forecast.csv"
  
  if (file.exists(weather_file)) {
    weather_data <- read_csv(weather_file, show_col_types = FALSE)
    
    # Verificar se snow_3h existe e adicionar se necessário
    if (!"snow_3h" %in% colnames(weather_data)) {
      weather_data$snow_3h <- 0
      cat("  ✅ Coluna 'snow_3h' adicionada com valores zero\n")
    }
    
    # Verificar se rain_3h existe
    if (!"rain_3h" %in% colnames(weather_data)) {
      weather_data$rain_3h <- ifelse(is.na(weather_data$rainfall_mm), 0, weather_data$rainfall_mm)
      cat("  ✅ Coluna 'rain_3h' adicionada baseada em 'rainfall_mm'\n")
    }
    
    # Verificar outras colunas essenciais
    essential_cols <- c("temperature_c", "humidity_percent", "wind_speed_m_s", 
                        "visibility_10m", "dew_point_temperature_c", 
                        "solar_radiation_mj_m2", "rainfall_mm", "snowfall_cm")
    
    for (col in essential_cols) {
      if (!col %in% colnames(weather_data)) {
        if (col == "snowfall_cm") {
          weather_data[[col]] <- 0
        } else if (col == "rainfall_mm") {
          weather_data[[col]] <- 0
        } else if (col == "visibility_10m") {
          weather_data[[col]] <- 1000
        } else {
          weather_data[[col]] <- 0
        }
        cat("  ✅ Coluna '", col, "' adicionada com valores padrão\n")
      }
    }
    
    # Salvar dados corrigidos
    write_csv(weather_data, weather_file)
    cat("✅ Dados meteorológicos corrigidos e salvos\n")
    
  } else {
    cat("⚠️ Arquivo de dados meteorológicos não encontrado. Execute a recolha de dados primeiro.\n")
  }
}

# ==============================================================================
# PROBLEMA 2: COLUNAS LAT/LON COM NOMES INCONSISTENTES
# ==============================================================================

fix_cities_coordinates <- function() {
  cat("🗺️ Corrigindo coordenadas das cidades...\n")
  
  cities_file <- "data/processed/world_cities.csv"
  
  if (file.exists(cities_file)) {
    cities_data <- read_csv(cities_file, show_col_types = FALSE)
    
    # Verificar e padronizar nomes das colunas
    col_mapping <- list(
      c("LatD", "lat_d", "Lat_D") = "lat_d",
      c("LatM", "lat_m", "Lat_M") = "lat_m", 
      c("LatS", "lat_s", "Lat_S") = "lat_s",
      c("LonD", "lon_d", "Lon_D") = "lon_d",
      c("LonM", "lon_m", "Lon_M") = "lon_m",
      c("LonS", "lon_s", "Lon_S") = "lon_s"
    )
    
    # Aplicar mapeamento de colunas
    for (possible_names in names(col_mapping)) {
      target_name <- col_mapping[[possible_names]]
      found_col <- NULL
      
      for (name in eval(parse(text = possible_names))) {
        if (name %in% colnames(cities_data)) {
          found_col <- name
          break
        }
      }
      
      if (!is.null(found_col) && found_col != target_name) {
        cities_data <- cities_data %>%
          rename(!!target_name := !!found_col)
        cat("  ✅ Coluna '", found_col, "'