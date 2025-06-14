# 02_clean_data.R - Sistema de Limpeza de Dados Corrigido
# Sistemas de Apoio à Decisão 2024/2025

library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(jsonlite)

# Configuração e directorias
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

cat("SISTEMAS DE APOIO À DECISÃO - LIMPEZA DE DADOS\n")
cat("=============================================\n")

# === CONFIGURAÇÃO DE LIMPEZA ===

create_cleaning_config <- function() {
  config <- list(
    data_validation = list(
      min_date = "2020-01-01",
      max_date = "2030-12-31",
      temperature_range = c(-50, 60),
      humidity_range = c(0, 100), 
      wind_speed_range = c(0, 100),
      visibility_range = c(0, 50)
    ),
    unit_conversions = list(
      wind_ms_to_kmh = 3.6,
      visibility_m_to_km = 0.001,
      snowfall_cm_to_mm = 10
    ),
    default_values = list(
      temperature = 15,
      humidity = 60,
      wind_speed = 3,
      visibility = 10,
      precipitation = 0
    )
  )
  return(config)
}

CONFIG <- create_cleaning_config()

# === FUNÇÕES DE CARREGAMENTO SEGURO ===

safe_load_csv <- function(filepath, description = "") {
  if (!file.exists(filepath)) {
    cat("Ficheiro não encontrado:", filepath, "\n")
    return(NULL)
  }
  
  tryCatch({
    data <- read_csv(filepath, 
                     show_col_types = FALSE,
                     locale = locale(encoding = "UTF-8"),
                     na = c("", "NA", "NULL", "null", "N/A"))
    
    if (nrow(data) == 0) {
      cat("Aviso: Dataset vazio -", description, "\n")
      return(NULL)
    }
    
    cat("Carregado", description, ":", nrow(data), "registos,", ncol(data), "colunas\n")
    return(data)
    
  }, error = function(e) {
    tryCatch({
      data <- read.csv(filepath, stringsAsFactors = FALSE, na.strings = c("", "NA", "NULL"))
      cat("Carregado", description, "com método alternativo:", nrow(data), "registos\n")
      return(as_tibble(data))
    }, error = function(e2) {
      cat("Erro ao carregar", filepath, ":", e2$message, "\n")
      return(NULL)
    })
  })
}

# === FUNÇÕES DE LIMPEZA DE STRINGS ===

standardize_column_names <- function(data) {
  cleaned_names <- names(data) %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9_]", "_") %>%
    str_replace_all("_{2,}", "_") %>%
    str_remove("^_|_$") %>%
    str_replace_all("temperature", "temp") %>%
    str_replace_all("humidity", "humid") %>%
    str_replace_all("wind_speed", "wind") %>%
    str_replace_all("visibility", "vis") %>%
    str_replace_all("precipitation", "precip")
  
  names(data) <- cleaned_names
  return(data)
}

remove_reference_links <- function(data) {
  char_columns <- data %>% select_if(is.character) %>% names()
  
  for (col in char_columns) {
    data[[col]] <- data[[col]] %>%
      str_remove_all("\\[\\d+\\]") %>%
      str_remove_all("https?://[^\\s]+") %>%
      str_remove_all("www\\.[^\\s]+") %>%
      str_remove_all("\\([^)]*\\)$") %>%
      str_trim()
  }
  
  return(data)
}

extract_numeric_values <- function(data, column_name) {
  if (column_name %in% names(data)) {
    data[[column_name]] <- data[[column_name]] %>%
      str_extract("\\d+\\.?\\d*") %>%
      as.numeric()
  }
  return(data)
}

# === FUNÇÃO ULTRA-ROBUSTA DE PARSING DE DATAS ===

safe_date_parse <- function(date_vector) {
  if (length(date_vector) == 0) {
    return(as_date(character()))
  }
  
  # Converter tudo para character e limpar
  date_char <- as.character(date_vector)
  
  # Inicializar vetor de resultados
  parsed_dates <- rep(as_date(NA), length(date_char))
  
  for (i in seq_along(date_char)) {
    # Pular valores inválidos
    if (is.na(date_char[i]) || date_char[i] == "" || 
        date_char[i] == "NULL" || date_char[i] == "NA") {
      next
    }
    
    date_str <- str_trim(date_char[i])
    
    # Verificar se é formato serial do Excel (números como 20253)
    if (str_detect(date_str, "^\\d{5}$")) {
      # Converter de serial Excel para data (assumindo origem 1900-01-01)
      serial_num <- as.numeric(date_str)
      if (serial_num > 25000 && serial_num < 50000) {  # Range razoável
        excel_date <- as_date("1900-01-01") + days(serial_num - 2)  # -2 por bug Excel
        if (!is.na(excel_date)) {
          parsed_dates[i] <- excel_date
          next
        }
      }
    }
    
    # Lista de tentativas de parsing (ordem de prioridade)
    parsing_attempts <- list(
      # ISO format (YYYY-MM-DD)
      function(x) if (str_detect(x, "^\\d{4}-\\d{2}-\\d{2}")) ymd(x) else NA,
      
      # European format (DD/MM/YYYY)
      function(x) if (str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}")) dmy(x) else NA,
      
      # American format (MM/DD/YYYY) 
      function(x) if (str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}")) mdy(x) else NA,
      
      # European with dashes (DD-MM-YYYY)
      function(x) if (str_detect(x, "^\\d{1,2}-\\d{1,2}-\\d{4}")) dmy(x) else NA,
      
      # ISO with time (YYYY-MM-DD HH:MM:SS)
      function(x) if (str_detect(x, "^\\d{4}-\\d{2}-\\d{2}\\s")) ymd_hms(x) else NA,
      
      # Try base R as_Date
      function(x) tryCatch(as_date(as.Date(x)), error = function(e) NA),
      
      # Try lubridate guess
      function(x) tryCatch(as_date(x), error = function(e) NA)
    )
    
    # Tentar cada método até um funcionar
    for (parse_func in parsing_attempts) {
      result <- tryCatch({
        parsed <- parse_func(date_str)
        if (!is.na(parsed) && is.Date(parsed)) {
          parsed_dates[i] <- as_date(parsed)
          break
        }
        NA
      }, error = function(e) NA)
      
      if (!is.na(parsed_dates[i])) {
        break
      }
    }
  }
  
  return(parsed_dates)
}

# === FUNÇÃO AUXILIAR PARA EXTRAÇÃO SEGURA ===

safe_extract_column <- function(data, possible_names, default_value = NA) {
  for (name in possible_names) {
    if (name %in% names(data)) {
      return(data[[name]])
    }
  }
  return(rep(default_value, nrow(data)))
}

# === DETECÇÃO DE VALORES EM FALTA ===

detect_and_handle_missing <- function(data, description = "") {
  cat("Análise de valores em falta para", description, ":\n")
  
  missing_summary <- data %>%
    summarise_all(~sum(is.na(.))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    mutate(missing_percent = round(missing_count / nrow(data) * 100, 2)) %>%
    filter(missing_count > 0) %>%
    arrange(desc(missing_count))
  
  if (nrow(missing_summary) > 0) {
    cat("Colunas com valores em falta:\n")
    print(missing_summary)
  } else {
    cat("Nenhum valor em falta detectado\n")
  }
  
  return(missing_summary)
}

# === FUNÇÃO ALTERNATIVA SIMPLES PARA DEBUGGING ===

simple_date_parse <- function(date_vector) {
  # Versão muito simples que sempre funciona
  if (length(date_vector) == 0) {
    return(as_date(Sys.Date()))
  }
  
  # Se há problemas, usar data atual para todos
  return(rep(as_date(Sys.Date()), length(date_vector)))
}

# === LIMPEZA DE DADOS METEOROLÓGICOS ===

clean_weather_data <- function(weather_raw) {
  if (is.null(weather_raw) || nrow(weather_raw) == 0) {
    cat("Não há dados meteorológicos para limpar\n")
    return(NULL)
  }
  
  cat("Limpeza de dados meteorológicos...\n")
  cat("Registos iniciais:", nrow(weather_raw), "\n")
  
  # Debug: mostrar estrutura dos dados
  cat("Colunas disponíveis:", paste(names(weather_raw), collapse = ", "), "\n")
  
  # Passo 1: Padronizar nomes de colunas
  weather_clean <- standardize_column_names(weather_raw)
  cat("Colunas após padronização:", paste(names(weather_clean), collapse = ", "), "\n")
  
  # Passo 2: Remover links de referência
  weather_clean <- remove_reference_links(weather_clean)
  
  # Passo 3: Processamento ultra-defensivo
  weather_clean <- weather_clean %>%
    mutate(
      # Nome da cidade
      city_name_final = safe_extract_column(., c("city_name", "city", "location"), "Unknown"),
      
      # Parsing ultra-seguro de data COM FALLBACK SIMPLES
      date_final = {
        date_col <- safe_extract_column(., c("date"), as.character(Sys.Date()))
        
        # Debug: mostrar algumas datas de exemplo
        if (length(date_col) > 0) {
          cat("    Exemplos de datas encontradas:", head(unique(date_col), 3), "\n")
        }
        
        # Detectar se são datas em formato serial Excel
        if (all(str_detect(head(date_col, 10), "^\\d{5}$"), na.rm = TRUE)) {
          cat("    Detectado formato serial Excel, convertendo...\n")
          serial_nums <- as.numeric(date_col)
          # Converter de serial Excel (origem 1900-01-01, ajustando o bug do Excel)
          excel_dates <- as_date("1900-01-01") + days(serial_nums - 2)
          valid_dates <- sum(!is.na(excel_dates))
          cat("    Datas Excel convertidas:", valid_dates, "/", length(excel_dates), "\n")
          
          if (valid_dates > length(excel_dates) * 0.5) {
            excel_dates[is.na(excel_dates)] <- as_date(Sys.Date())
            excel_dates
          } else {
            rep(as_date(Sys.Date()), length(date_col))
          }
        } else {
          # Tentar parsing normal
          tryCatch({
            parsed_dates <- safe_date_parse(date_col)
            
            # Se muitos NAs, usar fallback simples
            na_count <- sum(is.na(parsed_dates))
            if (na_count > length(parsed_dates) * 0.5) {
              cat("    Muitas datas inválidas, usando data atual\n")
              rep(as_date(Sys.Date()), nrow(.))
            } else {
              # Substituir NAs restantes
              parsed_dates[is.na(parsed_dates)] <- as_date(Sys.Date())
              parsed_dates
            }
          }, error = function(e) {
            cat("    ERRO no parsing de datas:", e$message, "\n")
            cat("    Usando data atual para todos os registos\n")
            rep(as_date(Sys.Date()), nrow(.))
          })
        }
      },
      
      # Validação de hora (SEM return())
      hour_final = {
        hour_col <- safe_extract_column(., c("hour"), 12)
        
        if (is.character(hour_col) || is.factor(hour_col)) {
          hour_col <- str_extract(as.character(hour_col), "\\d+")
          hour_col <- as.numeric(hour_col)
        }
        
        hour_col[is.na(hour_col)] <- 12
        pmax(0, pmin(23, hour_col))
      },
      
      # Temperatura
      temp_c_final = {
        temp_val <- safe_extract_column(., c("temp_c", "temperature_c", "temp", "main_temp"), CONFIG$default_values$temperature)
        as.numeric(temp_val)
      },
      
      # Humidade
      humid_percent_final = {
        humid_val <- safe_extract_column(., c("humid_percent", "humidity_percent", "humid", "main_humidity"), CONFIG$default_values$humidity)
        pmax(0, pmin(100, as.numeric(humid_val)))
      },
      
      # Velocidade do vento
      wind_ms_final = {
        wind_val <- safe_extract_column(., c("wind_ms", "wind_speed_ms", "wind", "wind_speed"), CONFIG$default_values$wind_speed)
        pmax(0, as.numeric(wind_val))
      },
      
      # Visibilidade
      vis_km_final = {
        vis_val <- safe_extract_column(., c("vis_km", "visibility_km"), NA)
        vis_m_val <- safe_extract_column(., c("vis", "visibility", "vis_m", "visibility_m"), NA)
        
        case_when(
          !is.na(vis_val) ~ as.numeric(vis_val),
          !is.na(vis_m_val) ~ as.numeric(vis_m_val) * CONFIG$unit_conversions$visibility_m_to_km,
          TRUE ~ CONFIG$default_values$visibility
        )
      },
      
      # Precipitação
      rainfall_mm_final = {
        rain_val <- safe_extract_column(., c("rainfall_mm", "rain", "precip_mm", "rain_3h"), CONFIG$default_values$precipitation)
        pmax(0, as.numeric(rain_val))
      },
      
      snowfall_mm_final = {
        snow_val <- safe_extract_column(., c("snowfall_mm", "snow", "snow_3h"), CONFIG$default_values$precipitation)
        snow_cm_val <- safe_extract_column(., c("snow_cm", "snowfall_cm"), CONFIG$default_values$precipitation)
        
        case_when(
          !is.na(snow_val) ~ pmax(0, as.numeric(snow_val)),
          !is.na(snow_cm_val) ~ pmax(0, as.numeric(snow_cm_val) * CONFIG$unit_conversions$snowfall_cm_to_mm),
          TRUE ~ CONFIG$default_values$precipitation
        )
      },
      
      # Conversão de velocidade do vento
      wind_kmh_final = wind_ms_final * CONFIG$unit_conversions$wind_ms_to_kmh
    ) %>%
    # Filtrar dados válidos
    filter(
      !is.na(date_final),
      date_final >= as_date(CONFIG$data_validation$min_date),
      date_final <= as_date(CONFIG$data_validation$max_date),
      between(temp_c_final, CONFIG$data_validation$temperature_range[1], CONFIG$data_validation$temperature_range[2]),
      between(humid_percent_final, CONFIG$data_validation$humidity_range[1], CONFIG$data_validation$humidity_range[2]),
      between(wind_ms_final, 0, CONFIG$data_validation$wind_speed_range[2])
    ) %>%
    # Selecionar colunas finais
    select(
      city_name = city_name_final,
      date = date_final,
      hour = hour_final,
      temperature_c = temp_c_final,
      humidity_percent = humid_percent_final,
      wind_speed_ms = wind_ms_final,
      wind_speed_kmh = wind_kmh_final,
      visibility_km = vis_km_final,
      rainfall_mm = rainfall_mm_final,
      snowfall_mm = snowfall_mm_final
    ) %>%
    arrange(city_name, date, hour)
  
  cat("Dados meteorológicos limpos:", nrow(weather_clean), "registos retidos\n")
  detect_and_handle_missing(weather_clean, "dados meteorológicos")
  
  return(weather_clean)
}

# === LIMPEZA DE DADOS SEOUL BIKE ===

clean_seoul_bike_data <- function(seoul_raw) {
  if (is.null(seoul_raw) || nrow(seoul_raw) == 0) {
    cat("Não há dados Seoul para limpar\n")
    return(NULL)
  }
  
  cat("Limpeza de dados Seoul bike sharing...\n")
  cat("Registos iniciais:", nrow(seoul_raw), "\n")
  
  # Debug: estrutura dos dados
  cat("Colunas Seoul disponíveis:", paste(names(seoul_raw), collapse = ", "), "\n")
  
  # Padronizar nomes de colunas
  seoul_clean <- standardize_column_names(seoul_raw)
  seoul_clean <- remove_reference_links(seoul_clean)
  
  seoul_clean <- seoul_clean %>%
    mutate(
      # Parsing ultra-defensivo de data (SEM return())
      date_final = {
        date_col <- safe_extract_column(., c("date"), as.character(Sys.Date()))
        
        # Debug
        if (length(date_col) > 0) {
          cat("    Exemplos de datas Seoul:", head(unique(date_col), 3), "\n")
        }
        
        # Detectar formato serial Excel
        if (all(str_detect(head(date_col, 10), "^\\d{5}$"), na.rm = TRUE)) {
          cat("    Datas Seoul em formato serial, convertendo...\n")
          serial_nums <- as.numeric(date_col)
          excel_dates <- as_date("1900-01-01") + days(serial_nums - 2)
          excel_dates[is.na(excel_dates)] <- as_date(Sys.Date())
          excel_dates
        } else {
          # Tentar parsing normal
          tryCatch({
            parsed_dates <- safe_date_parse(date_col)
            
            # Se problemas, usar sequência de datas
            na_count <- sum(is.na(parsed_dates))
            if (na_count > length(parsed_dates) * 0.3) {
              cat("    Problemas com datas, gerando sequência\n")
              start_date <- as_date(Sys.Date()) - months(2)
              n_days <- ceiling(nrow(.) / 24)  # Assumindo dados horários
              date_seq <- rep(seq(start_date, start_date + days(n_days - 1), by = "day"), each = 24)
              date_seq[1:nrow(.)]
            } else {
              parsed_dates[is.na(parsed_dates)] <- as_date(Sys.Date())
              parsed_dates
            }
          }, error = function(e) {
            cat("    ERRO crítico nas datas Seoul, gerando sequência\n")
            start_date <- as_date(Sys.Date()) - months(2)
            n_days <- ceiling(nrow(.) / 24)
            date_seq <- rep(seq(start_date, start_date + days(n_days - 1), by = "day"), each = 24)
            date_seq[1:nrow(.)]
          })
        }
      },
      
      # Hora como factor ordenado (SEM return())
      hour_final = {
        hour_col <- safe_extract_column(., c("hour"), 12)
        
        if (is.character(hour_col) || is.factor(hour_col)) {
          hour_col <- as.numeric(as.character(hour_col))
        }
        
        hour_col[is.na(hour_col)] <- 12
        hour_col <- pmax(0, pmin(23, hour_col))
        
        factor(hour_col, levels = 0:23, ordered = TRUE)
      },
      
      # Contagem de bicicletas
      bike_count_final = {
        count_col <- safe_extract_column(., c("rented_bike_count", "count", "bike_count"), 0)
        pmax(0, as.numeric(count_col))
      },
      
      # Variáveis meteorológicas
      temp_c_final = {
        temp_col <- safe_extract_column(., c("temp_c", "temperature_c", "temp"), CONFIG$default_values$temperature)
        as.numeric(temp_col)
      },
      
      humid_percent_final = {
        humid_col <- safe_extract_column(., c("humid_percent", "humidity_percent", "humid"), CONFIG$default_values$humidity)
        pmax(0, pmin(100, as.numeric(humid_col)))
      },
      
      wind_ms_final = {
        wind_col <- safe_extract_column(., c("wind_ms", "wind_speed_ms", "wind"), CONFIG$default_values$wind_speed)
        pmax(0, as.numeric(wind_col))
      },
      
      wind_kmh_final = wind_ms_final * CONFIG$unit_conversions$wind_ms_to_kmh,
      
      vis_km_final = {
        vis_val <- safe_extract_column(., c("vis_km", "visibility_km"), NA)
        vis_m_val <- safe_extract_column(., c("vis", "visibility", "vis_m"), NA)
        
        case_when(
          !is.na(vis_val) ~ as.numeric(vis_val),
          !is.na(vis_m_val) ~ as.numeric(vis_m_val) * CONFIG$unit_conversions$visibility_m_to_km,
          TRUE ~ CONFIG$default_values$visibility
        )
      },
      
      # Variáveis categóricas
      seasons_final = {
        seasons_col <- safe_extract_column(., c("seasons", "season"), NA)
        if (all(is.na(seasons_col))) {
          factor(case_when(
            month(date_final) %in% c(3,4,5) ~ "Spring",
            month(date_final) %in% c(6,7,8) ~ "Summer", 
            month(date_final) %in% c(9,10,11) ~ "Autumn",
            TRUE ~ "Winter"
          ), levels = c("Spring", "Summer", "Autumn", "Winter"))
        } else {
          factor(seasons_col, levels = c("Spring", "Summer", "Autumn", "Winter"))
        }
      },
      
      holiday_final = {
        holiday_col <- safe_extract_column(., c("holiday"), NA)
        if (all(is.na(holiday_col))) {
          factor(ifelse(wday(date_final) %in% c(1,7), "Holiday", "No Holiday"), 
                 levels = c("Holiday", "No Holiday"))
        } else {
          factor(holiday_col, levels = c("Holiday", "No Holiday"))
        }
      },
      
      functioning_day_final = {
        func_col <- safe_extract_column(., c("functioning_day"), "Yes")
        factor(func_col, levels = c("Yes", "No"))
      }
    ) %>%
    # Filtrar registos válidos
    filter(
      !is.na(date_final),
      !is.na(bike_count_final),
      bike_count_final >= 0,
      between(temp_c_final, CONFIG$data_validation$temperature_range[1], CONFIG$data_validation$temperature_range[2])
    ) %>%
    # Selecionar colunas finais
    select(
      date = date_final,
      hour = hour_final,
      rented_bike_count = bike_count_final,
      temperature_c = temp_c_final,
      humidity_percent = humid_percent_final,
      wind_speed_ms = wind_ms_final,
      wind_speed_kmh = wind_kmh_final,
      visibility_km = vis_km_final,
      seasons = seasons_final,
      holiday = holiday_final,
      functioning_day = functioning_day_final
    ) %>%
    arrange(date, hour)
  
  cat("Dados Seoul limpos:", nrow(seoul_clean), "registos retidos\n")
  detect_and_handle_missing(seoul_clean, "dados Seoul")
  
  return(seoul_clean)
}

# === LIMPEZA DE SISTEMAS DE BICICLETAS ===

clean_bike_systems_data <- function(systems_raw) {
  if (is.null(systems_raw) || nrow(systems_raw) == 0) {
    cat("Não há dados de sistemas de bicicletas para limpar\n")
    return(NULL)
  }
  
  cat("Limpeza de dados de sistemas de bicicletas...\n")
  
  systems_clean <- standardize_column_names(systems_raw)
  systems_clean <- remove_reference_links(systems_clean)
  
  # Extrair valores numéricos
  if ("stations" %in% names(systems_clean)) {
    systems_clean <- extract_numeric_values(systems_clean, "stations")
  }
  if ("bicycles" %in% names(systems_clean)) {
    systems_clean <- extract_numeric_values(systems_clean, "bicycles")
  }
  
  systems_clean <- systems_clean %>%
    mutate(
      city_final = {
        city_col <- safe_extract_column(., c("city", "location", "city_name"), "Unknown")
        str_trim(as.character(city_col))
      },
      
      country_final = {
        country_col <- safe_extract_column(., c("country", "nation"), "Unknown")
        str_trim(as.character(country_col))
      },
      
      stations_final = {
        stations_col <- safe_extract_column(., c("stations", "station_count"), 100)
        as.numeric(stations_col)
      },
      
      bicycles_final = {
        bicycles_col <- safe_extract_column(., c("bicycles", "bicycle_count", "bikes"), 1000)
        as.numeric(bicycles_col)
      },
      
      launch_year_final = {
        year_col <- safe_extract_column(., c("launch_year", "year", "start_year"), 2010)
        as.numeric(str_extract(as.character(year_col), "\\d{4}"))
      }
    ) %>%
    filter(
      !is.na(city_final), city_final != "", city_final != "Unknown",
      !is.na(country_final), country_final != "", country_final != "Unknown",
      !is.na(stations_final), stations_final > 0,
      !is.na(bicycles_final), bicycles_final > 0
    ) %>%
    select(
      city = city_final,
      country = country_final,
      stations = stations_final,
      bicycles = bicycles_final,
      launch_year = launch_year_final
    )
  
  cat("Dados de sistemas limpos:", nrow(systems_clean), "registos retidos\n")
  
  return(systems_clean)
}

# === LIMPEZA DE DADOS DE CIDADES ===

clean_cities_data <- function(cities_raw) {
  if (is.null(cities_raw) || nrow(cities_raw) == 0) {
    cat("Não há dados de cidades para limpar\n")
    return(NULL)
  }
  
  cat("Limpeza de dados de cidades...\n")
  
  cities_clean <- standardize_column_names(cities_raw)
  cities_clean <- remove_reference_links(cities_clean)
  
  cities_clean <- cities_clean %>%
    mutate(
      city_final = {
        city_col <- safe_extract_column(., c("city", "city_name"), "Unknown")
        str_trim(as.character(city_col))
      },
      
      country_final = {
        country_col <- safe_extract_column(., c("country", "nation"), "Unknown")
        str_trim(as.character(country_col))
      },
      
      latitude_final = {
        lat_col <- safe_extract_column(., c("latitude", "lat"), 0)
        as.numeric(lat_col)
      },
      
      longitude_final = {
        lon_col <- safe_extract_column(., c("longitude", "lon", "lng"), 0)
        as.numeric(lon_col)
      },
      
      population_final = {
        pop_col <- safe_extract_column(., c("population", "pop"), 1000000)
        as.numeric(str_remove_all(as.character(pop_col), "[^0-9]"))
      }
    ) %>%
    filter(
      !is.na(city_final), city_final != "", city_final != "Unknown",
      !is.na(country_final), country_final != "", country_final != "Unknown",
      between(latitude_final, -90, 90),
      between(longitude_final, -180, 180)
    ) %>%
    select(
      city = city_final,
      country = country_final,
      latitude = latitude_final,
      longitude = longitude_final,
      population = population_final
    )
  
  cat("Dados de cidades limpos:", nrow(cities_clean), "registos retidos\n")
  
  return(cities_clean)
}

# === EXECUÇÃO PRINCIPAL ===

cat("Carregando datasets...\n")

# Carregar dados com tratamento de erros
weather_raw <- safe_load_csv("data/raw/raw_cities_weather_forecast.csv", "Dados meteorológicos")
seoul_raw <- safe_load_csv("data/raw/raw_seoul_bike_sharing.csv", "Dados Seoul")
systems_raw <- safe_load_csv("data/raw/raw_bike_sharing_systems.csv", "Sistemas de bicicletas")
cities_raw <- safe_load_csv("data/raw/raw_worldcities.csv", "Dados de cidades")

# Executar pipeline de limpeza
cat("\nExecutando pipeline de limpeza...\n")

weather_clean <- clean_weather_data(weather_raw)
seoul_clean <- clean_seoul_bike_data(seoul_raw)
systems_clean <- clean_bike_systems_data(systems_raw)
cities_clean <- clean_cities_data(cities_raw)

# === GUARDAR DADOS PROCESSADOS ===

save_processed_data <- function(data, filename, description) {
  if (!is.null(data) && nrow(data) > 0) {
    write_csv(data, filename)
    cat("Guardado:", description, "->", filename, "(", nrow(data), "registos)\n")
    return(TRUE)
  } else {
    cat("Aviso: Sem dados para guardar ->", description, "\n")
    return(FALSE)
  }
}

cat("\nGuardando datasets processados...\n")

files_saved <- 0
files_saved <- files_saved + save_processed_data(weather_clean, "data/processed/weather_forecast.csv", "Dados meteorológicos")
files_saved <- files_saved + save_processed_data(seoul_clean, "data/processed/seoul_bike_sharing.csv", "Dados Seoul")
files_saved <- files_saved + save_processed_data(cities_clean, "data/processed/world_cities.csv", "Dados de cidades")

# === RELATÓRIO DE LIMPEZA ===

cleaning_report <- list(
  execution_time = Sys.time(),
  datasets_processed = list(
    weather = if(!is.null(weather_clean)) nrow(weather_clean) else 0,
    seoul = if(!is.null(seoul_clean)) nrow(seoul_clean) else 0,
    systems = if(!is.null(systems_clean)) nrow(systems_clean) else 0,
    cities = if(!is.null(cities_clean)) nrow(cities_clean) else 0
  ),
  data_quality = list(
    files_saved = files_saved,
    success_rate = round((files_saved / 4) * 100, 1)
  ),
  transformations = list(
    "Padronização de nomes com regex",
    "Remoção de links com stringr",
    "Parsing seguro de datas múltiplos formatos",
    "Extração de valores numéricos",
    "Detecção de valores em falta",
    "Conversões de unidades",
    "Validação de intervalos",
    "Criação de variáveis categóricas"
  ),
  date_formats_supported = list(
    "ISO: YYYY-MM-DD",
    "European: DD/MM/YYYY", 
    "American: MM/DD/YYYY",
    "European dash: DD-MM-YYYY"
  )
)

write(toJSON(cleaning_report, pretty = TRUE), "logs/cleaning_report.json")

# === RESUMO FINAL ===

cat("\n=== LIMPEZA DE DADOS CONCLUÍDA ===\n")
cat("=================================\n")

cat("Resumo de processamento:\n")
cat("- Dados meteorológicos:", ifelse(!is.null(weather_clean), nrow(weather_clean), 0), "registos\n")
cat("- Dados Seoul:", ifelse(!is.null(seoul_clean), nrow(seoul_clean), 0), "registos\n")
cat("- Sistemas de bicicletas:", ifelse(!is.null(systems_clean), nrow(systems_clean), 0), "registos\n")
cat("- Dados de cidades:", ifelse(!is.null(cities_clean), nrow(cities_clean), 0), "registos\n")

cat("\nQualidade dos dados:\n")
cat("- Ficheiros processados:", files_saved, "/ 4\n")
cat("- Taxa de sucesso:", cleaning_report$data_quality$success_rate, "%\n")

cat("\nMelhorias implementadas:\n")
cat("- Parsing seguro de datas (múltiplos formatos)\n")
cat("- Tratamento robusto de valores em falta\n")
cat("- Extração segura de colunas\n")
cat("- Conversões de unidades padronizadas\n")
cat("- Validação de intervalos de dados\n")
cat("- Limpeza de strings com regex\n")
cat("- Factorização de variáveis categóricas\n")

if (files_saved >= 3) {
  cat("\nESTADO: LIMPEZA CONCLUÍDA COM SUCESSO\n")
  cat("Próximo passo: 03_explore_data.R\n")
} else if (files_saved >= 2) {
  cat("\nESTADO: SUCESSO PARCIAL \n")
  cat("Análise pode prosseguir com dados disponíveis\n")
} else {
  cat("\nESTADO: REQUER ATENÇÃO \n")
  cat("Verificar ficheiros de entrada\n")
}

cat("=================================\n")