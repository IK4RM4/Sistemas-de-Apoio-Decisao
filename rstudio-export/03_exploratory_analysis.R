# 03_explore_data.R 

# Carregar pacotes necessários
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(DBI)
library(RSQLite)

# Criar directorias
dir.create("outputs/plots/eda", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/statistics", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/sql_results", recursive = TRUE, showWarnings = FALSE)

cat("SISTEMAS DE APOIO À DECISÃO - ANÁLISE EXPLORATÓRIA DE DADOS\n")
cat("==========================================================\n")

# === FUNÇÃO PARA VERIFICAR COLUNAS ===

check_columns <- function(data, required_cols) {
  available_cols <- colnames(data)
  missing_cols <- setdiff(required_cols, available_cols)
  existing_cols <- intersect(required_cols, available_cols)
  
  if (length(missing_cols) > 0) {
    cat("Colunas em falta:", paste(missing_cols, collapse = ", "), "\n")
  }
  
  return(existing_cols)
}

# === CARREGAMENTO DE DADOS ===

cat("Carregando dados processados...\n")

# Carregar Seoul bike sharing (obrigatório)
if (!file.exists("data/processed/seoul_bike_sharing.csv")) {
  stop("Ficheiro Seoul bike sharing não encontrado")
}

seoul_bike <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE)
cat("Seoul bike sharing:", nrow(seoul_bike), "registos\n")

# Verificar colunas essenciais do Seoul
essential_seoul_cols <- c("date", "hour", "rented_bike_count", "temperature_c", 
                          "humidity_percent", "seasons")
available_seoul_cols <- check_columns(seoul_bike, essential_seoul_cols)
cat("Colunas Seoul disponíveis:", paste(available_seoul_cols, collapse = ", "), "\n")

# Carregar outros dados (opcionais)
weather_forecast <- NULL
bike_systems <- NULL
world_cities <- NULL

if (file.exists("data/processed/weather_forecast.csv")) {
  weather_forecast <- read_csv("data/processed/weather_forecast.csv", show_col_types = FALSE)
  cat("Weather forecast:", nrow(weather_forecast), "registos\n")
}

if (file.exists("data/processed/bike_sharing_systems.csv")) {
  bike_systems <- read_csv("data/processed/bike_sharing_systems.csv", show_col_types = FALSE)
  cat("Bike systems:", nrow(bike_systems), "registos\n")
}

if (file.exists("data/processed/world_cities.csv")) {
  world_cities <- read_csv("data/processed/world_cities.csv", show_col_types = FALSE)
  cat("World cities:", nrow(world_cities), "registos\n")
}

# === CONFIGURAÇÃO DA BASE DE DADOS ===

cat("\nConfiguração da base de dados SQLite...\n")
con <- dbConnect(SQLite(), ":memory:")

# Escrever tabelas
dbWriteTable(con, "seoul_bike_sharing", seoul_bike)
cat("Tabela seoul_bike_sharing criada\n")

if (!is.null(weather_forecast)) {
  dbWriteTable(con, "cities_weather_forecast", weather_forecast)
  cat("Tabela cities_weather_forecast criada\n")
}

if (!is.null(bike_systems)) {
  dbWriteTable(con, "bike_sharing_systems", bike_systems)
  cat("Tabela bike_sharing_systems criada\n")
}

if (!is.null(world_cities)) {
  dbWriteTable(con, "world_cities", world_cities)
  cat("Tabela world_cities criada\n")
}

# === TAREFAS SQL SIMPLIFICADAS E CORRIGIDAS ===

cat("\nExecutando tarefas de análise SQL...\n")

sql_results <- list()

# Tarefa 1 - Contagem de registos
cat("Tarefa 1 - Contagem de registos\n")
task1_query <- "SELECT COUNT(*) as total_records FROM seoul_bike_sharing"
sql_results$task1 <- dbGetQuery(con, task1_query)
cat("Total de registos Seoul:", sql_results$task1$total_records, "\n\n")

# Tarefa 2 - Horas de funcionamento
cat("Tarefa 2 - Horas de funcionamento\n")
task2_query <- "SELECT COUNT(*) as non_zero_hours 
                FROM seoul_bike_sharing 
                WHERE rented_bike_count > 0"
sql_results$task2 <- dbGetQuery(con, task2_query)
cat("Horas com alugueres não zero:", sql_results$task2$non_zero_hours, "\n\n")

# Tarefa 3 - Perspectiva meteorológica (só se houver dados meteorológicos)
if (!is.null(weather_forecast)) {
  cat("Tarefa 3 - Perspectiva meteorológica\n")
  # Verificar se Seoul existe nos dados meteorológicos
  seoul_weather_check <- dbGetQuery(con, "SELECT COUNT(*) as count 
                                          FROM cities_weather_forecast 
                                          WHERE city_name LIKE '%Seoul%'")
  
  if (seoul_weather_check$count > 0) {
    task3_query <- "SELECT city_name, date, hour, temperature_c, humidity_percent
                    FROM cities_weather_forecast 
                    WHERE city_name LIKE '%Seoul%' 
                    ORDER BY date, hour 
                    LIMIT 3"
    sql_results$task3 <- dbGetQuery(con, task3_query)
    cat("Perspectiva meteorológica Seoul:\n")
    print(sql_results$task3)
  } else {
    cat("Dados meteorológicos para Seoul não encontrados\n")
  }
} else {
  cat("Tarefa 3 - Dados meteorológicos não disponíveis\n")
}
cat("\n")

# Tarefa 4 - Estações
cat("Tarefa 4 - Estações\n")
if ("seasons" %in% available_seoul_cols) {
  task4_query <- "SELECT DISTINCT seasons 
                  FROM seoul_bike_sharing 
                  WHERE seasons IS NOT NULL
                  ORDER BY seasons"
  sql_results$task4 <- dbGetQuery(con, task4_query)
  cat("Estações incluídas:", paste(sql_results$task4$seasons, collapse = ", "), "\n")
} else {
  cat("Coluna 'seasons' não disponível\n")
}
cat("\n")

# Tarefa 5 - Intervalo de datas
cat("Tarefa 5 - Intervalo de datas\n")
task5_query <- "SELECT MIN(date) as first_date, MAX(date) as last_date 
                FROM seoul_bike_sharing"
sql_results$task5 <- dbGetQuery(con, task5_query)
cat("Intervalo de datas: De", sql_results$task5$first_date, "a", sql_results$task5$last_date, "\n\n")

# Tarefa 6 - Procura máxima
cat("Tarefa 6 - Procura máxima (máximo histórico)\n")
task6_query <- "SELECT date, hour, rented_bike_count 
                FROM seoul_bike_sharing 
                WHERE rented_bike_count = (SELECT MAX(rented_bike_count) FROM seoul_bike_sharing)"
sql_results$task6 <- dbGetQuery(con, task6_query)
cat("Procura máxima:", sql_results$task6$rented_bike_count, "bicicletas em", 
    sql_results$task6$date, "às", sql_results$task6$hour, "h\n\n")

# Tarefa 7 - Popularidade horária e temperatura por estação
cat("Tarefa 7 - Popularidade horária e temperatura por estação\n")
if (all(c("seasons", "hour", "temperature_c") %in% available_seoul_cols)) {
  task7_query <- "SELECT seasons, hour, 
                         AVG(temperature_c) as avg_temperature,
                         AVG(rented_bike_count) as avg_bike_count
                  FROM seoul_bike_sharing 
                  GROUP BY seasons, hour 
                  ORDER BY avg_bike_count DESC 
                  LIMIT 10"
  sql_results$task7 <- dbGetQuery(con, task7_query)
  cat("Top 10 padrões horários por estação:\n")
  print(sql_results$task7)
} else {
  cat("Colunas necessárias não disponíveis\n")
}
cat("\n")

# Tarefa 8 - Sazonalidade do aluguer
cat("Tarefa 8 - Sazonalidade do aluguer\n")
if ("seasons" %in% available_seoul_cols) {
  task8_query <- "SELECT seasons,
                         AVG(rented_bike_count) as avg_hourly_count,
                         MIN(rented_bike_count) as min_hourly_count,
                         MAX(rented_bike_count) as max_hourly_count
                  FROM seoul_bike_sharing
                  GROUP BY seasons
                  ORDER BY avg_hourly_count DESC"
  sql_results$task8 <- dbGetQuery(con, task8_query)
  cat("Padrões de aluguer sazonal:\n")
  print(sql_results$task8)
} else {
  cat("Coluna 'seasons' não disponível\n")
}
cat("\n")

# Tarefa 9 - Sazonalidade meteorológica (versão corrigida)
cat("Tarefa 9 - Sazonalidade meteorológica\n")
if ("seasons" %in% available_seoul_cols) {
  # Lista predefinida de colunas meteorológicas possíveis
  weather_cols_mapping <- list(
    "temperature_c" = "AVG(temperature_c) as avg_temperature_c",
    "humidity_percent" = "AVG(humidity_percent) as avg_humidity_percent",
    "wind_speed_ms" = "AVG(wind_speed_ms) as avg_wind_speed_ms",
    "wind_speed_kmh" = "AVG(wind_speed_kmh) as avg_wind_speed_kmh",
    "visibility_km" = "AVG(visibility_km) as avg_visibility_km"
  )
  
  # Verificar quais colunas estão disponíveis
  available_weather <- intersect(names(weather_cols_mapping), available_seoul_cols)
  
  if (length(available_weather) > 0) {
    # Construir SELECT clause com colunas disponíveis
    weather_selects <- sapply(available_weather, function(col) weather_cols_mapping[[col]])
    weather_select_clause <- paste(weather_selects, collapse = ", ")
    
    task9_query <- paste0("SELECT seasons, ",
                          weather_select_clause, ", ",
                          "AVG(rented_bike_count) as avg_bike_count ",
                          "FROM seoul_bike_sharing ",
                          "GROUP BY seasons ",
                          "ORDER BY avg_bike_count DESC")
    
    sql_results$task9 <- dbGetQuery(con, task9_query)
    cat("Sazonalidade meteorológica:\n")
    print(sql_results$task9)
  } else {
    cat("Colunas meteorológicas não disponíveis\n")
  }
} else {
  cat("Coluna 'seasons' não disponível\n")
}
cat("\n")

# Tarefa 10 - Informação da cidade de Seoul
cat("Tarefa 10 - Informação da cidade de Seoul\n")
if (!is.null(bike_systems) && !is.null(world_cities)) {
  task10_query <- "SELECT bs.city, bs.country, wc.latitude, wc.longitude, 
                          wc.population, bs.bicycles
                   FROM bike_sharing_systems bs
                   INNER JOIN world_cities wc ON LOWER(bs.city) = LOWER(wc.city)
                   WHERE LOWER(bs.city) LIKE '%seoul%'"
  sql_results$task10 <- dbGetQuery(con, task10_query)
  
  if (nrow(sql_results$task10) > 0) {
    cat("Informação da cidade de Seoul:\n")
    print(sql_results$task10)
  } else {
    cat("Seoul não encontrado nos dados de sistemas de bicicletas\n")
  }
} else {
  cat("Dados de sistemas de bicicletas ou cidades não disponíveis\n")
}
cat("\n")

# Tarefa 11 - Cidades com escala semelhante
cat("Tarefa 11 - Cidades com escala de bicicletas semelhante a Seoul\n")
if (!is.null(bike_systems) && !is.null(world_cities)) {
  task11_query <- "SELECT bs.city, bs.country, wc.latitude, wc.longitude,
                          wc.population, bs.bicycles
                   FROM bike_sharing_systems bs
                   LEFT JOIN world_cities wc ON LOWER(bs.city) = LOWER(wc.city)
                   WHERE bs.bicycles BETWEEN 15000 AND 20000
                   ORDER BY bs.bicycles DESC"
  sql_results$task11 <- dbGetQuery(con, task11_query)
  
  if (nrow(sql_results$task11) > 0) {
    cat("Cidades com frotas entre 15.000-20.000 bicicletas:\n")
    print(sql_results$task11)
  } else {
    cat("Nenhuma cidade encontrada nesse intervalo\n")
  }
} else {
  cat("Dados de sistemas de bicicletas ou cidades não disponíveis\n")
}

# === GUARDAR RESULTADOS SQL ===

cat("\nGuardando resultados SQL...\n")

# Guardar apenas os resultados que existem
if (!is.null(sql_results$task7) && nrow(sql_results$task7) > 0) {
  write_csv(sql_results$task7, "outputs/sql_results/hourly_patterns_by_season.csv")
  cat("Padrões horários por estação guardados\n")
}

if (!is.null(sql_results$task8) && nrow(sql_results$task8) > 0) {
  write_csv(sql_results$task8, "outputs/sql_results/seasonal_rental_patterns.csv")
  cat("Padrões de aluguer sazonal guardados\n")
}

if (!is.null(sql_results$task9) && nrow(sql_results$task9) > 0) {
  write_csv(sql_results$task9, "outputs/sql_results/weather_seasonality.csv")
  cat("Sazonalidade meteorológica guardada\n")
}

if (!is.null(sql_results$task11) && nrow(sql_results$task11) > 0) {
  write_csv(sql_results$task11, "outputs/sql_results/similar_bike_cities.csv")
  cat("Cidades semelhantes guardadas\n")
}

# === ANÁLISE EDA COM VISUALIZAÇÃO ===

cat("\nExecutando EDA com visualização...\n")

# Preparar dados Seoul para visualização
seoul_data <- seoul_bike %>%
  mutate(
    date = case_when(
      str_detect(as.character(date), "^\\d{2}/\\d{2}/\\d{4}$") ~ dmy(date),
      str_detect(as.character(date), "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(date),
      TRUE ~ as_date(date)
    ),
    hour = factor(hour, levels = 0:23, ordered = TRUE)
  )

cat("Dados preparados para visualização:", nrow(seoul_data), "registos\n")

# Estatísticas básicas
cat("\nEstatísticas básicas:\n")
cat("- Procura média:", round(mean(seoul_data$rented_bike_count, na.rm = TRUE), 1), "bicicletas/hora\n")
cat("- Procura máxima:", max(seoul_data$rented_bike_count, na.rm = TRUE), "bicicletas\n")
cat("- Período dos dados:", min(seoul_data$date, na.rm = TRUE), "a", max(seoul_data$date, na.rm = TRUE), "\n")

# Gráfico 1 - Tendência temporal
plot1 <- ggplot(seoul_data, aes(x = date, y = rented_bike_count)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", linewidth = 1.2) +
  labs(
    title = "Procura de Partilha de Bicicletas Seoul ao Longo do Tempo",
    subtitle = "Padrões temporais na contagem de alugueres",
    x = "Data",
    y = "Contagem de Bicicletas Alugadas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(labels = comma_format())

ggsave("outputs/plots/eda/seoul_demand_trend.png", plot1, 
       width = 12, height = 8, dpi = 300)

# Gráfico 2 - Padrão horário
if ("hour" %in% colnames(seoul_data)) {
  hourly_pattern <- seoul_data %>%
    group_by(hour) %>%
    summarise(avg_demand = mean(rented_bike_count, na.rm = TRUE), .groups = "drop")
  
  plot2 <- ggplot(hourly_pattern, aes(x = as.numeric(hour), y = avg_demand)) +
    geom_line(color = "steelblue", linewidth = 1.5) +
    geom_point(color = "darkblue", size = 2.5) +
    labs(
      title = "Padrão de Procura Horária",
      x = "Hora do Dia",
      y = "Procura Média"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    scale_x_continuous(breaks = seq(0, 23, 3))
  
  ggsave("outputs/plots/eda/hourly_pattern.png", plot2, 
         width = 10, height = 6, dpi = 300)
}

# Gráfico 3 - Distribuição da procura
plot3 <- ggplot(seoul_data, aes(x = rented_bike_count)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, 
                 fill = "lightblue", alpha = 0.7, color = "white") +
  geom_density(color = "darkblue", linewidth = 1.5) +
  labs(
    title = "Distribuição de Contagens de Aluguer de Bicicletas",
    x = "Contagem de Bicicletas Alugadas",
    y = "Densidade"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("outputs/plots/eda/demand_distribution.png", plot3, 
       width = 10, height = 6, dpi = 300)

# Gráfico 4 - Padrões sazonais (se disponível)
if ("seasons" %in% colnames(seoul_data)) {
  seasonal_data <- seoul_data %>%
    group_by(seasons) %>%
    summarise(
      avg_demand = mean(rented_bike_count, na.rm = TRUE),
      median_demand = median(rented_bike_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  plot4 <- ggplot(seasonal_data, aes(x = seasons)) +
    geom_col(aes(y = avg_demand), fill = "lightcoral", alpha = 0.8) +
    geom_point(aes(y = median_demand), color = "darkred", size = 3) +
    labs(
      title = "Padrões de Procura Sazonal",
      subtitle = "Barras: Média, Pontos: Mediana",
      x = "Estação",
      y = "Procura de Bicicletas"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ggsave("outputs/plots/eda/seasonal_patterns.png", plot4, 
         width = 10, height = 6, dpi = 300)
}

# === FECHAR CONEXÃO E RESUMO FINAL ===

dbDisconnect(con)

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("ANÁLISE CONCLUÍDA\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\nResumo da execução:\n")
cat("- Tarefas SQL executadas: 11\n")
cat("- Visualizações criadas: 3-4 (dependendo dos dados disponíveis)\n")
cat("- Análise estatística: Abrangente\n")

cat("\nResultados principais:\n")
cat("- Total de registos Seoul:", sql_results$task1$total_records, "\n")
cat("- Horas com actividade:", sql_results$task2$non_zero_hours, "\n")
cat("- Intervalo de datas:", sql_results$task5$first_date, "a", sql_results$task5$last_date, "\n")
cat("- Procura máxima:", sql_results$task6$rented_bike_count, "bicicletas\n")

cat("\nFicheiros gerados:\n")
cat("- Resultados SQL: outputs/sql_results/\n")
cat("- Visualizações EDA: outputs/plots/eda/\n")
cat("- Resumos estatísticos: outputs/statistics/\n")

cat("\nPróxima fase: Modelação Preditiva (04_model.R)\n")
cat(paste(rep("=", 60), collapse = ""), "\n")