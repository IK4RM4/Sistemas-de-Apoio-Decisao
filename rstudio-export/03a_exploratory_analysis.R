# 03a_exploratory_analysis.R - Parte 1

# Carregar pacotes
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, ggplot2, scales, gridExtra, viridis, 
               DBI, RSQLite, corrr, GGally)

# Criar diretórios
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/analysis", recursive = TRUE, showWarnings = FALSE)

# Carregar dados limpos
seoul_bike <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE)
bike_systems <- read_csv("data/processed/bike_sharing_systems.csv", show_col_types = FALSE)
weather_forecast <- read_csv("data/processed/weather_forecast.csv", show_col_types = FALSE)

# Reformular data e hora
seoul_bike <- seoul_bike %>%
  mutate(
    date = as_date(date),
    hour = factor(hour, levels = as.character(0:23), ordered = TRUE),
    # Criar variáveis adicionais úteis para a análise
    weekday = wday(date, label = TRUE),
    month = month(date, label = TRUE),
    is_weekend = weekday %in% c("Sat", "Sun"),
    day_period = case_when(
      hour %in% 5:11 ~ "Manhã",
      hour %in% 12:17 ~ "Tarde", 
      hour %in% 18:22 ~ "Noite",
      TRUE ~ "Madrugada"
    ),
    day_period = factor(day_period, levels = c("Madrugada", "Manhã", "Tarde", "Noite"))
  )

# Análise de valores ausentes
missing_data <- seoul_bike %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Percent = Missing_Count / nrow(seoul_bike) * 100) %>%
  arrange(desc(Missing_Count))

write_csv(missing_data, "outputs/analysis/missing_data_summary.csv")

# Estatísticas descritivas básicas
summary_stats <- seoul_bike %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    Min = min(value, na.rm = TRUE),
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Median = median(value, na.rm = TRUE),
    Mean = mean(value, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    Max = max(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Skewness = (mean(value, na.rm = TRUE) - median(value, na.rm = TRUE)) / sd(value, na.rm = TRUE),
    N_missing = sum(is.na(value))
  )

write_csv(summary_stats, "outputs/analysis/numeric_summary_stats.csv")

# Análise por período (Dia/Noite, Estação, Dia da semana)
period_analysis <- seoul_bike %>%
  group_by(seasons, day_period) %>%
  summarise(
    avg_bikes = mean(rented_bike_count, na.rm = TRUE),
    median_bikes = median(rented_bike_count, na.rm = TRUE),
    sd_bikes = sd(rented_bike_count, na.rm = TRUE),
    min_bikes = min(rented_bike_count, na.rm = TRUE),
    max_bikes = max(rented_bike_count, na.rm = TRUE),
    records = n(),
    .groups = "drop"
  ) %>%
  arrange(seasons, day_period)

write_csv(period_analysis, "outputs/analysis/period_analysis.csv")

# Análise por dia da semana
weekday_analysis <- seoul_bike %>%
  group_by(weekday, holiday) %>%
  summarise(
    avg_bikes = mean(rented_bike_count, na.rm = TRUE),
    median_bikes = median(rented_bike_count, na.rm = TRUE),
    sd_bikes = sd(rented_bike_count, na.rm = TRUE),
    records = n(),
    .groups = "drop"
  ) %>%
  arrange(weekday, holiday)

write_csv(weekday_analysis, "outputs/analysis/weekday_analysis.csv")