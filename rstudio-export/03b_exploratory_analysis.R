# 03b_exploratory_analysis.R - Parte 2 (Visualizações)

# Carregar pacotes
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, ggplot2, scales, gridExtra, viridis, ggcorrplot)

# Carregar os dados se necessário
seoul_bike <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE) %>%
  mutate(
    date = as_date(date),
    hour = factor(hour, levels = as.character(0:23), ordered = TRUE),
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

# 1. Série temporal da contagem de bicicletas
plot_time_series <- ggplot(seoul_bike, aes(x = date, y = rented_bike_count)) +
  geom_line(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Evolução da Procura de Bicicletas ao Longo do Tempo",
    subtitle = "Dados de Seoul Bike Sharing",
    x = "Data", y = "Contagem de Bicicletas Alugadas"
  ) +
  theme_minimal()

ggsave("outputs/plots/time_series_plot.png", plot_time_series, width = 10, height = 6, dpi = 300)

# 2. Histograma com densidade
plot_histogram <- ggplot(seoul_bike, aes(x = rented_bike_count)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribuição da Contagem de Bicicletas Alugadas",
    x = "Contagem de Bicicletas", y = "Densidade"
  ) +
  theme_minimal()

ggsave("outputs/plots/histogram_density.png", plot_histogram, width = 8, height = 6, dpi = 300)

# 3. Contagem por hora do dia, colorido por estação
hourly_season_plot <- ggplot(seoul_bike, aes(x = hour, y = rented_bike_count, fill = seasons)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(
    title = "Distribuição de Bicicletas Alugadas por Hora e Estação",
    x = "Hora do Dia", y = "Contagem de Bicicletas",
    fill = "Estação"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/plots/hourly_by_season.png", hourly_season_plot, width = 10, height = 6, dpi = 300)

# 4. Correlação entre variáveis numéricas
corr_vars <- seoul_bike %>%
  select(rented_bike_count, temperature_c, humidity_percent, wind_speed_m_s,
         visibility_10m, dew_point_temperature_c, solar_radiation_mj_m2,
         rainfall_mm, snowfall_cm)

cor_matrix <- cor(corr_vars, use = "pairwise.complete.obs")

corr_plot <- ggcorrplot(
  cor_matrix,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  colors = c("#6D9EC1", "white", "#E46726"),
  title = "Correlação entre Variáveis"
)

ggsave("outputs/plots/correlation_plot.png", corr_plot, width = 9, height = 8, dpi = 300)

# 5. Gráfico de dispersão: Temperatura vs Demanda, colorido por hora
temp_demand_plot <- ggplot(seoul_bike, aes(x = temperature_c, y = rented_bike_count, color = hour)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  scale_color_viridis_d() +
  labs(
    title = "Relação entre Temperatura e Demanda de Bicicletas",
    subtitle = "Colorido por Hora do Dia",
    x = "Temperatura (°C)", y = "Contagem de Bicicletas",
    color = "Hora"
  ) +
  theme_minimal()

ggsave("outputs/plots/temp_vs_demand.png", temp_demand_plot, width = 10, height = 6, dpi = 300)

# 6. Demanda média por dia da semana e período do dia
weekday_period_avg <- seoul_bike %>%
  group_by(weekday, day_period) %>%
  summarise(
    avg_bikes = mean(rented_bike_count, na.rm = TRUE),
    .groups = "drop"
  )

weekday_period_plot <- ggplot(weekday_period_avg, 
                              aes(x = weekday, y = avg_bikes, fill = day_period)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  labs(
    title = "Média de Bicicletas Alugadas por Dia da Semana e Período",
    x = "Dia da Semana", y = "Média de Bicicletas",
    fill = "Período do Dia"
  ) +
  theme_minimal()

ggsave("outputs/plots/weekday_period_avg.png", weekday_period_plot, width = 10, height = 6, dpi = 300)