
# 03_exploratory_analysis.R

# Instalar e carregar pacotes
# install.packages(c("tidyverse", "lubridate", "ggplot2", "scales"))
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# Criar diretório de plots
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

# Carregar dados limpos
seoul_bike <- read_csv("data/processed/seoul_bike_sharing.csv")

# Reformular data
seoul_bike <- seoul_bike %>%
  mutate(date = as_date(date, format = "%Y-%m-%d"))

# Hora como categórica ordenada
seoul_bike$hour <- factor(seoul_bike$hour, levels = as.character(0:23), ordered = TRUE)

# Gráfico 1
plot1 <- ggplot(seoul_bike, aes(x = date, y = rented_bike_count)) +
  geom_point(alpha = 0.3)
ggsave("outputs/plots/rented_vs_date.png", plot = plot1)

# Gráfico 2
plot2 <- ggplot(seoul_bike, aes(x = date, y = rented_bike_count, color = hour)) +
  geom_point(alpha = 0.5)
ggsave("outputs/plots/rented_vs_date_hour_colored.png", plot = plot2)

# Gráfico 3
plot3 <- ggplot(seoul_bike, aes(x = rented_bike_count)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", alpha = 0.7) +
  geom_density(color = "darkblue")
ggsave("outputs/plots/rented_hist_density.png", plot = plot3)


cat("Análise exploratória concluída.")
