library(tidymodels)
library(readr)
library(dplyr)

# Criar diretório para guardar modelos
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)

# Carregar os dados
seoul_bike <- read_csv("data/processed/seoul_bike_sharing.csv")

# Separar treino e teste
set.seed(123)
data_split <- initial_split(seoul_bike, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Definir receita (corrigido)
recipe_weather <- recipe(rented_bike_count ~ temperature_c + humidity_percent +
                           wind_speed_m_s + visibility_10m +
                           dew_point_temperature_c + solar_radiation_mj_m2 +
                           rainfall_mm + snowfall_cm,
                         data = train_data) %>%
  step_normalize(all_numeric_predictors())

# Definir modelo
model_lm <- linear_reg() %>%
  set_engine("lm")

# Criar workflow
workflow_weather <- workflow() %>%
  add_recipe(recipe_weather) %>%
  add_model(model_lm)

# Ajustar modelo
fit_weather <- fit(workflow_weather, data = train_data)

# Avaliar resultados
results_weather <- predict(fit_weather, new_data = test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = rented_bike_count, estimate = .pred)

print(results_weather)

# Guardar modelo
saveRDS(fit_weather, "outputs/models/weather_model.rds")
