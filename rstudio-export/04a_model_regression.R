# 04_model.R - versão corrigida

# Carregar pacotes
library(tidymodels)
library(readr)
library(dplyr)
library(lubridate)

# Criar diretório para guardar modelos
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)

# Carregar os dados
seoul_bike <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE)

# Verificar os dados
cat("Número de linhas:", nrow(seoul_bike), "\n")
cat("Colunas disponíveis:", paste(colnames(seoul_bike), collapse = ", "), "\n")

# Separar treino e teste
set.seed(123)
data_split <- initial_split(seoul_bike, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

cat("Dados de treino:", nrow(train_data), "linhas\n")
cat("Dados de teste:", nrow(test_data), "linhas\n")

# Definir receita base (sem interações problemáticas)
model_recipe <- recipe(rented_bike_count ~ temperature_c + humidity_percent +
                         wind_speed_m_s + visibility_10m +
                         dew_point_temperature_c + solar_radiation_mj_m2 +
                         rainfall_mm + snowfall_cm + hour +
                         seasons + holiday + functioning_day,
                       data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Definir modelo de Random Forest
rf_spec <- rand_forest(
  mtry = 8,           # Número de variáveis a considerar em cada divisão
  trees = 300,        # Número de árvores
  min_n = 5           # Tamanho mínimo do nó
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Definir modelo linear simples para comparação
lm_spec <- linear_reg() %>%
  set_engine("lm")

# Criar workflows
rf_workflow <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(rf_spec)

lm_workflow <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(lm_spec)

# Treinar modelos
cat("Treinando modelo Random Forest...\n")
rf_fit <- fit(rf_workflow, data = train_data)

cat("Treinando modelo Linear...\n")
lm_fit <- fit(lm_workflow, data = train_data)

# Avaliar no conjunto de teste
rf_results <- predict(rf_fit, new_data = test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = rented_bike_count, estimate = .pred)

lm_results <- predict(lm_fit, new_data = test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = rented_bike_count, estimate = .pred)

cat("\nResultados do modelo Random Forest:\n")
print(rf_results)

cat("\nResultados do modelo Linear:\n")
print(lm_results)

# Determinar o melhor modelo
rf_rmse <- rf_results %>% filter(.metric == "rmse") %>% pull(.estimate)
lm_rmse <- lm_results %>% filter(.metric == "rmse") %>% pull(.estimate)

best_model <- if(rf_rmse < lm_rmse) rf_fit else lm_fit
best_model_name <- if(rf_rmse < lm_rmse) "Random Forest" else "Linear Regression"

cat("\nMelhor modelo:", best_model_name, "com RMSE de", 
    if(rf_rmse < lm_rmse) rf_rmse else lm_rmse, "\n")

# Guardar modelos
saveRDS(rf_fit, "outputs/models/rf_model.rds")
saveRDS(lm_fit, "outputs/models/lm_model.rds")
saveRDS(best_model, "outputs/models/best_model.rds")

cat("Modelos treinados e salvos com sucesso!\n")