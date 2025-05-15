# 04b_model_regression.R - Parte 2

# Carregar pacotes
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidymodels, readr, dplyr, lubridate, vip, ggplot2, gridExtra)

# Carregar modelos ajustados (supondo que 04a_model_regression.R já foi executado)
models_dir <- "outputs/models"
workflows_fitted <- list()
for (model_file in list.files(models_dir, pattern = ".rds")) {
  if (model_file != "model_evaluation_results.rds") {
    model_name <- gsub(".rds$", "", model_file)
    workflows_fitted[[model_name]] <- readRDS(file.path(models_dir, model_file))
  }
}

results <- readRDS("outputs/models/model_evaluation_results.rds")

# Selecionar o melhor modelo baseado no RMSE
model_rmse <- sapply(results, function(x) x$.estimate[1])
best_model_name <- names(which.min(model_rmse))
best_model <- workflows_fitted[[best_model_name]]

cat(paste0("Melhor modelo: ", best_model_name, 
           " (RMSE: ", round(min(model_rmse), 2), ")\n"), 
    file = "logs/modeling.log", append = TRUE)

# Analisar importância das variáveis para o melhor modelo (se for Random Forest)
if (grepl("rf", best_model_name)) {
  rf_model <- extract_fit_parsnip(best_model)
  vip_data <- vip::vip(rf_model, num_features = 20)
  
  # Salvar o gráfico de importância de variáveis
  ggsave("outputs/plots/variable_importance.png", vip_data, width = 10, height = 8, dpi = 300)
}

# Criar gráfico comparativo das métricas dos modelos
model_metrics <- lapply(names(results), function(model_name) {
  metrics_df <- results[[model_name]]
  metrics_df$.metric <- as.character(metrics_df$.metric)
  metrics_df$model <- model_name
  return(metrics_df)
}) %>% bind_rows()

# Gráfico comparativo de RMSE
rmse_plot <- model_metrics %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = reorder(model, .estimate), y = .estimate, fill = model)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Comparação de RMSE entre os Modelos",
    x = "Modelo",
    y = "RMSE (menor é melhor)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfico comparativo de R²
rsq_plot <- model_metrics %>%
  filter(.metric == "rsq") %>%
  ggplot(aes(x = reorder(model, -.estimate), y = .estimate, fill = model)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Comparação de R² entre os Modelos",
    x = "Modelo",
    y = "R² (maior é melhor)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Combinar gráficos
grid_plot <- gridExtra::grid.arrange(rmse_plot, rsq_plot, ncol = 2)
ggsave("outputs/plots/model_comparison.png", grid_plot, width = 12, height = 6, dpi = 300)

# Analisar resíduos do melhor modelo linear (se aplicável)
if (grepl("lm", best_model_name)) {
  test_data <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE) %>%
    filter(as_date(date) > as_date(max(date)) - days(30)) %>%
    mutate(
      date = as_date(date),
      hour = factor(hour, levels = as.character(0:23), ordered = TRUE),
      seasons = factor(seasons),
      holiday = factor(holiday),
      functioning_day = factor(functioning_day),
      weekday = wday(date, label = TRUE),
      month = month(date, label = TRUE)
    )
  
  # Fazer previsões
  preds <- predict(best_model, new_data = test_data)
  residuals_df <- bind_cols(
    test_data %>% select(date, hour, rented_bike_count, temperature_c),
    .pred = preds$.pred,
    residual = test_data$rented_bike_count - preds$.pred
  )
  
  # Salvar dataframe de resíduos
  write_csv(residuals_df, "outputs/analysis/model_residuals.csv")
  
  # Gráfico de resíduos vs valores previstos
  resid_plot <- ggplot(residuals_df, aes(x = .pred, y = residual)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(
      title = "Resíduos vs Valores Previstos",
      x = "Valor Previsto",
      y = "Resíduo"
    ) +
    theme_minimal()
  
  ggsave("outputs/plots/residuals_plot.png", resid_plot, width = 10, height = 6, dpi = 300)
}

# Salvar o melhor modelo separadamente para uso no dashboard
saveRDS(best_model, "outputs/models/best_model.rds")

cat(paste0("Análise de modelos concluída: ", Sys.time(), "\n"), 
    file = "logs/modeling.log", append = TRUE)