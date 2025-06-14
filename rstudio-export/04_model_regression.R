library(tidymodels)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(vip)
library(corrplot)
library(glmnet)
library(randomForest)
library(xgboost)
library(stringr) 

# Create output directories
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/predictions", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/model_evaluation", recursive = TRUE, showWarnings = FALSE)

cat("SISTEMAS DE APOIO À DECISÃO - MODELAÇÃO PREDITIVA\n")
cat("================================================\n")

# === DATA LOADING AND VALIDATION ===

load_and_validate_data <- function() {
  cat("Loading and validating datasets...\n")
  
  # Load Seoul bike sharing data
  seoul_data <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE)
  
  if (is.null(seoul_data) || nrow(seoul_data) == 0) {
    stop("Seoul bike sharing data is required for modeling")
  }
  
  cat("Seoul bike data loaded:", nrow(seoul_data), "records\n")
  cat("Variables available:", ncol(seoul_data), "\n")
  cat("Available columns:", paste(colnames(seoul_data), collapse = ", "), "\n")
  cat("Date range:", min(seoul_data$date, na.rm = TRUE), "to", max(seoul_data$date, na.rm = TRUE), "\n")
  
  return(seoul_data)
}

# === FEATURE ENGINEERING (SIMPLIFICADA) ===

engineer_features <- function(data) {
  cat("Engineering features for modeling...\n")
  
  # Check which columns are available
  available_cols <- colnames(data)
  cat("Available columns:", paste(available_cols, collapse = ", "), "\n")
  
  engineered_data <- data %>%
    # Ensure proper data types
    mutate(
      date = as_date(date),
      rented_bike_count = as.numeric(rented_bike_count),
      hour = as.numeric(as.character(hour))
    ) %>%
    # Remove invalid records
    filter(
      !is.na(date),
      !is.na(rented_bike_count),
      rented_bike_count >= 0
    ) %>%
    # Create basic temporal features
    mutate(
      # Basic temporal features
      year = year(date),
      month = month(date),
      day_of_year = yday(date),
      weekday = wday(date),
      is_weekend = as.numeric(weekday %in% c(1, 7)),
      
      # Hour-based features
      hour_sin = sin(2 * pi * hour / 24),
      hour_cos = cos(2 * pi * hour / 24),
      
      # Day of year cyclical features
      day_sin = sin(2 * pi * day_of_year / 365),
      day_cos = cos(2 * pi * day_of_year / 365),
      
      # Rush hour indicators
      morning_rush = as.numeric(hour %in% 7:9),
      evening_rush = as.numeric(hour %in% 17:19),
      business_hours = as.numeric(hour %in% 9:17)
    )
  
  # Add weather features if available (IMPROVED - avoid multicollinearity)
  if ("temperature_c" %in% available_cols) {
    engineered_data <- engineered_data %>%
      mutate(
        temperature_comfort = case_when(
          temperature_c >= 15 & temperature_c <= 25 ~ "optimal",
          temperature_c >= 10 & temperature_c <= 30 ~ "good",
          TRUE ~ "poor"
        )
        # Removed temperature_squared to avoid multicollinearity
      )
  }
  
  if ("humidity_percent" %in% available_cols) {
    engineered_data <- engineered_data %>%
      mutate(
        humidity_comfort = case_when(
          humidity_percent <= 60 ~ "comfortable",
          humidity_percent <= 80 ~ "moderate", 
          TRUE ~ "uncomfortable"
        )
      )
  }
  
  # Wind speed features (choose ONE to avoid multicollinearity)
  if ("wind_speed_ms" %in% available_cols) {
    engineered_data <- engineered_data %>%
      mutate(
        wind_comfort = case_when(
          wind_speed_ms <= 4.16 ~ "calm",      # <= 15 km/h
          wind_speed_ms <= 6.94 ~ "breezy",    # <= 25 km/h
          TRUE ~ "windy"
        )
      )
  } else if ("wind_speed_kmh" %in% available_cols) {
    engineered_data <- engineered_data %>%
      mutate(
        wind_speed_ms = wind_speed_kmh / 3.6,
        wind_comfort = case_when(
          wind_speed_kmh <= 15 ~ "calm",
          wind_speed_kmh <= 25 ~ "breezy",
          TRUE ~ "windy"
        )
      ) %>%
      select(-wind_speed_kmh)  # Remove to avoid duplication
  }
  
  # Simplified interaction terms (only if really beneficial)
  if (all(c("temperature_c", "humidity_percent") %in% available_cols)) {
    engineered_data <- engineered_data %>%
      mutate(
        temp_humidity_interaction = temperature_c * (humidity_percent / 100)
      )
  }
  
  # Remove temp_wind_interaction to reduce multicollinearity
  
  # Weather severity index
  if ("rainfall_mm" %in% available_cols) {
    engineered_data$rainfall_mm <- ifelse(is.na(engineered_data$rainfall_mm), 0, engineered_data$rainfall_mm)
  } else {
    engineered_data$rainfall_mm <- 0
  }
  
  if ("snowfall_cm" %in% available_cols) {
    engineered_data$snowfall_cm <- ifelse(is.na(engineered_data$snowfall_cm), 0, engineered_data$snowfall_cm)
  } else {
    engineered_data$snowfall_cm <- 0
  }
  
  engineered_data <- engineered_data %>%
    mutate(
      weather_severity = case_when(
        rainfall_mm > 5 | snowfall_cm > 2 ~ "severe",
        rainfall_mm > 1 | snowfall_cm > 0.5 ~ "moderate",
        TRUE ~ "mild"
      )
    )
  
  # Convert categorical variables to factors (only if they exist)
  if ("seasons" %in% available_cols) {
    engineered_data <- engineered_data %>%
      mutate(seasons = factor(seasons, levels = c("Spring", "Summer", "Autumn", "Winter")))
  }
  
  if ("holiday" %in% available_cols) {
    engineered_data <- engineered_data %>%
      mutate(holiday = factor(holiday, levels = c("No Holiday", "Holiday")))
  }
  
  if ("functioning_day" %in% available_cols) {
    engineered_data <- engineered_data %>%
      mutate(functioning_day = factor(functioning_day, levels = c("Yes", "No")))
  }
  
  # Convert new categorical variables to factors
  if ("temperature_comfort" %in% colnames(engineered_data)) {
    engineered_data <- engineered_data %>%
      mutate(temperature_comfort = factor(temperature_comfort, levels = c("poor", "good", "optimal")))
  }
  
  if ("humidity_comfort" %in% colnames(engineered_data)) {
    engineered_data <- engineered_data %>%
      mutate(humidity_comfort = factor(humidity_comfort, levels = c("uncomfortable", "moderate", "comfortable")))
  }
  
  if ("wind_comfort" %in% colnames(engineered_data)) {
    engineered_data <- engineered_data %>%
      mutate(wind_comfort = factor(wind_comfort, levels = c("windy", "breezy", "calm")))
  }
  
  if ("weather_severity" %in% colnames(engineered_data)) {
    engineered_data <- engineered_data %>%
      mutate(weather_severity = factor(weather_severity, levels = c("severe", "moderate", "mild")))
  }
  
  cat("Features engineered:", ncol(engineered_data), "total variables\n")
  cat("Records after cleaning:", nrow(engineered_data), "\n")
  
  return(engineered_data)
}

# === DATA SPLITTING ===

split_data <- function(data) {
  cat("Splitting data into training and testing sets...\n")
  
  set.seed(2024)  # For reproducibility
  
  # Stratified split based on demand quartiles
  data_with_quartiles <- data %>%
    mutate(demand_quartile = ntile(rented_bike_count, 4))
  
  # 80-20 split
  data_split <- initial_split(data_with_quartiles, prop = 0.8, strata = demand_quartile)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Remove quartile column
  train_data <- train_data %>% select(-demand_quartile)
  test_data <- test_data %>% select(-demand_quartile)
  
  cat("Training set:", nrow(train_data), "records\n")
  cat("Testing set:", nrow(test_data), "records\n")
  
  return(list(train = train_data, test = test_data, split = data_split))
}

# === MODEL RECIPES (SIMPLIFICADAS) ===

create_model_recipes <- function(train_data) {
  cat("Creating model recipes...\n")
  
  available_cols <- colnames(train_data)
  cat("Available columns for recipes:", paste(available_cols, collapse = ", "), "\n")
  
  # Base recipe - only with existing columns (IMPROVED)
  base_recipe <- recipe(rented_bike_count ~ ., data = train_data) %>%
    step_rm(date) %>%  # Remove date variable
    step_impute_median(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%  # Remove zero variance
    step_corr(all_numeric_predictors(), threshold = 0.95) %>%  # Remove highly correlated
    step_lincomb(all_numeric_predictors()) %>%  # Remove linear combinations
    step_normalize(all_numeric_predictors())
  
  # Weather only recipe (if weather columns exist) - IMPROVED
  weather_vars <- intersect(c("temperature_c", "humidity_percent", "wind_speed_ms", "wind_speed_kmh", "visibility_km"), available_cols)
  
  weather_recipe <- NULL
  if (length(weather_vars) > 0) {
    weather_formula <- as.formula(paste("rented_bike_count ~", paste(weather_vars, collapse = " + ")))
    weather_recipe <- recipe(weather_formula, data = train_data) %>%
      step_impute_median(all_numeric_predictors()) %>%
      step_corr(all_numeric_predictors(), threshold = 0.95) %>%  # Remove correlations
      step_zv(all_predictors()) %>%
      step_normalize(all_numeric_predictors())
  }
  
  # Temporal only recipe - IMPROVED
  temporal_vars <- intersect(c("hour", "weekday", "month", "is_weekend", "morning_rush", "evening_rush", "business_hours", "hour_sin", "hour_cos", "day_sin", "day_cos", "seasons", "holiday"), available_cols)
  
  temporal_recipe <- NULL
  if (length(temporal_vars) > 0) {
    temporal_formula <- as.formula(paste("rented_bike_count ~", paste(temporal_vars, collapse = " + ")))
    temporal_recipe <- recipe(temporal_formula, data = train_data) %>%
      step_impute_mode(all_nominal_predictors()) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_zv(all_predictors()) %>%
      step_lincomb(all_numeric_predictors()) %>%  # Remove linear dependencies
      step_normalize(all_numeric_predictors())
  }
  
  cat("Model recipes created\n")
  
  return(list(
    base = base_recipe,
    weather = weather_recipe,
    temporal = temporal_recipe
  ))
}

# === MODEL SPECIFICATIONS ===

create_model_specs <- function() {
  cat("Creating model specifications...\n")
  
  # Linear regression models
  lm_spec <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
  
  # Ridge regression with regularization
  ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  
  # Lasso regression with regularization
  lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  
  # Random forest
  rf_spec <- rand_forest(
    mtry = tune(),
    trees = 500,
    min_n = tune()
  ) %>%
    set_engine("randomForest") %>%
    set_mode("regression")
  
  cat("Model specifications created: linear, ridge, lasso, random forest\n")
  
  return(list(
    linear = lm_spec,
    ridge = ridge_spec,
    lasso = lasso_spec,
    random_forest = rf_spec
  ))
}

# === MODEL TRAINING AND EVALUATION ===

train_and_evaluate_models <- function(recipes, specs, split_data) {
  cat("Training and evaluating models...\n")
  
  train_data <- split_data$train
  test_data <- split_data$test
  
  # Create cross-validation folds
  set.seed(2024)
  cv_folds <- vfold_cv(train_data, v = 5, strata = rented_bike_count)
  
  # Model combinations to test (only non-NULL recipes)
  model_combinations <- list()
  
  if (!is.null(recipes$weather)) {
    model_combinations <- append(model_combinations, 
                                 list(list(name = "Linear_Weather", recipe = recipes$weather, spec = specs$linear)))
  }
  
  if (!is.null(recipes$temporal)) {
    model_combinations <- append(model_combinations, 
                                 list(list(name = "Linear_Temporal", recipe = recipes$temporal, spec = specs$linear)))
  }
  
  if (!is.null(recipes$base)) {
    model_combinations <- append(model_combinations, 
                                 list(list(name = "Linear_Full", recipe = recipes$base, spec = specs$linear),
                                      list(name = "Ridge_Full", recipe = recipes$base, spec = specs$ridge),
                                      list(name = "Lasso_Full", recipe = recipes$base, spec = specs$lasso),
                                      list(name = "RandomForest_Full", recipe = recipes$base, spec = specs$random_forest)))
  }
  
  results <- list()
  
  for (combo in model_combinations) {
    cat("Training model:", combo$name, "\n")
    
    tryCatch({
      # Create workflow
      workflow_obj <- workflow() %>%
        add_recipe(combo$recipe) %>%
        add_model(combo$spec)
      
      # Handle tuning vs non-tuning models
      if (combo$name %in% c("Linear_Weather", "Linear_Temporal", "Linear_Full")) {
        # Fit without tuning
        model_fit <- fit(workflow_obj, data = train_data)
        
        # Evaluate on test set
        predictions <- predict(model_fit, new_data = test_data) %>%
          bind_cols(test_data) %>%
          select(.pred, rented_bike_count)
        
        metrics_result <- predictions %>%
          metrics(truth = rented_bike_count, estimate = .pred)
        
      } else {
        # Tune hyperparameters
        if (combo$name == "Ridge_Full") {
          tune_grid <- grid_regular(penalty(range = c(-5, 0)), levels = 10)
        } else if (combo$name == "Lasso_Full") {
          tune_grid <- grid_regular(penalty(range = c(-5, 0)), levels = 10)
        } else if (combo$name == "RandomForest_Full") {
          tune_grid <- grid_regular(
            mtry(range = c(2, 8)),
            min_n(range = c(5, 25)),
            levels = 3
          )
        }
        
        # Perform tuning
        tune_results <- tune_grid(
          workflow_obj,
          resamples = cv_folds,
          grid = tune_grid,
          metrics = metric_set(rmse, rsq, mae)
        )
        
        # Select best model
        best_params <- select_best(tune_results, metric = "rmse")
        final_workflow <- finalize_workflow(workflow_obj, best_params)
        model_fit <- fit(final_workflow, data = train_data)
        
        # Evaluate on test set
        predictions <- predict(model_fit, new_data = test_data) %>%
          bind_cols(test_data) %>%
          select(.pred, rented_bike_count)
        
        metrics_result <- predictions %>%
          metrics(truth = rented_bike_count, estimate = .pred)
      }
      
      # Store results
      results[[combo$name]] <- list(
        model = model_fit,
        predictions = predictions,
        metrics = metrics_result,
        workflow = workflow_obj
      )
      
      # Print key metrics
      rmse_val <- metrics_result %>% filter(.metric == "rmse") %>% pull(.estimate)
      rsq_val <- metrics_result %>% filter(.metric == "rsq") %>% pull(.estimate)
      mae_val <- metrics_result %>% filter(.metric == "mae") %>% pull(.estimate)
      
      cat("  RMSE:", round(rmse_val, 3), "| R²:", round(rsq_val, 3), "| MAE:", round(mae_val, 3), "\n")
      
    }, error = function(e) {
      cat("  Error training", combo$name, ":", e$message, "\n")
    })
  }
  
  return(results)
}

# === MODEL COMPARISON AND SELECTION ===

compare_and_select_models <- function(results) {
  cat("Comparing model performances...\n")
  
  # Extract metrics for comparison
  comparison_data <- map_dfr(results, function(result) {
    result$metrics %>%
      pivot_wider(names_from = .metric, values_from = .estimate)
  }, .id = "model_name")
  
  # Rank models by RMSE (lower is better)
  comparison_ranked <- comparison_data %>%
    arrange(rmse) %>%
    mutate(
      rank = row_number(),
      rmse_improvement = round((max(rmse) - rmse) / max(rmse) * 100, 1)
    )
  
  cat("Model performance ranking:\n")
  print(comparison_ranked %>% select(model_name, rank, rmse, rsq, mae, rmse_improvement))
  
  # Select best model
  best_model_name <- comparison_ranked$model_name[1]
  best_model_result <- results[[best_model_name]]
  
  cat("Best model selected:", best_model_name, "\n")
  
  # Save comparison results
  write_csv(comparison_ranked, "outputs/model_evaluation/model_comparison.csv")
  
  return(list(
    comparison = comparison_ranked,
    best_model = best_model_result,
    best_model_name = best_model_name,
    all_results = results
  ))
}

# === FEATURE IMPORTANCE ANALYSIS (IMPROVED) ===

analyze_feature_importance <- function(best_model, best_model_name) {
  cat("Analyzing feature importance...\n")
  
  tryCatch({
    # Check model type using grepl instead of str_detect
    if (grepl("Linear|Ridge|Lasso", best_model_name)) {
      # For linear models, extract coefficients
      model_fit <- best_model$model
      
      if (inherits(model_fit, "workflow")) {
        fitted_model <- extract_fit_parsnip(model_fit)
      } else {
        fitted_model <- model_fit
      }
      
      # Create VIP plot
      vip_plot <- fitted_model %>%
        vip(num_features = 15) +
        labs(
          title = paste("Feature Importance:", best_model_name),
          subtitle = "Top 15 most important variables"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"))
      
      ggsave("outputs/model_evaluation/feature_importance.png", vip_plot,
             width = 10, height = 8, dpi = 300)
      
      cat("Feature importance analysis completed for linear model\n")
      
    } else if (grepl("RandomForest", best_model_name)) {
      # For random forest, use built-in importance
      rf_model <- extract_fit_engine(best_model$model)
      
      # Check if importance is available
      if ("importance" %in% names(rf_model)) {
        importance_data <- importance(rf_model) %>%
          as.data.frame() %>%
          tibble::rownames_to_column("variable") %>%
          arrange(desc(IncNodePurity)) %>%
          head(15)
        
        vip_plot <- ggplot(importance_data, aes(x = reorder(variable, IncNodePurity), y = IncNodePurity)) +
          geom_col(fill = "steelblue", alpha = 0.8) +
          coord_flip() +
          labs(
            title = paste("Feature Importance:", best_model_name),
            x = "Variables",
            y = "Importance (IncNodePurity)"
          ) +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold"))
        
        ggsave("outputs/model_evaluation/feature_importance.png", vip_plot,
               width = 10, height = 8, dpi = 300)
        
        cat("Feature importance analysis completed for random forest\n")
      } else {
        cat("Random forest model does not have importance information\n")
      }
    } else {
      cat("Model type not supported for feature importance analysis\n")
    }
    
  }, error = function(e) {
    cat("Could not generate feature importance plot:", e$message, "\n")
    
    # Create a simple placeholder plot
    tryCatch({
      placeholder_plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
        geom_text(label = "Feature importance\nnot available\nfor this model", size = 6) +
        labs(title = paste("Model:", best_model_name)) +
        theme_void() +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
      
      ggsave("outputs/model_evaluation/feature_importance.png", placeholder_plot,
             width = 10, height = 8, dpi = 300)
      
      cat("Created placeholder feature importance plot\n")
    }, error = function(e2) {
      cat("Could not create placeholder plot:", e2$message, "\n")
    })
  })
}

# === PREDICTION VISUALIZATION ===

create_prediction_visualizations <- function(model_results) {
  cat("Creating prediction visualizations...\n")
  
  best_predictions <- model_results$best_model$predictions
  
  # Actual vs Predicted scatter plot
  pred_plot <- ggplot(best_predictions, aes(x = rented_bike_count, y = .pred)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "lm", se = TRUE, color = "darkgreen", alpha = 0.3) +
    labs(
      title = paste("Prediction Accuracy:", model_results$best_model_name),
      subtitle = "Red line: perfect prediction, Green line: model trend",
      x = "Actual Bike Count",
      y = "Predicted Bike Count"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    coord_equal()
  
  ggsave("outputs/model_evaluation/prediction_accuracy.png", pred_plot,
         width = 10, height = 8, dpi = 300)
  
  # Residual analysis
  residuals_data <- best_predictions %>%
    mutate(
      residuals = rented_bike_count - .pred,
      fitted_values = .pred
    )
  
  residual_plot <- ggplot(residuals_data, aes(x = fitted_values, y = residuals)) +
    geom_point(alpha = 0.6, color = "coral") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", se = TRUE, color = "blue") +
    labs(
      title = "Residual Analysis",
      subtitle = "Checking for patterns in model errors",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ggsave("outputs/model_evaluation/residual_analysis.png", residual_plot,
         width = 10, height = 8, dpi = 300)
  
  # Histogram of residuals
  residual_hist <- ggplot(residuals_data, aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, 
                   fill = "lightblue", alpha = 0.7, color = "white") +
    geom_density(color = "darkblue", linewidth = 1.2) +
    labs(
      title = "Distribution of Model Residuals",
      subtitle = "Checking assumption of normality",
      x = "Residuals",
      y = "Density"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ggsave("outputs/model_evaluation/residual_distribution.png", residual_hist,
         width = 10, height = 8, dpi = 300)
}

# === SAVE MODELS ===

save_models <- function(model_results) {
  cat("Saving trained models...\n")
  
  # Save best model
  saveRDS(model_results$best_model$model, "outputs/models/best_model.rds")
  
  # Save all model results
  saveRDS(model_results$all_results, "outputs/models/all_models.rds")
  
  # Save predictions
  write_csv(model_results$best_model$predictions, "outputs/predictions/test_predictions.csv")
  
  # Save model comparison
  write_csv(model_results$comparison, "outputs/model_evaluation/final_model_comparison.csv")
  
  cat("Models and results saved successfully\n")
}

# === MAIN EXECUTION ===

cat("Starting predictive modeling pipeline...\n\n")

# Load and prepare data
seoul_data <- load_and_validate_data()
engineered_data <- engineer_features(seoul_data)
split_result <- split_data(engineered_data)

# Create recipes and model specifications
recipes <- create_model_recipes(split_result$train)
specs <- create_model_specs()

# Train and evaluate models
model_results <- train_and_evaluate_models(recipes, specs, split_result)

# Compare and select best model
final_results <- compare_and_select_models(model_results)

# Analyze feature importance
analyze_feature_importance(final_results$best_model, final_results$best_model_name)

# Create visualizations
create_prediction_visualizations(final_results)

# Save models and results
save_models(final_results)

# === FINAL SUMMARY ===

cat("\nPREDICTIVE MODELING COMPLETED\n")
cat("============================\n")
cat("Models trained:", length(final_results$all_results), "\n")
cat("Best model:", final_results$best_model_name, "\n")

best_metrics <- final_results$comparison %>% filter(rank == 1)
cat("Best model performance:\n")
cat("  RMSE:", round(best_metrics$rmse, 3), "\n")
cat("  R²:", round(best_metrics$rsq, 3), "\n")
cat("  MAE:", round(best_metrics$mae, 3), "\n")

cat("\nFiles generated:\n")
cat("- Best model: outputs/models/best_model.rds\n")
cat("- All models: outputs/models/all_models.rds\n")
cat("- Predictions: outputs/predictions/test_predictions.csv\n")
cat("- Model comparison: outputs/model_evaluation/final_model_comparison.csv\n")
cat("- Visualizations: outputs/model_evaluation/*.png\n")

cat("\nNext phase: Interactive Dashboard (05_dashboard.R)\n")
cat("============================\n")