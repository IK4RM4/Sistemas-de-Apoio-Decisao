# run_essential.R 
cat("========================================================================\n")
cat("SISTEMAS DE APOIO À DECISÃO 2024/2025\n")
cat("PIPELINE ESSENCIAL DE ANÁLISE DE PARTILHA DE BICICLETAS\n")
cat("========================================================================\n\n")

# === INSTALAÇÃO APENAS DE PACOTES ESSENCIAIS ===

essential_packages <- c(
  "tidyverse", "dplyr", "ggplot2", "readr", "stringr", "lubridate", 
  "jsonlite", "httr", "rvest", "janitor","RSQLite"
)

cat("Instalando apenas pacotes essenciais...\n")

install_essential <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Instalando:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
    return(TRUE)
  }
  return(TRUE)
}

# Instalar pacotes essenciais
for (pkg in essential_packages) {
  tryCatch({
    install_essential(pkg)
  }, error = function(e) {
    cat("Erro ao instalar", pkg, "- continuando sem este pacote\n")
  })
}

# === CRIAR ESTRUTURA DE DIRECTORIAS ===

cat("Criando estrutura de directorias...\n")
dirs_to_create <- c(
  "data/raw", "data/processed", "config", "outputs/plots", 
  "outputs/statistics", "logs"
)

for (dir in dirs_to_create) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

# === FUNÇÃO DE EXECUÇÃO SIMPLIFICADA ===

execute_script_simple <- function(script_path, description) {
  cat("\n", paste(rep("-", 50), collapse = ""), "\n")
  cat("EXECUTANDO:", description, "\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  if (file.exists(script_path)) {
    tryCatch({
      source(script_path, echo = FALSE)
      cat("✓ CONCLUÍDO:", description, "\n")
      return(TRUE)
    }, error = function(e) {
      cat("✗ ERRO em", description, ":", e$message, "\n")
      return(FALSE)
    })
  } else {
    cat("✗ Ficheiro não encontrado:", script_path, "\n")
    return(FALSE)
  }
}

# === EXECUTAR PIPELINE ESSENCIAL ===

cat("\nIniciando pipeline essencial...\n")

# Scripts essenciais (sem modelação avançada e dashboard complexo)
essential_scripts <- list(
  list(path = "01_fetch_data_IMPROVED.R", desc = "Recolha de Dados"),
  list(path = "02_clean_data_CORRIGIDO.R", desc = "Limpeza de Dados")
)

results <- list()
for (script in essential_scripts) {
  results[[script$path]] <- execute_script_simple(script$path, script$desc)
}

# === ANÁLISE EXPLORATÓRIA SIMPLIFICADA ===

cat("\n", paste(rep("-", 50), collapse = ""), "\n")
cat("EXECUTANDO: Análise Exploratória Simplificada\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Análise EDA básica sem SQL complexo
tryCatch({
  # Carregar dados processados
  if (file.exists("data/processed/seoul_bike_sharing.csv")) {
    seoul_data <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE)
    
    cat("Dados Seoul carregados:", nrow(seoul_data), "registos\n")
    
    # Estatísticas básicas
    cat("\nEstatísticas Básicas:\n")
    cat("Procura média por hora:", round(mean(seoul_data$rented_bike_count, na.rm = TRUE), 1), "\n")
    cat("Procura máxima:", max(seoul_data$rented_bike_count, na.rm = TRUE), "\n")
    cat("Período dos dados:", min(seoul_data$date, na.rm = TRUE), "a", max(seoul_data$date, na.rm = TRUE), "\n")
    
    # Visualização básica
    if (require("ggplot2", quietly = TRUE)) {
      cat("Criando visualizações básicas...\n")
      
      # Gráfico de procura ao longo do tempo
      trend_plot <- ggplot(seoul_data, aes(x = as.Date(date), y = rented_bike_count)) +
        geom_point(alpha = 0.3, color = "steelblue") +
        geom_smooth(method = "loess", color = "red") +
        labs(title = "Tendência de Procura de Bicicletas Seoul",
             x = "Data", y = "Contagem de Bicicletas Alugadas") +
        theme_minimal()
      
      ggsave("outputs/plots/seoul_demand_trend.png", trend_plot, width = 10, height = 6, dpi = 300)
      
      # Padrão horário
      hourly_pattern <- seoul_data %>%
        group_by(hour) %>%
        summarise(avg_demand = mean(rented_bike_count, na.rm = TRUE), .groups = "drop")
      
      hourly_plot <- ggplot(hourly_pattern, aes(x = as.numeric(hour), y = avg_demand)) +
        geom_line(color = "steelblue", size = 1.2) +
        geom_point(color = "darkblue", size = 2) +
        labs(title = "Padrão de Procura Horária",
             x = "Hora do Dia", y = "Procura Média") +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0, 23, 3))
      
      ggsave("outputs/plots/hourly_pattern.png", hourly_plot, width = 10, height = 6, dpi = 300)
      
      cat("Visualizações guardadas em outputs/plots/\n")
    }
    
    results[["eda_simplificada"]] <- TRUE
    
  } else {
    cat("Dados Seoul não encontrados - saltar análise\n")
    results[["eda_simplificada"]] <- FALSE
  }
  
}, error = function(e) {
  cat("Erro na análise exploratória:", e$message, "\n")
  results[["eda_simplificada"]] <- FALSE
})

# === MODELAÇÃO BÁSICA (SE POSSÍVEL) ===

cat("\n", paste(rep("-", 50), collapse = ""), "\n")
cat("EXECUTANDO: Modelação Básica\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

tryCatch({
  if (exists("seoul_data") && nrow(seoul_data) > 0) {
    
    # Modelo linear simples
    cat("Criando modelo linear básico...\n")
    
    # Preparar dados para modelação
    model_data <- seoul_data %>%
      filter(!is.na(rented_bike_count), !is.na(temperature_c), !is.na(humidity_percent)) %>%
      mutate(hour_numeric = as.numeric(hour))
    
    # Modelo linear simples
    simple_model <- lm(rented_bike_count ~ temperature_c + humidity_percent + hour_numeric, 
                       data = model_data)
    
    # Resumo do modelo
    model_summary <- summary(simple_model)
    cat("R² do modelo:", round(model_summary$r.squared, 3), "\n")
    cat("RMSE:", round(sqrt(mean(simple_model$residuals^2)), 1), "\n")
    
    # Guardar resumo do modelo
    model_info <- list(
      r_squared = model_summary$r.squared,
      rmse = sqrt(mean(simple_model$residuals^2)),
      coefficients = model_summary$coefficients[,1],
      model_type = "Linear simples"
    )
    
    write(toJSON(model_info, pretty = TRUE), "outputs/simple_model_summary.json")
    
    results[["modelacao_basica"]] <- TRUE
    
  } else {
    cat("Dados não disponíveis para modelação\n")
    results[["modelacao_basica"]] <- FALSE
  }
  
}, error = function(e) {
  cat("Erro na modelação básica:", e$message, "\n")
  results[["modelacao_basica"]] <- FALSE
})

# === RESUMO FINAL ===

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("RESUMO DO PIPELINE ESSENCIAL\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

successful_tasks <- sum(unlist(results))
total_tasks <- length(results)

cat("\nResultados:\n")
for (task in names(results)) {
  status <- ifelse(results[[task]], "✓ SUCESSO", "✗ FALHOU")
  cat(sprintf("  %s: %s\n", task, status))
}

cat(sprintf("\nTaxa de sucesso: %d/%d (%.1f%%)\n", 
            successful_tasks, total_tasks, 
            (successful_tasks/total_tasks)*100))

# Verificar ficheiros criados
cat("\nFicheiros criados:\n")
output_files <- c(
  "data/processed/seoul_bike_sharing.csv",
  "data/processed/weather_forecast.csv", 
  "outputs/plots/seoul_demand_trend.png",
  "outputs/plots/hourly_pattern.png",
  "outputs/simple_model_summary.json"
)

for (file in output_files) {
  if (file.exists(file)) {
    cat("  ✓", file, "\n")
  } else {
    cat("  ✗", file, "(não criado)\n")
  }
}

# === PRÓXIMOS PASSOS ===

cat("\nPróximos passos recomendados:\n")

if (successful_tasks >= 2) {
  cat("1. Verificar visualizações em outputs/plots/\n")
  cat("2. Rever dados processados em data/processed/\n")
  cat("3. Instalar pacotes adicionais para análise completa:\n")
  cat("   install.packages(c('tidymodels', 'shiny', 'plotly'))\n")
  cat("4. Executar run_all.R para pipeline completo\n")
} else {
  cat("1. Verificar erros nos scripts de recolha/limpeza de dados\n")
  cat("2. Confirmar conexão à internet para APIs\n")
  cat("3. Verificar estrutura de directorias\n")
}

cat("Para análise completa, instalar todos os pacotes e executar run_all.R\n")

# Guardar log simplificado
simple_log <- list(
  execution_date = Sys.time(),
  pipeline_type = "essencial",
  results = results,
  success_rate = (successful_tasks/total_tasks)*100,
  packages_used = essential_packages
)

write(toJSON(simple_log, pretty = TRUE), "logs/essential_pipeline_log.json")

cat("\nLog guardado em: logs/essential_pipeline_log.json\n")