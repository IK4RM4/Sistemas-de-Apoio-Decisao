# run_complete_project.R - Execução Completa do Projeto SAD 2024/2025
# Sistema de Apoio à Decisão - Previsão de Demanda para Partilha de Bicicletas

cat("=======================================================\n")
cat("    PROJETO SAD 2024/2025 - EXECUÇÃO COMPLETA\n")
cat("    Sistema de Previsão de Demanda para Bicicletas\n")
cat("=======================================================\n")

# === CONFIGURAÇÃO INICIAL ===

# Verificar versão do R
r_version <- R.Version()
cat("Versão do R:", r_version$version.string, "\n")

# Pacotes necessários para o projeto completo
required_packages <- c(
  # Manipulação de dados
  "tidyverse", "dplyr", "readr", "tidyr", "stringr", "lubridate", "janitor",
  
  # Coleta de dados
  "httr", "jsonlite", "rvest",
  
  # Visualização
  "ggplot2", "plotly", "scales", "viridis", "gridExtra", "corrplot",
  
  # Modelação
  "tidymodels", "ranger", "rpart", "kknn",
  
  # Dashboard
  "shiny", "shinydashboard", "DT", "leaflet"
)

# Função para instalar pacotes em falta
install_missing_packages <- function(packages) {
  missing <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if (length(missing) > 0) {
    cat("Instalando pacotes em falta:", paste(missing, collapse = ", "), "\n")
    
    # Tentar instalar pacotes com retry
    for (pkg in missing) {
      for (attempt in 1:3) {
        tryCatch({
          install.packages(pkg, dependencies = TRUE, quiet = TRUE)
          cat("Instalado:", pkg, "\n")
          break
        }, error = function(e) {
          if (attempt == 3) {
            cat("Falhou:", pkg, "-", e$message, "\n")
          } else {
            cat("Tentativa", attempt, "falhada para", pkg, ", retentando...\n")
            Sys.sleep(2)
          }
        })
      }
    }
  } else {
    cat("Todos os pacotes necessários já estão instalados.\n")
  }
}

# Instalar pacotes em falta
install_missing_packages(required_packages)

# Carregar bibliotecas essenciais
essential_libs <- c("tidyverse", "lubridate", "ggplot2")
for (lib in essential_libs) {
  if (!require(lib, character.only = TRUE, quietly = TRUE)) {
    stop("Não foi possível carregar a biblioteca essencial: ", lib)
  }
}

# Criar estrutura de diretórios
cat("\nCriando estrutura de diretórios...\n")
directories <- c(
  "data/raw", "data/processed", 
  "outputs/plots", "outputs/models", "outputs/statistics", "outputs/predictions"
)

for (dir in directories) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cat("  ✓", dir, "\n")
}

# === FUNÇÕES AUXILIARES ===

execute_script <- function(script_name, description) {
  cat("\n", rep("=", 60), "\n")
  cat("EXECUTANDO:", description, "\n")
  cat("Ficheiro:", script_name, "\n")
  cat(rep("=", 60), "\n")
  
  if (!file.exists(script_name)) {
    cat("ERRO: Ficheiro não encontrado:", script_name, "\n")
    return(FALSE)
  }
  
  start_time <- Sys.time()
  
  tryCatch({
    source(script_name, echo = FALSE)
    end_time <- Sys.time()
    execution_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
    cat("✓ SUCESSO:", description, "concluído em", execution_time, "segundos\n")
    return(TRUE)
  }, error = function(e) {
    cat("ERRO em", description, ":", e$message, "\n")
    cat("Continuando com o próximo módulo...\n")
    return(FALSE)
  })
}

validate_outputs <- function(phase_name, expected_files) {
  cat("\nValidando outputs da fase:", phase_name, "\n")
  
  valid_files <- 0
  for (file in expected_files) {
    if (file.exists(file)) {
      file_size <- file.size(file)
      if (file_size > 0) {
        cat("  ✓", file, "(", round(file_size/1024, 1), "KB)\n")
        valid_files <- valid_files + 1
      } else {
        cat("  ⚠", file, "(vazio)\n")
      }
    } else {
      cat("  ✗", file, "(não encontrado)\n")
    }
  }
  
  success_rate <- round((valid_files / length(expected_files)) * 100, 1)
  cat("Taxa de sucesso:", success_rate, "%\n")
  
  return(success_rate >= 70)  # 70% dos ficheiros devem existir
}

create_project_summary <- function() {
  cat("\n", rep("=", 60), "\n")
  cat("RESUMO FINAL DO PROJETO\n")
  cat(rep("=", 60), "\n")
  
  # Verificar ficheiros de dados
  data_files <- c(
    "data/raw/raw_cities_weather_forecast.csv",
    "data/raw/raw_bike_sharing_systems.csv", 
    "data/raw/raw_worldcities.csv",
    "data/raw/raw_seoul_bike_sharing.csv",
    "data/processed/weather_forecast.csv",
    "data/processed/seoul_bike_sharing.csv"
  )
  
  cat("\nFICHEIROS DE DADOS:\n")
  data_success <- 0
  for (file in data_files) {
    if (file.exists(file) && file.size(file) > 0) {
      cat("  ✓", basename(file), "\n")
      data_success <- data_success + 1
    } else {
      cat("  ✗", basename(file), "\n")
    }
  }
  
  # Verificar visualizações
  plot_files <- list.files("outputs/plots", pattern = "*.png", full.names = TRUE)
  cat("\nVISUALIZAÇÕES GERADAS:", length(plot_files), "\n")
  for (plot in plot_files) {
    cat("  ✓", basename(plot), "\n")
  }
  
  # Verificar modelos
  model_files <- list.files("outputs/models", pattern = "*.rds", full.names = TRUE)
  cat("\nMODELOS TREINADOS:", length(model_files), "\n")
  for (model in model_files) {
    cat("  ✓", basename(model), "\n")
  }
  
  # Estatísticas do projeto
  cat("\nESTATÍSTICAS DO PROJETO:\n")
  cat("  Ficheiros de dados:", data_success, "/", length(data_files), "\n")
  cat("  Visualizações:", length(plot_files), "\n")
  cat("  Modelos:", length(model_files), "\n")
  
  # Verificar se o dashboard pode ser executado
  dashboard_ready <- file.exists("05_interactive_dashboard.R") && 
    length(plot_files) > 0 && 
    data_success >= 3
  
  cat("  Dashboard pronto:", ifelse(dashboard_ready, "✓ SIM", "NÃO"), "\n")
  
  # Recomendações finais
  cat("\nPRÓXIMOS PASSOS:\n")
  if (dashboard_ready) {
    cat("  1. Executar dashboard: source('05_interactive_dashboard.R')\n")
    cat("  2. Revisar visualizações em outputs/plots/\n")
    cat("  3. Examinar modelos em outputs/models/\n")
    cat("  4. Preparar relatório final\n")
  } else {
    cat("  1. Verificar ficheiros de dados em falta\n")
    cat("  2. Re-executar módulos que falharam\n")
    cat("  3. Verificar conectividade de rede para APIs\n")
  }
  
  return(dashboard_ready)
}

# === EXECUÇÃO SEQUENCIAL DOS MÓDULOS ===

cat("\nIniciando execução sequencial dos módulos...\n")

# Lista de scripts para executar
scripts <- list(
  list(
    file = "01_fetch_data.R",
    alt_file = "01_fetch_data_FIXED.R", 
    description = "Obtenção de Dados (APIs e Web Scraping)",
    expected_outputs = c(
      "data/raw/raw_cities_weather_forecast.csv",
      "data/raw/raw_bike_sharing_systems.csv",
      "data/raw/raw_worldcities.csv", 
      "data/raw/raw_seoul_bike_sharing.csv"
    )
  ),
  list(
    file = "02_clean_data.R",
    alt_file = "02_clean_data_FIXED.R",
    description = "Limpeza e Estruturação de Dados", 
    expected_outputs = c(
      "data/processed/weather_forecast.csv",
      "data/processed/seoul_bike_sharing.csv",
      "data/processed/bike_sharing_systems.csv"
    )
  ),
  list(
    file = "03_exploratory_analysis.R",
    alt_file = "03_exploratory_analysis_ENHANCED.R",
    description = "Análise Exploratória e Visualização",
    expected_outputs = c(
      "outputs/plots/01_time_series_demand.png",
      "outputs/plots/02_temporal_patterns.png", 
      "outputs/plots/03_weather_impact.png"
    )
  ),
  list(
    file = "_model_regression.R", 
    alt_file = "04_model_regression.R",
    description = "Modelação Preditiva",
    expected_outputs = c(
      "outputs/models/model_comparison.csv",
      "outputs/models/best_model.rds"
    )
  )
)

# Executar cada script
execution_results <- list()

for (i in seq_along(scripts)) {
  script_info <- scripts[[i]]
  
  # Tentar ficheiro principal, depois alternativo
  script_file <- script_info$file
  if (!file.exists(script_file) && !is.null(script_info$alt_file)) {
    script_file <- script_info$alt_file
  }
  
  # Executar script
  success <- execute_script(script_file, script_info$description)
  execution_results[[i]] <- success
  
  # Validar outputs
  if (success) {
    validation_success <- validate_outputs(
      script_info$description, 
      script_info$expected_outputs
    )
    
    if (!validation_success) {
      cat("AVISO: Alguns outputs esperados não foram gerados\n")
    }
  }
  
  # Pequena pausa entre módulos
  Sys.sleep(1)
}

# === RELATÓRIO FINAL ===

project_ready <- create_project_summary()

# Gerar estatísticas de execução
successful_modules <- sum(unlist(execution_results))
total_modules <- length(execution_results)

cat("\n", rep("=", 60), "\n")
cat("EXECUÇÃO CONCLUÍDA\n")
cat(rep("=", 60), "\n")
cat("Módulos executados com sucesso:", successful_modules, "/", total_modules, "\n")
cat("Taxa de sucesso geral:", round((successful_modules/total_modules)*100, 1), "%\n")

if (project_ready) {
  cat("\nPROJETO COMPLETO E PRONTO PARA USO!\n")
  cat("\nPara iniciar o dashboard interativo, execute:\n")
  cat("source('05_interactive_dashboard.R')\n")
} else {
  cat("\nPROJETO PARCIALMENTE COMPLETO\n")
  cat("\nAlguns módulos podem ter falhado. Verifique os logs acima.\n")
}

# Informações sobre normas europeias aplicadas
cat("\nNORMAS EUROPEIAS APLICADAS:\n")
cat("  • Temperatura: Celsius (°C)\n")
cat("  • Velocidade do vento: Quilómetros por hora (km/h)\n")
cat("  • Distância/Visibilidade: Quilómetros (km)\n")
cat("  • Precipitação: Milímetros (mm)\n")
cat("  • Formato de data: dd/mm/aaaa\n")
cat("  • Formato de hora: 24 horas\n")

