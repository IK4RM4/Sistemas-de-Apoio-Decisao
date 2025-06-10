
# run_all.R - Execução Sequencial do Projeto SAD 2024/2025




# === CONFIGURAÇÃO INICIAL ===

# Instalar pacotes necessários, se não estiverem instalados
required_packages <- c("tidyverse", "lubridate", "httr", "jsonlite", "rvest",
                       "shiny", "shinydashboard", "DT", "plotly", "leaflet",
                       "viridis", "janitor", "tidymodels")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Carregar bibliotecas base
library(tidyverse)
library(lubridate)

# Criar diretórios, se necessário
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)

# === EXECUÇÃO DOS MÓDULOS ===

source("01_fetch_data.R")        # Obtenção de dados (API e scraping)
source("02_clean_data.R")        # Limpeza e estruturação
source("03_exploratory_analysis.R") # Análise exploratória e visualização
source("04_model_regression.R")  # Modelação preditiva (regressão)
# source("05_dashboard_app.R")   # Aplicação Shiny (executar separadamente)

cat("\nPipeline concluído. Verifique os outputs nas respetivas pastas.\n")
