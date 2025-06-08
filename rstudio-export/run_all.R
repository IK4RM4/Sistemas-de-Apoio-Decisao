# run_all.R - Script para executar todo o pipeline

# Registrar início
start_time <- Sys.time()
cat(paste0("Iniciando execução completa: ", start_time, "\n"), 
    file = "execution_log.txt")

# Criar diretórios necessários
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/analysis", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", showWarnings = FALSE)

# 1. Recolha de dados
cat("1. Iniciando recolha de dados...\n")
source("01_fetch_data.R")
cat("✓ Recolha de dados concluída\n")

# 2. Limpeza de dados
cat("2. Iniciando limpeza e preparação de dados...\n")
source("02_clean_data.R")
cat("✓ Limpeza de dados concluída\n")

# 3. Análise exploratória
cat("3. Iniciando análise exploratória...\n")
source("03_explore_data.R")
cat("✓ Análise exploratória concluída\n")

# 4. Modelação
cat("4. Iniciando modelação...\n")
source("04_model.R")
cat("✓ Modelação concluída\n")

# 5. Verificar se todos os requisitos para o dashboard estão presentes
cat("5. Verificando requisitos para o dashboard...\n")

files_needed <- c(
  "outputs/models/best_model.rds",
  "data/processed/weather_forecast.csv",
  "data/processed/world_cities.csv",
  "data/processed/bike_sharing_systems.csv"
)

all_files_exist <- all(sapply(files_needed, file.exists))

if (all_files_exist) {
  cat("✓ Todos os arquivos necessários estão presentes\n")
  cat("6. Dashboard pronto para iniciar.\n")
  cat("   Execute 'library(shiny); runApp(\"05_dashboard.R\")' para iniciar o dashboard.\n")
} else {
  missing_files <- files_needed[!sapply(files_needed, file.exists)]
  cat("⚠️ Alguns arquivos necessários estão faltando:\n")
  cat(paste(" -", missing_files, collapse = "\n"), "\n")
  cat("   Verifique os logs de erro para mais detalhes.\n")
}

# Registrar fim
end_time <- Sys.time()
execution_time <- end_time - start_time
cat(paste0("Execução completa finalizada: ", end_time, "\n"), 
    file = "execution_log.txt", append = TRUE)
cat(paste0("Tempo total de execução: ", round(execution_time, 2), " ", 
           attr(execution_time, "units"), "\n"), 
    file = "execution_log.txt", append = TRUE)

cat(paste0("\nTempo total de execução: ", round(execution_time, 2), " ", 
           attr(execution_time, "units"), "\n"))
cat("Relatório detalhado de execução salvo em execution_log.txt\n")