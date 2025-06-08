# setup_and_verify.R - Configuração e Verificação do Projeto
# Projeto: Previsão de Partilha de Bicicletas com Dados Meteorológicos
# Curso: Sistemas de Apoio à Decisão 2024/2025

# ==============================================================================
# CONFIGURAÇÃO INICIAL E VERIFICAÇÃO DO AMBIENTE
# ==============================================================================

cat("🚴‍♂️ SISTEMA DE PREVISÃO DE PARTILHA DE BICICLETAS\n")
cat("📚 Projeto de Sistemas de Apoio à Decisão 2024/2025\n")
cat("🏫 Universidade Autónoma de Lisboa\n\n")

# Função para verificar requisitos do sistema
check_system_requirements <- function() {
  cat("🔍 Verificando requisitos do sistema...\n")
  
  requirements <- list(
    r_version = R.Version()$major >= 4,
    memory = as.numeric(system("echo $(free -m | awk 'NR==2{printf \"%.0f\", $2}')", intern = TRUE)) > 2000,
    disk_space = TRUE  # Simplificado
  )
  
  if (requirements$r_version) {
    cat("✅ R versão:", R.version.string, "\n")
  } else {
    cat("❌ R versão muito antiga. Requer R 4.0+\n")
  }
  
  cat("✅ Sistema operativo:", Sys.info()["sysname"], "\n")
  cat("✅ Arquitetura:", Sys.info()["machine"], "\n")
  
  return(all(unlist(requirements)))
}

# Verificar conectividade
check_internet_connectivity <- function() {
  cat("🌐 Verificando conectividade...\n")
  
  # Testar APIs essenciais
  apis_to_test <- list(
    "OpenWeather" = "https://api.openweathermap.org",
    "CRAN" = "https://cran.r-project.org",
    "GitHub" = "https://github.com",
    "Wikipedia" = "https://en.wikipedia.org"
  )
  
  results <- sapply(names(apis_to_test), function(name) {
    url <- apis_to_test[[name]]
    result <- tryCatch({
      con <- url(url, timeout = 5)
      close(con)
      TRUE
    }, error = function(e) FALSE)
    
    if (result) {
      cat("✅", name, "- Acessível\n")
    } else {
      cat("⚠️ ", name, "- Não acessível\n")
    }
    
    return(result)
  })
  
  return(sum(results) >= 3)  # Pelo menos 3 de 4 devem funcionar
}

# Instalar pacotes necessários
install_required_packages <- function() {
  cat("📦 Verificando e instalando pacotes...\n")
  
  # Lista completa de pacotes organizados por categoria
  packages <- list(
    core = c("tidyverse", "dplyr", "ggplot2", "readr", "tidyr", "purrr", "stringr", "lubridate"),
    cleaning = c("janitor", "VIM", "mice", "skimr"),
    modeling = c("tidymodels", "ranger", "xgboost", "glmnet", "kernlab", "kknn", "vip", "DALEXtra"),
    web = c("httr", "jsonlite", "rvest", "xml2"),
    visualization = c("plotly", "corrplot", "ggcorrplot", "viridis", "scales", "gridExtra", 
                      "patchwork", "GGally", "ggridges", "treemapify"),
    dashboard = c("shiny", "shinydashboard", "shinyWidgets", "shinycssloaders", "DT", 
                  "leaflet", "fresh", "htmltools"),
    time_series = c("forecast", "tseries", "zoo", "xts"),
    parallel = c("parallel", "doParallel", "foreach"),
    utilities = c("here", "config", "tictoc", "progress", "crayon", "cli", "knitr")
  )
  
  all_packages <- unlist(packages)
  
  # Verificar quais pacotes estão em falta
  installed <- installed.packages()[,"Package"]
  missing <- all_packages[!all_packages %in% installed]
  
  if (length(missing) > 0) {
    cat("📥 Instalando", length(missing), "pacotes em falta...\n")
    
    # Instalar em lotes para melhor gestão de erros
    batch_size <- 10
    batches <- split(missing, ceiling(seq_along(missing) / batch_size))
    
    for (i in seq_along(batches)) {
      batch <- batches[[i]]
      cat("   Lote", i, "de", length(batches), ":", paste(batch, collapse = ", "), "\n")
      
      tryCatch({
        install.packages(batch, dependencies = TRUE, repos = "https://cran.rstudio.com/")
        cat("   ✅ Lote", i, "instalado com sucesso\n")
      }, error = function(e) {
        cat("   ⚠️ Erro no lote", i, ":", e$message, "\n")
      })
    }
  } else {
    cat("✅ Todos os pacotes já estão instalados\n")
  }
  
  # Verificar instalação
  final_check <- all_packages %in% installed.packages()[,"Package"]
  success_rate <- sum(final_check) / length(final_check) * 100
  
  cat("📊 Taxa de sucesso da instalação:", round(success_rate, 1), "%\n")
  
  if (success_rate < 90) {
    cat("⚠️ Alguns pacotes podem não ter sido instalados corretamente\n")
    cat("   Pacotes em falta:", paste(all_packages[!final_check], collapse = ", "), "\n")
  }
  
  return(success_rate >= 90)
}

# Criar estrutura de diretórios
create_project_structure <- function() {
  cat("📁 Criando estrutura de diretórios...\n")
  
  directories <- c(
    "data/raw",
    "data/processed", 
    "data/external",
    "outputs/plots",
    "outputs/models",
    "outputs/analysis",
    "outputs/reports",
    "logs",
    "config",
    "docs",
    "tests"
  )
  
  created <- 0
  for (dir in directories) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      created <- created + 1
      cat("  ✅ Criado:", dir, "\n")
    } else {
      cat("  📁 Existe:", dir, "\n")
    }
  }
  
  if (created > 0) {
    cat("✅", created, "diretórios criados\n")
  } else {
    cat("✅ Todos os diretórios já existem\n")
  }
  
  return(TRUE)
}

# Configurar arquivos de configuração
setup_config_files <- function() {
  cat("⚙️ Configurando arquivos de configuração...\n")
  
  # Criar arquivo de configuração principal
  config_content <- '
# Configuração do Projeto SAD 2024/2025
project:
  name: "Bike Sharing Prediction System"
  version: "1.0.0"
  description: "Sistema de Previsão de Partilha de Bicicletas"

data:
  update_interval_hours: 6
  max_forecast_days: 5
  cities:
    - Seoul
    - "New York"
    - Paris
    - London
    - Suzhou
    - Amsterdam
    - Barcelona
    - Berlin
    - Tokyo

api:
  openweather:
    base_url: "https://api.openweathermap.org/data/2.5"
    timeout: 30
    retry_attempts: 3

model:
  algorithm: "random_forest"
  target_variable: "rented_bike_count"
  validation_split: 0.2
  cv_folds: 5

dashboard:
  port: 8080
  host: "127.0.0.1"
  theme: "blue"
'
  
  writeLines(config_content, "config/project_config.yml")
  
  # Criar README básico se não existir
  if (!file.exists("README.md")) {
    readme_basic <- '# Projeto SAD 2024/2025 - Sistema de Previsão de Partilha de Bicicletas

## Início Rápido
1. Execute `source("setup_and_verify.R")` para configurar o ambiente
2. Execute `source("run_complete_project.R")` para executar o projeto completo
3. Execute o dashboard com `shiny::runApp("05_interactive_dashboard.R")`

## Estrutura
- `01_data_collection.R` - Recolha de dados
- `02_data_cleaning.R` - Limpeza de dados  
- `03_exploratory_analysis.R` - Análise exploratória
- `04_advanced_modeling.R` - Modelação ML
- `05_interactive_dashboard.R` - Dashboard Shiny

Para mais informações, consulte a documentação completa.
'
    writeLines(readme_basic, "README.md")
    cat("  ✅ README.md criado\n")
  }
  
  # Criar arquivo de exemplo para API key
  api_example <- '# Configuração da API OpenWeather
# 1. Criar conta em https://openweathermap.org/api
# 2. Obter chave API gratuita
# 3. Substituir "SUA_CHAVE_AQUI" pela chave real
# 4. Salvar este arquivo como "api_key.txt"

SUA_CHAVE_AQUI'
  
  writeLines(api_example, "config/api_key_example.txt")
  
  cat("✅ Arquivos de configuração criados\n")
  return(TRUE)
}

# Verificar integridade dos dados
verify_data_integrity <- function() {
  cat("🔍 Verificando integridade dos dados...\n")
  
  # Verificar arquivos essenciais
  essential_files <- list(
    "Dados de Seoul" = "data/processed/seoul_bike_sharing.csv",
    "Dados meteorológicos" = "data/processed/weather_forecast.csv", 
    "Modelo treinado" = "outputs/models/best_model.rds",
    "Cidades do mundo" = "data/processed/world_cities.csv"
  )
  
  missing_files <- c()
  
  for (desc in names(essential_files)) {
    file_path <- essential_files[[desc]]
    
    if (file.exists(file_path)) {
      size <- file.size(file_path)
      if (size > 0) {
        cat("  ✅", desc, "- OK (", round(size/1024, 1), "KB )\n")
      } else {
        cat("  ⚠️ ", desc, "- Arquivo vazio\n")
        missing_files <- c(missing_files, file_path)
      }
    } else {
      cat("  ❌", desc, "- Não encontrado\n")
      missing_files <- c(missing_files, file_path)
    }
  }
  
  if (length(missing_files) > 0) {
    cat("⚠️ Arquivos em falta ou corrompidos:", length(missing_files), "\n")
    cat("   Execute os scripts de preparação de dados primeiro.\n")
    return(FALSE)
  } else {
    cat("✅ Todos os arquivos essenciais estão presentes\n")
    return(TRUE)
  }
}

# Executar testes básicos
run_basic_tests <- function() {
  cat("🧪 Executando testes básicos...\n")
  
  tests_passed <- 0
  total_tests <- 0
  
  # Teste 1: Carregar tidyverse
  total_tests <- total_tests + 1
  tryCatch({
    library(tidyverse, quietly = TRUE)
    cat("  ✅ Teste 1: Tidyverse carrega corretamente\n")
    tests_passed <- tests_passed + 1
  }, error = function(e) {
    cat("  ❌ Teste 1: Erro ao carregar tidyverse:", e$message, "\n")
  })
  
  # Teste 2: Carregar dados de Seoul (se existir)
  if (file.exists("data/processed/seoul_bike_sharing.csv")) {
    total_tests <- total_tests + 1
    tryCatch({
      data <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE)
      if (nrow(data) > 1000) {
        cat("  ✅ Teste 2: Dados de Seoul carregam corretamente (", nrow(data), "linhas )\n")
        tests_passed <- tests_passed + 1
      } else {
        cat("  ⚠️ Teste 2: Dados de Seoul muito pequenos\n")
      }
    }, error = function(e) {
      cat("  ❌ Teste 2: Erro ao carregar dados de Seoul\n")
    })
  }
  
  # Teste 3: Modelo (se existir)
  if (file.exists("outputs/models/best_model.rds")) {
    total_tests <- total_tests + 1
    tryCatch({
      model <- readRDS("outputs/models/best_model.rds")
      cat("  ✅ Teste 3: Modelo carrega corretamente\n")
      tests_passed <- tests_passed + 1
    }, error = function(e) {
      cat("  ❌ Teste 3: Erro ao carregar modelo\n")
    })
  }
  
  # Teste 4: Criar gráfico simples
  total_tests <- total_tests + 1
  tryCatch({
    p <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) + geom_point()
    cat("  ✅ Teste 4: Gráficos funcionam corretamente\n")
    tests_passed <- tests_passed + 1
  }, error = function(e) {
    cat("  ❌ Teste 4: Erro na criação de gráficos\n")
  })
  
  success_rate <- tests_passed / total_tests * 100
  cat("📊 Taxa de sucesso dos testes:", round(success_rate, 1), "% (", tests_passed, "/", total_tests, ")\n")
  
  return(success_rate >= 75)
}

# Gerar relatório de verificação
generate_verification_report <- function(system_ok, internet_ok, packages_ok, 
                                         structure_ok, config_ok, data_ok, tests_ok) {
  cat("📋 Gerando relatório de verificação...\n")
  
  overall_score <- mean(c(system_ok, internet_ok, packages_ok, structure_ok, 
                          config_ok, data_ok, tests_ok)) * 100
  
  status <- case_when(
    overall_score >= 90 ~ "🟢 EXCELENTE",
    overall_score >= 75 ~ "🟡 BOM", 
    overall_score >= 60 ~ "🟠 ACEITÁVEL",
    TRUE ~ "🔴 PROBLEMÁTICO"
  )
  
  report_content <- sprintf(
    '# Relatório de Verificação do Projeto SAD 2024/2025

## Status Geral: %s (%.1f%%)

## Verificações Realizadas

### Sistema
- **Requisitos do sistema**: %s
- **Conectividade**: %s  
- **Pacotes R**: %s

### Projeto
- **Estrutura de diretórios**: %s
- **Arquivos de configuração**: %s
- **Integridade dos dados**: %s
- **Testes básicos**: %s

## Próximos Passos

%s

---
*Relatório gerado em %s*',
    status, overall_score,
    ifelse(system_ok, "✅ OK", "❌ Falha"),
    ifelse(internet_ok, "✅ OK", "❌ Falha"), 
    ifelse(packages_ok, "✅ OK", "❌ Falha"),
    ifelse(structure_ok, "✅ OK", "❌ Falha"),
    ifelse(config_ok, "✅ OK", "❌ Falha"),
    ifelse(data_ok, "✅ OK", "❌ Falha"),
    ifelse(tests_ok, "✅ OK", "❌ Falha"),
    if (overall_score >= 75) {
      "✅ Sistema pronto para uso!\n- Execute `source('run_complete_project.R')` para começar\n- Consulte README.md para instruções detalhadas"
    } else {
      "⚠️ Sistema requer configuração adicional\n- Verifique os itens marcados como falha\n- Execute novamente após corrigir os problemas"
    },
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  writeLines(report_content, "VERIFICATION_REPORT.md")
  cat("✅ Relatório salvo em: VERIFICATION_REPORT.md\n")
  
  return(overall_score)
}

# Função principal de verificação
main_verification <- function() {
  cat("🚀 INICIANDO VERIFICAÇÃO COMPLETA DO PROJETO\n")
  cat("=" %+% paste0(rep("=", 50), collapse = "") %+% "\n\n")
  
  start_time <- Sys.time()
  
  # Executar todas as verificações
  system_ok <- check_system_requirements()
  internet_ok <- check_internet_connectivity()
  packages_ok <- install_required_packages()
  structure_ok <- create_project_structure()
  config_ok <- setup_config_files()
  data_ok <- verify_data_integrity()
  tests_ok <- run_basic_tests()
  
  # Gerar relatório
  overall_score <- generate_verification_report(
    system_ok, internet_ok, packages_ok, 
    structure_ok, config_ok, data_ok, tests_ok
  )
  
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  
  cat("\n" %+% paste0(rep("=", 60), collapse = "") %+% "\n")
  cat("🏁 VERIFICAÇÃO CONCLUÍDA\n")
  cat("⏱️  Tempo total:", round(as.numeric(duration), 2), "minutos\n")
  cat("📊 Pontuação geral:", round(overall_score, 1), "%\n")
  
  if (overall_score >= 75) {
    cat("🎉 Sistema pronto para uso!\n")
    cat("▶️  Próximo passo: source('run_complete_project.R')\n")
  } else {
    cat("⚠️  Sistema requer atenção. Consulte VERIFICATION_REPORT.md\n")
  }
  
  cat(paste0(rep("=", 60), collapse = "") %+% "\n")
  
  return(overall_score >= 75)
}

# Operador auxiliar
`%+%` <- function(a, b) paste0(a, b)

# Menu de opções rápidas
quick_menu <- function() {
  cat("🎯 MENU DE VERIFICAÇÃO RÁPIDA\n")
  cat("1. Verificação completa\n")
  cat("2. Apenas instalar pacotes\n") 
  cat("3. Apenas verificar dados\n")
  cat("4. Apenas executar testes\n")
  cat("5. Limpar e reconfigurar\n")
  cat("0. Sair\n")
  
  choice <- readline("Escolha uma opção (0-5): ")
  
  switch(choice,
         "1" = main_verification(),
         "2" = install_required_packages(),
         "3" = verify_data_integrity(),
         "4" = run_basic_tests(), 
         "5" = {
           create_project_structure()
           setup_config_files()
           cat("✅ Projeto reconfigurado\n")
         },
         "0" = cat("👋 Saindo...\n"),
         cat("❌ Opção inválida!\n")
  )
}

# Executar verificação se chamado diretamente
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if ("--menu" %in% args) {
    quick_menu()
  } else {
    main_verification()
  }
} else {
  cat("🎯 Setup e Verificação do Projeto carregado!\n")
  cat("📋 Funções disponíveis:\n")
  cat("   - main_verification()     # Verificação completa\n")
  cat("   - quick_menu()           # Menu interativo\n")
  cat("   - install_required_packages() # Instalar pacotes\n")
  cat("   - verify_data_integrity() # Verificar dados\n")
  cat("   - run_basic_tests()      # Executar testes\n\n")
}