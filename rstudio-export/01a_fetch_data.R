# 01a_fetch_data.R - Parte 1

# Instalar e carregar pacotes necessários
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(httr, jsonlite, rvest, readr, dplyr, tidyr, purrr)

# Criar diretórios
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", showWarnings = FALSE)

# Registrar início com timestamp
cat(paste0("Iniciando coleta de dados: ", Sys.time(), "\n"), 
    file = "logs/data_collection.log", append = TRUE)

# 1. Obter dados da Wikipedia (sistemas de bike sharing)
url_bike_wiki <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"

tryCatch({
  bike_html <- read_html(url_bike_wiki)
  bike_table <- bike_html %>%
    html_node(xpath = "(//table[contains(@class, 'wikitable')])[1]") %>%
    html_table(fill = TRUE)
  
  # Adicionar timestamp de coleta
  bike_table$data_collected <- Sys.time()
  
  write_csv(bike_table, "data/raw/raw_bike_sharing_systems.csv")
  cat("✓ Dados de sistemas de bike sharing coletados com sucesso\n", 
      file = "logs/data_collection.log", append = TRUE)
}, error = function(e) {
  cat(paste0("ERRO na coleta de dados Wiki: ", e$message, "\n"), 
      file = "logs/data_collection.log", append = TRUE)
})

# 2. Obter dados de cidades do mundo (fonte alternativa gratuita)
url_cities <- "https://people.sc.fsu.edu/~jburkardt/data/csv/cities.csv"

tryCatch({
  download.file(url_cities, destfile = "data/raw/raw_worldcities.csv", mode = "wb")
  cat("✓ Dados de cidades do mundo baixados com sucesso\n", 
      file = "logs/data_collection.log", append = TRUE)
}, error = function(e) {
  cat(paste0("ERRO na coleta de dados de cidades: ", e$message, "\n"), 
      file = "logs/data_collection.log", append = TRUE)
})