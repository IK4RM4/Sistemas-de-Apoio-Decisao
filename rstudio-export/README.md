
# Projeto SAD 2024/2025 — Previsão de Partilha de Bicicletas com Dados Meteorológicos

Este projeto aplica técnicas de Sistemas de Apoio à Decisão (SAD) para prever a procura de partilha de bicicletas em cidades com base em dados climáticos.

## Estrutura do Projeto

```
SAD_Project_R_Code/
├── data/
│   ├── raw/              # Dados brutos recolhidos das APIs e websites
│   └── processed/        # Dados tratados e prontos para análise/modelação
├── outputs/
│   ├── plots/            # Gráficos da análise exploratória
│   └── models/           # Modelos treinados (.rds)
├── 01_fetch_data.R       # Script para recolher dados de fontes externas
├── 02_clean_data.R       # Script para limpar e preparar os dados
├── 03_exploratory_analysis.R # Análise exploratória dos dados
├── 04_model_regression.R # Modelos de regressão linear com tidymodels
└── 05_dashboard_app.R    # Aplicação interativa em R Shiny
```

## Pré-requisitos

Instalação dos pacotes necessários em R:

```r
install.packages(c("tidyverse", "lubridate", "janitor", "stringr", 
                   "httr", "jsonlite", "rvest", "readr", 
                   "tidymodels", "shiny"))
```

## Como executar

1. **Recolher os dados**
   ```r
   source("01_fetch_data.R")
   ```

2. **Limpar e preparar os dados**
   ```r
   source("02_clean_data.R")
   ```

3. **Análise exploratória e geração de gráficos**
   ```r
   source("03_exploratory_analysis.R")
   ```

4. **Treinar e avaliar modelos de regressão**
   ```r
   source("04_model_regression.R")
   ```

5. **Executar o dashboard Shiny**
   ```r
   shiny::runApp("05_dashboard_app.R")
   ```

## API Key

O script `01_fetch_data.R` requer uma chave da API OpenWeather. Substitui `"INSERE_AQUI_A_TUA_API_KEY"` pela tua chave real no código.

---

Este projeto foi desenvolvido no âmbito da disciplina de **Sistemas de Apoio à Decisão** e cobre todas as etapas: recolha de dados, preparação, visualização, modelação e visualização interativa.
