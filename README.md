# 🚲 Previsão de Procura de Bicicletas com Dados Meteorológicos

Este projeto foi desenvolvido no contexto da unidade curricular **Sistemas de Apoio à Decisão** (UAL - 2024/2025). O objetivo é explorar e modelar a relação entre variáveis meteorológicas e a procura por bicicletas partilhadas, utilizando técnicas de análise de dados, regressão e visualização interativa.

---

## 🎯 Objetivos do Projeto

- Recolher dados reais sobre o clima e sistemas de partilha de bicicletas.
- Explorar e analisar a influência das variáveis meteorológicas na procura de bicicletas.
- Construir modelos de previsão com base em regressões lineares e outras técnicas.
- Implementar um painel interativo com previsões por cidade.
- Ajudar na tomada de decisão sobre dimensionamento da frota de bicicletas.

---

## 🧰 Tecnologias e Bibliotecas Utilizadas

- **R** e **RStudio**
- 📦 `tidyverse`, `janitor`, `lubridate`, `stringr` – limpeza e transformação de dados
- 📦 `httr`, `jsonlite` – comunicação com API OpenWeather
- 📦 `tidymodels`, `vip` – modelação e avaliação de modelos
- 📦 `ggplot2` – visualizações
- 📦 `shiny`, `leaflet` – dashboard interativo
- 📁 `sqlite` (opcional) – integração de dados com SQL (não incluído nesta versão)

---

## 📁 Estrutura do Projeto

```
.
├── 01_fetch_data.R              # Recolha de dados das APIs e ficheiros CSV
├── 02_clean_data.R              # Limpeza e transformação dos dados
├── 03_exploratory_analysis.R    # Análise exploratória com visualizações
├── 04_model_regression.R        # Criação e avaliação de modelos preditivos
├── 05_interactive_dashboard.R   # Painel interativo R Shiny
├── run_complete_project.R       # Script para correr todas as fases
├── config/                      # Configurações e parâmetros JSON
├── data/raw/                    # Dados brutos recolhidos
├── data/processed/              # Dados limpos e prontos para modelação
├── outputs/                     # Modelos, gráficos, previsões e estatísticas
└── README.md                    # Este ficheiro
```

---

## 🌐 Fontes de Dados

- **OpenWeather API**: previsão do tempo (5 dias, janelas de 3h)
- **Seoul Bike Sharing Dataset**: aluguer de bicicletas por hora + clima
- **World Cities Dataset**: dados geográficos de cidades globais
- **Global Bike Sharing Systems**: dados sobre frota de bicicletas (extra-Wikipedia)

As cidades foco incluem: **Seoul**, **Londres**, **Nova Iorque**, **Paris** e **Suzhou**.

---

## 🔎 Metodologia

1. **Recolha**  
   Os dados foram obtidos através da API OpenWeather e ficheiros CSV. A previsão meteorológica foi recolhida para várias cidades simultaneamente.

2. **Limpeza**  
   Aplicaram-se expressões regulares e transformação de colunas para uniformizar os dados, eliminar ruído, normalizar variáveis e preparar para análise.

3. **Análise Exploratória**  
   Foram exploradas correlações entre procura e variáveis como temperatura, precipitação, hora do dia e estação do ano, através de boxplots, séries temporais e histogramas.

4. **Modelação Preditiva**  
   Usámos regressão linear com:
   - apenas variáveis meteorológicas
   - apenas variáveis temporais
   - combinação de ambas com termos de interação  
   Foram comparados modelos adicionais como KNN e Decision Tree. A avaliação utilizou métricas como RMSE, R² e importância das variáveis (`vip`).

5. **Dashboard Interativo**  
   Um painel em R Shiny permite visualizar:
   - Previsão de procura horária para cada cidade
   - Evolução climática prevista
   - Potencial de procura máxima com base nos dados futuros
   > ⚠️ Nota: mapa interativo com `leaflet` pode estar em desenvolvimento.

---

## ⚙️ Como Executar

### 1. Instalar Pacotes Necessários

```r
if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, jsonlite, httr, lubridate, janitor, stringr, tidymodels, ggplot2, shiny, leaflet, readr, vip)
```

### 2. Recolher e Processar os Dados

```r
source("run_complete_project.R")
```

### 3. Lançar o Dashboard

```r
runApp("05_interactive_dashboard.R")
```

---

## 📊 Resultados

- A temperatura e a ausência de precipitação são os maiores fatores que influenciam positivamente a procura.
- Há uma forte sazonalidade horária e por estação.
- O modelo final apresenta um RMSE satisfatório para previsões curtas (até 3 dias).
- As cidades com padrões semelhantes a Seul mostram potencial para adoção de modelos preditivos semelhantes.

---

## 🚧 Limitações

- O modelo está calibrado sobretudo para Seul (dados históricos).
- O dashboard pode apresentar erro caso o ficheiro `best_model.rds` esteja ausente.
- O mapa interativo com `leaflet` não foi completamente integrado.

---

## 🚀 Possíveis Melhorias Futuras

- Integração de base de dados SQL (SQLite/PostgreSQL) com consultas diretas.
- Inclusão de mais variáveis exógenas (ex: eventos, feriados locais, mobilidade urbana).
- Integração de notificação por e-mail ou app com base em previsão de procura.
- Dashboard com ranking de cidades por risco de sub/sobredimensionamento da frota.


