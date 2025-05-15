# 05a_dashboard_app.R - Correção final para o tipo de dados de hour

# Carregar pacotes
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(leaflet)

# Verificar se os arquivos necessários existem
required_files <- c(
  "outputs/models/best_model.rds",
  "data/processed/weather_forecast.csv",
  "data/processed/world_cities.csv", 
  "data/processed/bike_sharing_systems.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  cat("Atenção: Os seguintes arquivos necessários não foram encontrados:\n")
  cat(paste("- ", missing_files, collapse = "\n"), "\n")
  cat("O dashboard pode não funcionar corretamente.\n")
}

# Carregar dados e modelo
tryCatch({
  model <- readRDS("outputs/models/best_model.rds")
  weather <- read_csv("data/processed/weather_forecast.csv", show_col_types = FALSE)
  cities <- read_csv("data/processed/world_cities.csv", show_col_types = FALSE)
  bike_systems <- read_csv("data/processed/bike_sharing_systems.csv", show_col_types = FALSE)
  
  # Carregar também os dados de treino para verificar a estrutura do modelo
  seoul_bike <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE)
  
  cat("Dados e modelo carregados com sucesso.\n")
  
  # Verificar os dados e estrutura
  cat("Número de entradas de previsão:", nrow(weather), "\n")
  cat("Número de cidades:", nrow(cities), "\n")
  cat("Número de sistemas de bike sharing:", nrow(bike_systems), "\n")
  
  # Verificar a estrutura da coluna hour no conjunto de treino
  cat("Estrutura da coluna hour nos dados de treino:", class(seoul_bike$hour), "\n")
  
}, error = function(e) {
  cat("Erro ao carregar dados ou modelo:", conditionMessage(e), "\n")
  stop("Não foi possível carregar os dados necessários.")
})

# UI do Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Previsão de Procura de Bicicletas"),
  
  dashboardSidebar(
    selectInput("city", "Cidade:", choices = unique(weather$city)),
    dateRangeInput("date_range", "Período:", 
                   start = Sys.Date(), end = Sys.Date() + 5),
    checkboxGroupInput("periods", "Períodos do dia:",
                       choices = c("Manhã", "Tarde", "Noite", "Madrugada"),
                       selected = c("Manhã", "Tarde", "Noite"))
  ),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_demand_box", width = 6),
      valueBoxOutput("peak_hour_box", width = 6)
    ),
    
    fluidRow(
      box(
        title = "Procura por Hora",
        width = 12,
        plotOutput("hourly_demand_plot")
      )
    ),
    
    fluidRow(
      tabBox(
        width = 12,
        tabPanel("Mapa", leafletOutput("city_map")),
        tabPanel("Temperatura", plotOutput("temp_plot"))
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Verificar se as cidades selecionadas estão disponíveis
  observe({
    available_cities <- unique(weather$city)
    updateSelectInput(session, "city", choices = available_cities)
  })
  
  # Filtrar dados com base nas seleções
  filtered_data <- reactive({
    req(input$city)
    
    # IMPORTANTE: Verificar o tipo de hour nos dados de treino
    seoul_bike <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE)
    hour_type <- class(seoul_bike$hour)
    
    weather %>%
      filter(
        city == input$city,
        as_date(date) >= input$date_range[1],
        as_date(date) <= input$date_range[2]
      ) %>%
      mutate(
        day_period = case_when(
          hour %in% 0:4 ~ "Madrugada",
          hour %in% 5:11 ~ "Manhã",
          hour %in% 12:17 ~ "Tarde",
          hour %in% 18:23 ~ "Noite",
          TRUE ~ "Outros"
        ),
        # CORREÇÃO: Manter hour como numeric, não converter para factor
        hour = as.numeric(hour),
        # Definir outras colunas de categorias
        seasons = factor(case_when(
          month(date) %in% c(12, 1, 2) ~ "Winter",
          month(date) %in% c(3, 4, 5) ~ "Spring",
          month(date) %in% c(6, 7, 8) ~ "Summer",
          TRUE ~ "Autumn"
        )),
        holiday = factor("No Holiday"),
        functioning_day = factor("Yes")
      ) %>%
      filter(day_period %in% input$periods)
  })
  
  # Fazer previsões com tratamento de erro
  predictions <- reactive({
    tryCatch({
      pred_data <- filtered_data()
      if (nrow(pred_data) == 0) {
        return(data.frame(.pred = numeric(0)))
      }
      
      # Imprimir informações de debug
      cat("Estrutura de hour nos dados filtrados:", class(pred_data$hour), "\n")
      
      # Fazer previsão
      predict(model, new_data = pred_data) %>%
        bind_cols(pred_data) %>%
        mutate(.pred = pmax(0, .pred))  # Garantir que não há valores negativos
    }, 
    error = function(e) {
      # Logar erro e retornar dataframe vazio
      cat("Erro na previsão:", conditionMessage(e), "\n")
      data.frame(.pred = numeric(0))
    })
  })
  
  # Procura total
  output$total_demand_box <- renderValueBox({
    pred <- predictions()
    if (nrow(pred) == 0) {
      total <- 0
    } else {
      total <- sum(pred$.pred, na.rm = TRUE)
    }
    
    valueBox(
      round(total),
      "Procura Total Prevista",
      icon = icon("bicycle"),
      color = "blue"
    )
  })
  
  # Hora de pico
  output$peak_hour_box <- renderValueBox({
    pred <- predictions()
    if (nrow(pred) == 0) {
      peak_hour <- "N/A"
    } else {
      peak_data <- pred %>%
        group_by(hour) %>%
        summarise(avg_demand = mean(.pred, na.rm = TRUE)) %>%
        arrange(desc(avg_demand)) %>%
        slice(1)
      
      peak_hour <- if(nrow(peak_data) > 0) paste0(peak_data$hour, "h") else "N/A"
    }
    
    valueBox(
      peak_hour,
      "Hora de Pico",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  # Gráfico de procura por hora
  output$hourly_demand_plot <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    pred %>%
      group_by(hour, day_period) %>%
      summarise(avg_demand = mean(.pred, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = hour, y = avg_demand, fill = day_period)) +
      geom_col() +
      labs(
        title = paste("Procura Prevista para", input$city),
        x = "Hora do Dia",
        y = "Procura Média",
        fill = "Período"
      ) +
      theme_minimal()
  })
  
  # Mapa da cidade
  output$city_map <- renderLeaflet({
    # Procurar a cidade nas coordenadas
    city_name <- strsplit(input$city, ",")[[1]][1]
    city_info <- cities %>% 
      filter(grepl(city_name, city, ignore.case = TRUE)) %>% 
      slice(1)
    
    # Se não encontrou coordenadas, tentar adivinhar
    if(nrow(city_info) == 0 || is.na(city_info$lat) || is.na(city_info$lon)) {
      if(city_name == "Seoul") {
        lat <- 37.5665
        lon <- 126.9780
      } else if(city_name == "New York") {
        lat <- 40.7128
        lon <- -74.0060
      } else if(city_name == "Paris") {
        lat <- 48.8566
        lon <- 2.3522
      } else if(city_name == "London") {
        lat <- 51.5074
        lon <- -0.1278
      } else if(city_name == "Suzhou") {
        lat <- 31.3117
        lon <- 120.6283
      } else {
        # Coordenadas padrão
        lat <- 0
        lon <- 0
      }
    } else {
      lat <- city_info$lat
      lon <- city_info$lon
    }
    
    # Calcular demanda total para o popup
    pred <- predictions()
    total_demand <- if(nrow(pred) > 0) round(sum(pred$.pred, na.rm = TRUE)) else 0
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = lon, lat = lat, zoom = 12) %>%
      addMarkers(
        lng = lon,
        lat = lat,
        popup = paste0(
          "<b>", city_name, "</b><br>",
          "Procura total prevista: ", total_demand
        )
      )
  })
  
  # Gráfico de temperatura
  output$temp_plot <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    pred %>%
      ggplot(aes(x = temperature_c, y = .pred)) +
      geom_point(aes(color = day_period), alpha = 0.7) +
      geom_smooth(method = "loess", formula = y ~ x) +
      labs(
        title = "Relação entre Temperatura e Procura",
        x = "Temperatura (°C)",
        y = "Procura Prevista",
        color = "Período"
      ) +
      theme_minimal()
  })
}

# Executar o dashboard
shinyApp(ui, server)