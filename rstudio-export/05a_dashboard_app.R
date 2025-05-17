# 05a_dashboard_app.R - Dashboard com visualizações avançadas incluindo heatmaps

# Carregar pacotes
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(leaflet)
library(plotly)
library(DT)
library(viridis)
library(reshape2)  # Para os heatmaps
library(scales)    # Para formatação de escalas

# Carregar dados e modelo
tryCatch({
  model <- readRDS("outputs/models/best_model.rds")
  weather <- read_csv("data/processed/weather_forecast.csv", show_col_types = FALSE)
  cities <- read_csv("data/processed/world_cities.csv", show_col_types = FALSE)
  bike_systems <- read_csv("data/processed/bike_sharing_systems.csv", show_col_types = FALSE)
  seoul_bike <- read_csv("data/processed/seoul_bike_sharing.csv", show_col_types = FALSE)
  
  cat("Dados e modelo carregados com sucesso.\n")
  
}, error = function(e) {
  cat("Erro ao carregar dados ou modelo:", conditionMessage(e), "\n")
  stop("Não foi possível carregar os dados necessários.")
})

# UI do Dashboard com gráficos avançados
ui <- dashboardPage(
  dashboardHeader(title = "Previsão de Bicicletas"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Análise Horária", tabName = "hourly", icon = icon("clock")),
      menuItem("Heatmaps", tabName = "heatmaps", icon = icon("fire")),
      menuItem("Temperatura", tabName = "temperature", icon = icon("temperature-half")),
      menuItem("Mapa", tabName = "map", icon = icon("map")),
      menuItem("Sobre", tabName = "about", icon = icon("info-circle"))
    ),
    
    # Controles comuns a todas as abas
    selectInput("city", "Cidade:", choices = unique(weather$city)),
    dateRangeInput("date_range", "Período:", 
                   start = Sys.Date(), end = Sys.Date() + 5),
    checkboxGroupInput("periods", "Períodos do dia:",
                       choices = c("Manhã", "Tarde", "Noite", "Madrugada"),
                       selected = c("Manhã", "Tarde", "Noite"))
  ),
  
  dashboardBody(
    tabItems(
      # Aba 1: Dashboard Principal
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_demand_box", width = 6),
          valueBoxOutput("peak_hour_box", width = 6)
        ),
        
        fluidRow(
          box(
            title = "Previsão de Procura por Hora",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("hourly_demand_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Radar de Procura por Período",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("radar_chart")
          ),
          
          box(
            title = "Distribuição por Período",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("period_distribution_plot")
          )
        )
      ),
      
      # Aba 2: Análise Horária
      tabItem(
        tabName = "hourly",
        fluidRow(
          box(
            title = "Padrão de Procura por Hora do Dia",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("hourly_pattern_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Distribuição de Procura por Hora",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("hour_distribution_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Tabela de Procura por Hora",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("hourly_table")
          )
        )
      ),
      
      # Aba 3: Heatmaps
      tabItem(
        tabName = "heatmaps",
        fluidRow(
          box(
            title = "Heatmap: Procura por Hora e Dia",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("hour_day_heatmap")
          )
        ),
        
        fluidRow(
          box(
            title = "Heatmap: Temperatura vs. Hora do Dia",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("temp_hour_heatmap")
          ),
          
          box(
            title = "Heatmap: Correlação entre Variáveis",
            status = "success",
            solidHeader = TRUE, 
            width = 6,
            plotOutput("correlation_heatmap")
          )
        )
      ),
      
      # Aba 4: Análise de Temperatura
      tabItem(
        tabName = "temperature",
        fluidRow(
          box(
            title = "Temperatura vs. Procura",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("temp_demand_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Previsão de Temperatura (Área)",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("temp_forecast_area_plot")
          ),
          
          box(
            title = "Temperatura por Período (Violin Plot)",
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            plotOutput("temp_period_violin_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Gráfico 3D: Hora x Temperatura x Procura",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("temp_hour_demand_3d")
          )
        )
      ),
      
      # Aba 5: Mapa Interativo
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "Mapa de Procura",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("demand_map", height = 500)
          )
        ),
        
        fluidRow(
          box(
            title = "Comparação entre Cidades (Lollipop Chart)",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("cities_lollipop_chart")
          )
        )
      ),
      
      # Aba 6: Sobre o Projeto
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "Sobre o Projeto",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h3("Sistema de Previsão de Procura de Bicicletas"),
            
            p("Este dashboard foi desenvolvido como parte do projeto da disciplina de Sistemas de Apoio à Decisão 2024/2025."),
            
            h4("Objetivo"),
            p("Analisar como o clima afeta a procura por partilha de bicicletas em áreas urbanas e criar um sistema de previsão baseado em condições meteorológicas."),
            
            h4("Metodologia"),
            tags$ul(
              tags$li("Recolha de dados históricos de utilização de bicicletas de Seul"),
              tags$li("Análise exploratória para identificar padrões e correlações"),
              tags$li("Treinamento de modelos de machine learning para prever a procura"),
              tags$li("Desenvolvimento de uma interface interativa para visualização")
            ),
            
            h4("Modelo de Machine Learning"),
            p("O sistema utiliza Random Forest para prever a procura, atingindo um R² de 0.87 (87% da variabilidade explicada). Este resultado é significativamente superior ao obtido com regressão linear (R² de 0.55)."),
            
            h4("Equipa"),
            p("Desenvolvido por: [Nome dos Integrantes do Grupo]")
          )
        )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Filtrar dados com base nas seleções
  filtered_data <- reactive({
    req(input$city)
    
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
        # Manter hour como numeric conforme o modelo espera
        hour = as.numeric(hour),
        # Definir outras colunas de categorias
        seasons = factor(case_when(
          month(date) %in% c(12, 1, 2) ~ "Winter",
          month(date) %in% c(3, 4, 5) ~ "Spring",
          month(date) %in% c(6, 7, 8) ~ "Summer",
          TRUE ~ "Autumn"
        )),
        holiday = factor("No Holiday"),
        functioning_day = factor("Yes"),
        # Dados adicionais para visualizações
        weekday = wday(date, label = TRUE),
        day_name = weekdays(date),
        day_num = wday(date)
      ) %>%
      filter(day_period %in% input$periods)
  })
  
  # Fazer previsões
  predictions <- reactive({
    tryCatch({
      pred_data <- filtered_data()
      if (nrow(pred_data) == 0) {
        return(data.frame(.pred = numeric(0)))
      }
      
      # Fazer previsão
      predict(model, new_data = pred_data) %>%
        bind_cols(pred_data) %>%
        mutate(.pred = pmax(0, .pred))  # Garantir que não há valores negativos
    }, 
    error = function(e) {
      cat("Erro na previsão:", conditionMessage(e), "\n")
      data.frame(.pred = numeric(0))
    })
  })
  
  # Dados para comparação de cidades
  cities_data <- reactive({
    # Preparar dados para todas as cidades disponíveis
    cities_list <- unique(weather$city)
    
    # Filtrar por período
    all_cities_data <- weather %>%
      filter(
        as_date(date) >= input$date_range[1],
        as_date(date) <= input$date_range[2]
      )
    
    # Preparar sumário por cidade
    city_summaries <- all_cities_data %>%
      group_by(city) %>%
      summarise(
        avg_temp = mean(main_temp, na.rm = TRUE),
        avg_humidity = mean(main_humidity, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Adicionar dados fictícios de procura baseados na temperatura
    city_summaries <- city_summaries %>%
      mutate(
        estimated_demand = 200 + 50 * (avg_temp - 10) - 10 * (avg_humidity/10 - 5)^2
      ) %>%
      arrange(desc(estimated_demand))
    
    return(city_summaries)
  })
  
  # === OUTPUTS DO DASHBOARD ===
  
  # 1. Value Boxes
  output$total_demand_box <- renderValueBox({
    pred <- predictions()
    if (nrow(pred) == 0) {
      total <- 0
    } else {
      total <- sum(pred$.pred, na.rm = TRUE)
    }
    
    valueBox(
      format(round(total), big.mark = ".", decimal.mark = ","),
      "Procura Total Prevista",
      icon = icon("bicycle"),
      color = "blue"
    )
  })
  
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
  
  # 2. Gráfico de procura por hora (versão simplificada e corrigida)
  output$hourly_demand_plot <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    hourly_data <- pred %>%
      group_by(hour, day_period) %>%
      summarise(avg_demand = mean(.pred, na.rm = TRUE), .groups = "drop")
    
    # Dados agregados para linha de tendência
    hourly_summary <- hourly_data %>%
      group_by(hour) %>%
      summarise(avg_demand = mean(avg_demand), .groups = "drop")
    
    # Criar gráfico
    ggplot() +
      # Adicionar barras
      geom_col(data = hourly_data, 
               aes(x = hour, y = avg_demand, fill = day_period),
               position = "dodge", alpha = 0.8) +
      # Adicionar linha de tendência
      geom_line(data = hourly_summary,
                aes(x = hour, y = avg_demand),
                color = "darkred", linewidth = 1) +
      # Configurar escalas e temas
      scale_fill_viridis_d() +
      scale_x_continuous(breaks = 0:23) +
      labs(
        title = paste("Procura Prevista para", input$city),
        x = "Hora do Dia", 
        y = "Procura Média",
        fill = "Período"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })
  
  # 3. Gráfico radar (simplificado)
  output$radar_chart <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Agrupar por período
    period_data <- pred %>%
      group_by(day_period) %>%
      summarise(
        avg_demand = mean(.pred, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Criar gráfico de barras circular
    ggplot(period_data, aes(x = day_period, y = avg_demand, fill = day_period)) +
      geom_col() +
      geom_text(aes(label = round(avg_demand)), vjust = -0.5, color = "black", size = 4) +
      scale_fill_viridis_d() +
      coord_polar() +
      labs(
        title = "Procura Média por Período do Dia",
        x = "",
        y = ""
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none"
      )
  })
  
  # 4. Gráfico de pizza para distribuição por período
  output$period_distribution_plot <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Preparar dados
    period_data <- pred %>%
      group_by(day_period) %>%
      summarise(
        total_demand = sum(.pred, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        percentage = total_demand / sum(total_demand) * 100,
        ypos = cumsum(percentage) - 0.5 * percentage
      )
    
    # Criar gráfico de pizza
    ggplot(period_data, aes(x = "", y = percentage, fill = day_period)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(y = ypos, label = paste0(round(percentage), "%")), 
                color = "white", fontface = "bold", size = 5) +
      scale_fill_viridis_d() +
      labs(
        title = "Distribuição da Procura por Período",
        fill = "Período"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      )
  })
  
  # 5. Gráfico de padrão horário
  output$hourly_pattern_plot <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Agregar por hora e dia da semana
    hourly_pattern <- pred %>%
      group_by(hour, weekday) %>%
      summarise(avg_demand = mean(.pred, na.rm = TRUE), .groups = "drop")
    
    # Criar gráfico de linhas
    ggplot(hourly_pattern, aes(x = hour, y = avg_demand, color = weekday, group = weekday)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_viridis_d() +
      scale_x_continuous(breaks = 0:23) +
      labs(
        title = "Padrão de Procura por Hora e Dia da Semana",
        x = "Hora do Dia",
        y = "Procura Média",
        color = "Dia da Semana"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })
  
  # 6. Gráfico de distribuição por hora
  output$hour_distribution_plot <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Criar gráfico de densidade
    ggplot(pred, aes(x = .pred, color = as.factor(hour), group = hour)) +
      geom_density(alpha = 0.5) +
      scale_color_viridis_d() +
      labs(
        title = "Distribuição da Procura por Hora do Dia",
        x = "Procura Prevista",
        y = "Densidade",
        color = "Hora"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "right"
      )
  })
  
  # 7. Tabela de procura por hora
  output$hourly_table <- renderDT({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(NULL)
    }
    
    hourly_data <- pred %>%
      group_by(hour) %>%
      summarise(
        `Procura Média` = round(mean(.pred, na.rm = TRUE), 1),
        `Procura Máxima` = round(max(.pred, na.rm = TRUE), 1),
        `Procura Mínima` = round(min(.pred, na.rm = TRUE), 1),
        `Temperatura Média` = round(mean(temperature_c, na.rm = TRUE), 1),
        `Humidade Média` = round(mean(humidity_percent, na.rm = TRUE), 0),
        .groups = "drop"
      ) %>%
      arrange(hour)
    
    datatable(
      hourly_data,
      options = list(
        pageLength = 24,
        scrollY = "500px",
        dom = 'ft'
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Procura Média',
        background = styleColorBar(c(0, max(hourly_data$`Procura Média`)), 'lightblue'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # 8. Heatmap: Procura por Hora e Dia
  output$hour_day_heatmap <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Agregar dados por hora e dia
    heatmap_data <- pred %>%
      group_by(date, hour) %>%
      summarise(
        demand = mean(.pred, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Formatar a data para exibição
      mutate(date_label = format(date, "%d/%m"))
    
    # Criar heatmap
    ggplot(heatmap_data, aes(x = hour, y = date_label, fill = demand)) +
      geom_tile(color = "white", linewidth = 0.1) +
      scale_fill_viridis_c(option = "inferno") +
      scale_x_continuous(breaks = seq(0, 23, 3)) +
      labs(
        title = "Heatmap: Procura por Hora e Dia",
        subtitle = paste("Cidade:", input$city),
        x = "Hora do Dia",
        y = "Data",
        fill = "Procura"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        axis.text.y = element_text(size = 8)
      )
  })
  
  # 9. Heatmap: Temperatura por Hora
  output$temp_hour_heatmap <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Agregar temperatura por hora e dia
    temp_data <- data %>%
      group_by(date, hour) %>%
      summarise(
        temp = mean(temperature_c, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Formatar a data para exibição
      mutate(date_label = format(date, "%d/%m"))
    
    # Criar heatmap
    ggplot(temp_data, aes(x = hour, y = date_label, fill = temp)) +
      geom_tile(color = "white", linewidth = 0.1) +
      scale_fill_viridis_c(option = "plasma") +
      scale_x_continuous(breaks = seq(0, 23, 4)) +
      labs(
        title = "Temperatura por Hora e Dia",
        x = "Hora do Dia",
        y = "Data",
        fill = "Temp. (°C)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text.y = element_text(size = 8)
      )
  })
  
  # 10. Heatmap: Correlação entre Variáveis
  output$correlation_heatmap <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Selecionar apenas variáveis numéricas para correlação
    numeric_cols <- pred %>%
      select(.pred, temperature_c, humidity_percent, 
             wind_speed_m_s, visibility_10m, hour) %>%
      # Filtrar colunas com desvio padrão zero
      select_if(function(x) sd(x, na.rm = TRUE) > 0)
    
    # Renomear colunas
    col_names <- c("Procura", "Temperatura", "Humidade", 
                   "Vento", "Visibilidade", "Hora")[1:ncol(numeric_cols)]
    colnames(numeric_cols) <- col_names
    
    # Calcular matriz de correlação
    cor_matrix <- cor(numeric_cols, use = "pairwise.complete.obs")
    
    # Transformar para formato longo para ggplot
    cor_melted <- melt(cor_matrix)
    
    # Criar heatmap de correlação
    ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "#4575b4", 
        mid = "white", 
        high = "#d73027", 
        midpoint = 0, 
        limits = c(-1, 1),
        name = "Correlação"
      ) +
      # Adicionar valores de correlação
      geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
      labs(
        title = "Correlação entre Variáveis",
        x = "",
        y = ""
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10)
      )
  })
  
  # 11. Gráfico de temperatura vs. procura
  output$temp_demand_plot <- renderPlot({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Calcular correlação
    correlation <- cor(pred$temperature_c, pred$.pred, use = "complete.obs")
    
    # Criar gráfico de dispersão 
    ggplot(pred, aes(x = temperature_c, y = .pred)) +
      # Adicionar pontos coloridos por período
      geom_point(aes(color = day_period), alpha = 0.6, size = 3) +
      # Adicionar linha de tendência
      geom_smooth(method = "loess", color = "black", se = TRUE) +
      # Adicionar texto destacando correlação
      annotate(
        "text", x = min(pred$temperature_c) + 1, y = max(pred$.pred) * 0.9,
        label = paste("Correlação:", round(correlation, 2)),
        hjust = 0, size = 5, fontface = "bold", color = "darkblue"
      ) +
      # Configurar escalas e cores
      scale_color_viridis_d() +
      labs(
        title = "Relação entre Temperatura e Procura de Bicicletas",
        subtitle = paste("Cidade:", input$city),
        x = "Temperatura (°C)",
        y = "Procura Prevista",
        color = "Período do Dia"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      )
  })
  
  # 12. Gráfico de área para previsão de temperatura
  output$temp_forecast_area_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Preparar dados agrupados por data
    temp_data <- data %>%
      group_by(date) %>%
      summarise(
        temp_min = min(temperature_c, na.rm = TRUE),
        temp_avg = mean(temperature_c, na.rm = TRUE),
        temp_max = max(temperature_c, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Criar gráfico de área
    ggplot(temp_data, aes(x = date)) +
      # Área para o intervalo de temperatura
      geom_ribbon(aes(ymin = temp_min, ymax = temp_max), alpha = 0.3, fill = "skyblue") +
      # Linha para temperatura média
      geom_line(aes(y = temp_avg), color = "#1F77B4", linewidth = 1.2) +
      # Pontos para temperatura média
      geom_point(aes(y = temp_avg), color = "#1F77B4", size = 3) +
      # Valores de temperatura
      geom_text(aes(y = temp_max + 1, label = round(temp_max, 1)), 
                color = "darkred", size = 3, vjust = 0) +
      geom_text(aes(y = temp_min - 1, label = round(temp_min, 1)), 
                color = "darkblue", size = 3, vjust = 1) +
      # Configurar escalas e rótulos
      scale_x_date(date_labels = "%d/%m", date_breaks = "1 day") +
      labs(
        title = "Previsão de Temperatura",
        subtitle = paste("Cidade:", input$city),
        x = "Data",
        y = "Temperatura (°C)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # 13. Violin Plot para temperatura por período
  output$temp_period_violin_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Criar violin plot
    ggplot(data, aes(x = day_period, y = temperature_c, fill = day_period)) +
      # Adicionar violin plot
      geom_violin(alpha = 0.7, trim = FALSE) +
      # Adicionar boxplot por dentro
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
      # Configurar cores e rótulos
      scale_fill_viridis_d() +
      labs(
        title = "Distribuição de Temperatura por Período",
        x = "Período do Dia",
        y = "Temperatura (°C)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 10),
        legend.position = "none"
      )
  })
  
  # 14. Gráfico 3D interativo: Hora x Temperatura x Procura
  output$temp_hour_demand_3d <- renderPlotly({
    pred <- predictions()
    if (nrow(pred) == 0) {
      return(plot_ly() %>% add_annotations(text = "Sem dados disponíveis", showarrow = FALSE))
    }
    
    # Criar gráfico 3D interativo
    plot_ly(pred, x = ~hour, y = ~temperature_c, z = ~.pred, color = ~day_period,
            type = "scatter3d", mode = "markers",
            marker = list(size = 5, opacity = 0.8)) %>%
      layout(
        title = list(text = "Relação 3D: Hora x Temperatura x Procura"),
        scene = list(
          xaxis = list(title = "Hora do Dia"),
          yaxis = list(title = "Temperatura (°C)"),
          zaxis = list(title = "Procura Prevista")
        )
      )
  })
  
  # 15. Mapa interativo
  output$demand_map <- renderLeaflet({
    # Obter informações da cidade selecionada
    city_name <- strsplit(input$city, ",")[[1]][1]
    
    # Tentar encontrar as coordenadas
    city_info <- cities %>% 
      filter(grepl(city_name, city, ignore.case = TRUE)) %>% 
      slice(1)
    
    # Coordenadas default para as principais cidades
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
    
    # Obter valor total da procura
    pred <- predictions()
    total_demand <- if(nrow(pred) > 0) round(sum(pred$.pred, na.rm = TRUE)) else 0
    
    # Obter temperatura média
    data <- filtered_data()
    avg_temp <- if(nrow(data) > 0) round(mean(data$temperature_c, na.rm = TRUE), 1) else "N/A"
    
    # Criar mapa interativo aprimorado
    leaflet() %>%
      # Adicionar camada base
      addTiles() %>%
      # Centrar o mapa
      setView(lng = lon, lat = lat, zoom = 13) %>%
      # Adicionar marcador principal
      addMarkers(
        lng = lon,
        lat = lat,
        popup = paste0(
          "<b>", city_name, "</b><br>",
          "Procura Total Prevista: ", format(total_demand, big.mark = ".", decimal.mark = ","), "<br>",
          "Temperatura Média: ", avg_temp, "°C<br>",
          "Período: ", format(input$date_range[1]), " a ", format(input$date_range[2])
        )
      ) %>%
      # Adicionar círculo com tamanho proporcional à procura
      addCircles(
        lng = lon,
        lat = lat,
        radius = sqrt(total_demand) * 25,
        color = "#FF4500",
        fillColor = "#FF4500",
        fillOpacity = 0.2,
        weight = 2,
        popup = paste0("Procura estimada: ", format(total_demand, big.mark = ".", decimal.mark = ","))
      )
  })
  
  # 16. Gráfico de Lollipop (comparação entre cidades)
  output$cities_lollipop_chart <- renderPlot({
    data <- cities_data()
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados disponíveis") +
               theme_void())
    }
    
    # Limitar para as 10 principais cidades
    data <- head(data, 10)
    
    # Criar gráfico de lollipop
    ggplot(data, aes(x = reorder(city, estimated_demand), y = estimated_demand)) +
      # Adicionar segmentos (hastes)
      geom_segment(aes(xend = city, yend = 0, color = avg_temp), linewidth = 1.2) +
      # Adicionar pontos (cabeças)
      geom_point(aes(color = avg_temp), size = 8) +
      # Adicionar valores
      geom_text(aes(label = round(estimated_demand)), 
                color = "white", size = 3, fontface = "bold") +
      # Adicionar rótulos de temperatura
      geom_text(aes(label = paste0(round(avg_temp, 1), "°C"), y = 20), 
                hjust = 0, color = "gray30", size = 3) +
      # Configurar cores e escalas
      scale_color_viridis_c(option = "plasma") +
      coord_flip() +
      labs(
        title = "Comparação de Procura Estimada entre Cidades",
        subtitle = "Baseado na temperatura média prevista",
        x = "",
        y = "Procura Estimada de Bicicletas",
        color = "Temp. (°C)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom"
      )
  })
}

# Executar a aplicação
shinyApp(ui, server)
