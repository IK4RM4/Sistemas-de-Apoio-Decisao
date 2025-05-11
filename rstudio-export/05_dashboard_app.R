# 05_dashboard_app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(leaflet)

# ──────── Carregamento dos dados ────────
model <- readRDS("outputs/models/weather_model.rds")
weather <- read_csv("data/processed/weather_forecast.csv")
cities <- read_csv("data/processed/world_cities.csv")

# Preparar coordenadas das cidades
cities <- cities %>%
  mutate(
    lat = lat_d + lat_m / 60 + lat_s / 3600,
    lon = ifelse(ew == "W", -(lon_d + lon_m / 60 + lon_s / 3600),
                 (lon_d + lon_m / 60 + lon_s / 3600)),
    city = str_to_title(city)
  )


# ──────── Interface ────────
ui <- fluidPage(
  titlePanel("Previsão de Aluguer de Bicicletas por Clima"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cidade", "Seleciona a cidade:", choices = unique(weather$city)),
      sliderInput("horas", "Horas futuras (3h em 3h):", 1, 40, 5)
    ),
    mainPanel(
      plotOutput("grafico_pred"),
      tableOutput("tabela_pred"),
      leafletOutput("mapa_cidade")
    )
  )
)

# ──────── Servidor ────────
server <- function(input, output) {
  
  # Filtra e prepara os dados meteorológicos
  dados_filtrados <- reactive({
    weather %>%
      filter(city == input$cidade) %>%
      slice(1:input$horas) %>%
      mutate(dt_txt = ymd_hms(dt_txt)) %>%
      transmute(
        date = as_date(dt_txt),
        hour = hour(dt_txt),
        temperature_c = main_temp,
        humidity_percent = main_humidity,
        wind_speed_m_s = wind_speed,
        visibility_10m = visibility / 10,
        dew_point_temperature_c = temperature_c - ((100 - humidity_percent) / 5),
        solar_radiation_mj_m2 = 0,
        rainfall_mm = 0,
        snowfall_cm = 0
      )
  })
  
  # Gera previsões
  predicoes <- reactive({
    df <- dados_filtrados()
    df$hour <- as.factor(df$hour)
    predict(model, new_data = df) %>% bind_cols(df)
  })
  
  # Gráfico de previsão
  output$grafico_pred <- renderPlot({
    dados <- predicoes()
    ggplot(dados, aes(x = as.POSIXct(paste(date, hour), format = "%Y-%m-%d %H"), y = .pred)) +
      geom_line(color = "blue") +
      labs(
        title = paste("Previsão de Aluguer para", input$cidade),
        x = "Data e Hora", y = "Aluguer Estimado"
      )
  })
  
  # Tabela com previsões
  output$tabela_pred <- renderTable({
    predicoes() %>%
      mutate(date = as.character(date)) %>%
      select(date, hour, .pred) %>%
      rename(`Data` = date, `Hora` = hour, `Previsão` = .pred)
  })
  
  # Mapa com localização da cidade
  output$mapa_cidade <- renderLeaflet({
    cidade_info <- cities %>%
      filter(city == str_trim(str_to_title(str_split(input$cidade, ",")[[1]][1]))) %>%
      slice(1)
    
    
    if (nrow(cidade_info) == 0 || is.na(cidade_info$lat) || is.na(cidade_info$lon)) {
      leaflet() %>% 
        addTiles() %>% 
        addPopups(0, 0, "Coordenadas não encontradas para esta cidade.")
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = cidade_info$lon, lat = cidade_info$lat, zoom = 10) %>%
        addMarkers(
          lng = cidade_info$lon,
          lat = cidade_info$lat,
          popup = paste("Cidade:", cidade_info$city)
        )
    }
  })
  
}

# ──────── Executar a app ────────
shinyApp(ui = ui, server = server)
