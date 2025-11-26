library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(openair)
library(ggplot2)

wd_to_sector <- function(wd) {
  wd <- wd %% 360
  cuts <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
  labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  cut(wd, breaks = cuts, labels = labels, include.lowest = TRUE, right = FALSE)
}

ui <- fluidPage(
  titlePanel("ENV405 wind analysis app"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file_wind",
        "Upload wind CSV file",
        accept = ".csv"
      ),
      helpText("If no file is uploaded, the app uses ../data/wind.csv.")
    ),
    mainPanel(
      plotOutput("wind_rose"),
      plotOutput("direction_bar"),
      plotOutput("ws_time"),
      plotOutput("ws_hist"),
      plotOutput("footprint_plot"),
      h4("Wind direction frequency table"),
      tableOutput("direction_table"),
      h4("Wind speed summary"),
      tableOutput("wind_stats")
    )
  )
)

server <- function(input, output, session) {
  
  wind <- reactive({
    df <- if (!is.null(input$file_wind)) {
      read_csv(input$file_wind$datapath, show_col_types = FALSE)
    } else {
      read_csv("../data/wind.csv", show_col_types = FALSE)
    }
    df %>% mutate(date = ymd_hms(date))
  })
  
  output$wind_rose <- renderPlot({
    df <- wind()
    validate(
      need("ws" %in% names(df), "Cannot find column 'ws' in data."),
      need("wd" %in% names(df), "Cannot find column 'wd' in data.")
    )
    windRose(
      mydata = df,
      ws = "ws",
      wd = "wd",
      paddle = TRUE,
      key.position = "right",
      main = "Wind rose"
    )
  })
  
  direction_freq <- reactive({
    df <- wind()
    df %>%
      filter(!is.na(wd)) %>%
      mutate(direction_sector = wd_to_sector(wd)) %>%
      count(direction_sector, name = "count") %>%
      mutate(
        total = sum(count),
        rel_freq = count / total,
        rel_freq_percent = round(rel_freq * 100, 1)
      ) %>%
      arrange(direction_sector)
  })
  
  output$direction_table <- renderTable({
    direction_freq()
  })
  
  output$direction_bar <- renderPlot({
    df <- direction_freq()
    ggplot(df, aes(x = direction_sector, y = rel_freq_percent)) +
      geom_col() +
      labs(
        title = "Relative frequency of wind directions",
        x = "Wind direction sector",
        y = "Relative frequency (%)"
      ) +
      theme_minimal()
  })
  
  output$ws_time <- renderPlot({
    df <- wind()
    ggplot(df, aes(x = date, y = ws)) +
      geom_line() +
      labs(
        title = "Wind speed time series",
        x = "Time",
        y = "Wind speed"
      ) +
      theme_minimal()
  })
  
  output$ws_hist <- renderPlot({
    df <- wind()
    ggplot(df, aes(x = ws)) +
      geom_histogram(bins = 30) +
      labs(
        title = "Distribution of wind speed",
        x = "Wind speed",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$wind_stats <- renderTable({
    df <- wind()
    df %>%
      summarise(
        n = n(),
        mean_ws = mean(ws, na.rm = TRUE),
        median_ws = median(ws, na.rm = TRUE),
        min_ws = min(ws, na.rm = TRUE),
        max_ws = max(ws, na.rm = TRUE),
        sd_ws = sd(ws, na.rm = TRUE)
      )
  }, digits = 2)
  
  footprint_points <- reactive({
    df <- wind()
    df %>%
      filter(!is.na(ws), !is.na(wd)) %>%
      mutate(
        x = ws * sin(wd * pi / 180),
        y = ws * cos(wd * pi / 180)
      )
  })
  
  output$footprint_plot <- renderPlot({
    df <- footprint_points()
    ggplot(df, aes(x = x, y = y)) +
      stat_density_2d_filled() +
      labs(
        title = "Simple footprint-style map",
        x = "Cross-wind distance (arb. units)",
        y = "Along-wind distance (arb. units)"
      ) +
      coord_equal() +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
