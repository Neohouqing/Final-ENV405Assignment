library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(openair)
library(ggplot2)

# Helper: convert wind direction (degrees) to 8 sectors
wd_to_sector <- function(wd) {
  wd <- wd %% 360
  cuts   <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
  labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  cut(wd, breaks = cuts, labels = labels,
      include.lowest = TRUE, right = FALSE)
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
      helpText("If no file is uploaded, the app uses ../data/wind.csv."),
      
      sliderInput(
        "ws_range",
        "Wind speed range (m/s)",
        min = 0,
        max = 20,
        value = c(0, 20),
        step = 0.5
      ),
      selectInput(
        "sector_filter",
        "Filter by wind sector",
        choices = c("All", "N", "NE", "E", "SE", "S", "SW", "W", "NW"),
        selected = "All"
      ),
      checkboxInput(
        "show_points",
        "Show points on wind speed time series",
        value = TRUE
      ),
      
      hr(),
      h4("Download filtered data"),
      downloadButton("download_filtered", "Download CSV"),
      
      hr(),
      h4("Filter summary"),
      verbatimTextOutput("filter_summary"),
      
      hr(),
      h4("Statistical test: mean ws by sector"),
      selectInput(
        "test_sector1",
        "Group A sector:",
        choices = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
        selected = "N"
      ),
      selectInput(
        "test_sector2",
        "Group B sector:",
        choices = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
        selected = "S"
      ),
      actionButton("run_test", "Run t-test")
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
      tableOutput("wind_stats"),
      h4("Two-sample t-test on wind speed"),
      verbatimTextOutput("ttest_result")
    )
  )
)

server <- function(input, output, session) {
  
  # -------- base data + filters --------
  wind <- reactive({
    df <- if (!is.null(input$file_wind)) {
      read_csv(input$file_wind$datapath, show_col_types = FALSE)
    } else {
      read_csv("../data/wind.csv", show_col_types = FALSE)
    }
    df %>% mutate(date = ymd_hms(date))
  })
  
  filtered_wind <- reactive({
    df <- wind()
    df <- df %>%
      filter(!is.na(ws), !is.na(wd)) %>%
      mutate(direction_sector = wd_to_sector(wd)) %>%
      filter(
        ws >= input$ws_range[1],
        ws <= input$ws_range[2],
        if (input$sector_filter == "All") TRUE else direction_sector == input$sector_filter
      )
    df
  })
  
  output$filter_summary <- renderText({
    df <- filtered_wind()
    paste0(
      "Rows after filtering: ", nrow(df), "\n",
      "WS range: ", input$ws_range[1], "–", input$ws_range[2], " m/s\n",
      "Sector: ", input$sector_filter
    )
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("filtered_wind_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      df <- filtered_wind()
      write_csv(df, file)
    }
  )
  
  # -------- tables & plots using filtered data --------
  direction_freq <- reactive({
    df <- filtered_wind()
    df %>%
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
  
  output$wind_rose <- renderPlot({
    df <- filtered_wind()
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
  
  output$ws_time <- renderPlot({
    df <- filtered_wind()
    p <- ggplot(df, aes(x = date, y = ws)) +
      geom_line() +
      labs(
        title = "Wind speed time series",
        x = "Time",
        y = "Wind speed"
      ) +
      theme_minimal()
    if (isTRUE(input$show_points)) {
      p <- p + geom_point(size = 0.6)
    }
    p
  })
  
  output$ws_hist <- renderPlot({
    df <- filtered_wind()
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
    df <- filtered_wind()
    df %>%
      summarise(
        n = n(),
        mean_ws   = mean(ws, na.rm = TRUE),
        median_ws = median(ws, na.rm = TRUE),
        min_ws    = min(ws, na.rm = TRUE),
        max_ws    = max(ws, na.rm = TRUE),
        sd_ws     = sd(ws, na.rm = TRUE)
      )
  }, digits = 2)
  
  footprint_points <- reactive({
    df <- filtered_wind()
    df %>%
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
  
  # -------- statistical test: ws by sector (using filtered data) --------
  wind_with_sector <- reactive({
    filtered_wind()   # already has direction_sector
  })
  
  ttest_result <- eventReactive(input$run_test, {
    df <- wind_with_sector()
    
    group_a <- df %>%
      filter(direction_sector == input$test_sector1) %>%
      pull(ws)
    group_b <- df %>%
      filter(direction_sector == input$test_sector2) %>%
      pull(ws)
    
    if (length(group_a) < 5 || length(group_b) < 5) {
      return("Not enough data in one or both sectors (need at least 5 points each).")
    }
    
    res <- t.test(group_a, group_b)
    
    paste0(
      "Two-sample t-test for mean wind speed\n",
      "Group A sector: ", input$test_sector1,
      " (n = ", length(group_a), ")\n",
      "Group B sector: ", input$test_sector2,
      " (n = ", length(group_b), ")\n",
      "t = ", round(res$statistic, 3),
      ", df = ", round(res$parameter, 1),
      ", p-value = ", signif(res$p.value, 3), "\n",
      "Mean ws A = ", round(mean(group_a), 2), " m/s; ",
      "Mean ws B = ", round(mean(group_b), 2), " m/s\n",
      if (res$p.value < 0.05) {
        "Conclusion: difference is statistically significant at the 5% level."
      } else {
        "Conclusion: no statistically significant difference at the 5% level."
      }
    )
  })
  
  output$ttest_result <- renderText({
    ttest_result()
  })
}

shinyApp(ui = ui, server = server)
