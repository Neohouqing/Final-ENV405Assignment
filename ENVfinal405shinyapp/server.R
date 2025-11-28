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
axis_y_flat <- theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
server <- function(input, output, session) {
  wind <- reactive({
    df <- if (!is.null(input$file_wind)) {
      read_csv(input$file_wind$datapath, show_col_types = FALSE)
    } else {
      read_csv("wind.csv", show_col_types = FALSE)
    }
    df %>% mutate(date = ymd_hms(date))
  })
  
  filtered_wind <- reactive({
    df <- wind() %>%
      filter(!is.na(ws), !is.na(wd)) %>%
      mutate(direction_sector = wd_to_sector(wd)) %>%
      filter(ws >= input$ws_range[1], ws <= input$ws_range[2])
    if (input$sector_filter != "All") {
      df <- df %>% filter(direction_sector == input$sector_filter)
    }
    df
  })
  
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
  
  footprint_points <- reactive({
    filtered_wind() %>%
      mutate(
        x = ws * sin(wd * pi / 180),
        y = ws * cos(wd * pi / 180)
      )
  })
  
  sector_stats <- reactive({
    filtered_wind() %>%
      group_by(direction_sector) %>%
      summarise(
        n = n(),
        mean_ws = mean(ws, na.rm = TRUE),
        sd_ws = sd(ws, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  trend_data <- reactive({
    filtered_wind() %>%
      arrange(date) %>%
      mutate(time_numeric = as.numeric(date))
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
  
  output$direction_bar <- renderPlot({
    df <- direction_freq()
    ggplot(df, aes(x = direction_sector, y = rel_freq_percent)) +
      geom_col() +
      labs(
        title = "Relative frequency of wind directions",
        x = "Wind direction sector",
        y = "Relative frequency (%)"
      ) +
      theme_minimal() +
      axis_y_flat
  })
  
  output$ws_time <- renderPlot({
    df <- filtered_wind()
    p <- ggplot(df, aes(x = date, y = ws)) +
      geom_line()
    if (input$show_points) {
      p <- p + geom_point(alpha = 0.5)
    }
    p +
      labs(
        title = "Wind speed time series",
        x = "Time",
        y = "Wind speed"
      ) +
      theme_minimal() +
      axis_y_flat
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
      theme_minimal() +
      axis_y_flat
  })
  
  output$wind_stats <- renderTable({
    df <- filtered_wind()
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
  
  output$direction_table <- renderTable({
    direction_freq()
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
      theme_minimal() +
      axis_y_flat
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
  
  output$filter_summary <- renderText({
    df <- filtered_wind()
    paste0(
      "Rows after filtering: ", nrow(df),
      " | WS range: ", input$ws_range[1], "–", input$ws_range[2], " m/s",
      " | Sector: ", input$sector_filter
    )
  })
  
  output$sector_means <- renderTable({
    sector_stats()
  }, digits = 2)
  
  output$ttest_result <- renderPrint({
    df <- filtered_wind()
    a <- df %>% filter(direction_sector == input$group_a_sector) %>% pull(ws)
    b <- df %>% filter(direction_sector == input$group_b_sector) %>% pull(ws)
    if (length(a) < 2 || length(b) < 2) {
      cat("Not enough data in one of the groups for t-test.")
    } else {
      t.test(a, b, var.equal = FALSE)
    }
  })
  
  output$ws_trend <- renderPlot({
    df <- trend_data()
    if (nrow(df) < 3) {
      plot.new()
      text(0.5, 0.5, "Not enough data for trend analysis")
    } else {
      if (input$trend_model == "Linear") {
        ggplot(df, aes(x = date, y = ws)) +
          geom_point(alpha = 0.4, size = 0.6) +
          geom_smooth(method = "lm", se = TRUE) +
          labs(
            title = "Linear trend of wind speed",
            x = "Time",
            y = "Wind speed"
          ) +
          theme_minimal() +
          axis_y_flat
      } else {
        ggplot(df, aes(x = date, y = ws)) +
          geom_point(alpha = 0.4, size = 0.6) +
          geom_smooth(method = "loess", se = TRUE) +
          labs(
            title = "Loess smooth of wind speed",
            x = "Time",
            y = "Wind speed"
          ) +
          theme_minimal() +
          axis_y_flat
      }
    }
  })
  
  output$trend_summary <- renderTable({
    df <- trend_data()
    if (nrow(df) < 3) {
      return(data.frame(message = "Not enough data for trend analysis."))
    }
    if (input$trend_model == "Linear") {
      fit <- lm(ws ~ time_numeric, data = df)
      sm <- summary(fit)
      slope <- coef(sm)[2, "Estimate"]
      p_value <- coef(sm)[2, "Pr(>|t|)"]
      r2 <- sm$r.squared
      n <- nobs(fit)
      data.frame(
        model = "Linear",
        slope = round(slope, 6),
        p_value = signif(p_value, 3),
        r_squared = round(r2, 3),
        n = n
      )
    } else {
      data.frame(
        model = "Loess",
        note = "Non-parametric smooth; no single slope estimate."
      )
    }
  }, digits = 4, rownames = FALSE)
}
