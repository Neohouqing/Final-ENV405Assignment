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

ui <- fluidPage(
  titlePanel("ENV405 wind analysis app"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file_wind",
        "Upload wind CSV file",
        accept = ".csv"
      ),
      helpText("If no file is uploaded, the app uses wind.csv in the app folder."),
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
      radioButtons(
        "trend_model",
        "Trend model",
        choices = c("Linear", "Loess"),
        selected = "Linear"
      ),
      downloadButton("download_filtered", "Download CSV"),
      h4("Statistical test: mean ws by sector"),
      selectInput(
        "group_a_sector",
        "Group A sector",
        choices = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
        selected = "N"
      ),
      selectInput(
        "group_b_sector",
        "Group B sector",
        choices = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
        selected = "S"
      )
    ),
    mainPanel(
      h3("Wind rose"),
      plotOutput("wind_rose"),
      h3("Wind direction frequency"),
      plotOutput("direction_bar"),
      h3("Wind speed time series"),
      plotOutput("ws_time"),
      h3("Wind speed distribution"),
      plotOutput("ws_hist"),
      h3("Wind speed statistics"),
      tableOutput("wind_stats"),
      h3("Footprint-style plot"),
      plotOutput("footprint_plot"),
      h3("Wind direction frequency table"),
      tableOutput("direction_table"),
      h3("Filter summary"),
      textOutput("filter_summary"),
      h3("Sector means and t-test"),
      tableOutput("sector_means"),
      verbatimTextOutput("ttest_result"),
      h3("Trend analysis of wind speed"),
      plotOutput("ws_trend"),
      tableOutput("trend_summary")
    )
  )
)
