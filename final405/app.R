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
  cut(
    wd,
    breaks = cuts,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE
  )
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
      helpText("If no file is uploaded, the app uses data/wind.csv")
    ),
    mainPanel(
      plotOutput("wind_rose"),
      h4("Wind direction frequency table"),
      tableOutput("direction_table"),
      plotOutput("direction_bar")
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
    df %>%
      mutate(date = ymd_hms(date))
  })
  
  output$wind_rose <- renderPlot({
    df <- wind()
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
      mutate(direction_sector = wd_to_sector(ws)) %>%  # 故意的错误：用了 ws
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
}

shinyApp(ui = ui, server = server)


