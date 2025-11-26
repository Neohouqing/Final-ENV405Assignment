library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

wind <- read_csv("../data/wind.csv", show_col_types = FALSE) |>
  mutate(date = ymd_hms(date))

ui <- fluidPage(
  titlePanel("ENV405 - Wind data explorer"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        "n_rows",
        "Number of rows to show:",
        value = 20,
        min = 5,
        max = 100,
        step = 5
      )
    ),
    mainPanel(
      h4("Preview of wind data"),
      tableOutput("preview"),
      h4("Wind speed time series"),
      plotOutput("ws_time")
    )
  )
)

server <- function(input, output, session) {
  output$preview <- renderTable({
    head(wind, input$n_rows)
  })
  
  output$ws_time <- renderPlot({
    ggplot(wind, aes(x = date, y = ws)) +
      geom_line() +
      labs(
        title = "Wind speed time series",
        x = "Time",
        y = "Wind speed"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

