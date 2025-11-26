library(shiny)
library(readr)

wind <- read_csv("../data/wind.csv")

ui <- fluidPage(
  titlePanel("ENV405 - Wind data explorer (preview only)"),
  mainPanel(
    h4("First 20 rows of wind data"),
    tableOutput("preview")
  )
)

server <- function(input, output, session) {
  output$preview <- renderTable({
    head(wind, 20)
  })
}

shinyApp(ui = ui, server = server)
