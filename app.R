library(shiny)
library(readxl)
source("R/moving_average.R")

ui <- navbarPage("Production Planning",
  tabPanel("Learning Curve", learning_ui("learning")),
  tabPanel("Moving Average", moving_average_ui("ma_1"))
)

server <- function(input, output, session) {
  learning_server("learning")
  moving_average_server("ma_1")
}

shinyApp(ui, server)
