library(shiny)
source("R/ogrenme.R")
source("R/forecast_stationary.R")
source("R/forecast_trend.R")
source("R/forecast_timeseries.R")

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_ui("learning_curve")),
                 navbarMenu("Forecasting",
                            tabPanel("1. Stationary Series", forecast_stationary_ui("fc_stat")),
                            tabPanel("2. Trend & Seasonal", forecast_trend_ui("fc_trend")),
                            tabPanel("3. Time Series / ARIMA", forecast_timeseries_ui("fc_ts"))
                 )
)

server <- function(input, output, session) {
  learning_server("learning_curve")
  forecast_stationary_server("fc_stat")
  forecast_trend_server("fc_trend")
  forecast_timeseries_server("fc_ts")
}

shinyApp(ui, server)