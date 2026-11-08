library(shiny)

ui <- navbarPage("Production Planning",
                 # Learning Curve Module (Eksik olan satır buradaydı)
                 tabPanel("Learning Curve", learning_curve("learning_curve")),
                 
                 # Grouped Forecasting Module (Stationary, Trend & Seasonal, Time Series / ARIMA)
                 navbarMenu("Forecasting",
                            tabPanel("1. Stationary Series", forecast_stationary_ui("fc_stat")),
                            tabPanel("2. Trend & Seasonal", forecast_trend_ui("fc_trend")),
                            tabPanel("3. Time Series / ARIMA", forecast_timeseries_ui("fc_ts"))
                 )
)

server <- function(input, output, session) {
  # Server call for Learning Curve
  learning_server("learning_curve")
  
  # Forecasting server calls
  forecast_stationary_server("fc_stat")
  forecast_trend_server("fc_trend")
  forecast_timeseries_server("fc_ts")
}

shinyApp(ui, server)