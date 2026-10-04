library(shiny)

moving_average_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # Excel yerine sadece CSV kabul edecek sekilde guncellendi
        fileInput(ns("file"), "Upload a CSV file (.csv)", accept = c(".csv", "text/csv")),
        uiOutput(ns("col_selector")),
        radioButtons(ns("choice_type"), "How should N be chosen?",
                     choices = c("I will choose N" = "manual", "Find the best N" = "auto")),
        conditionalPanel(
          condition = sprintf("input[['%s']] == 'manual'", ns("choice_type")),
          numericInput(ns("n_manual"), "N (number of past periods)", value = 3, min = 1, max = 50)
        ),
        conditionalPanel(
          condition = sprintf("input[['%s']] == 'auto'", ns("choice_type")),
          selectInput(ns("metric"), "Select error metric to optimize:",
                      choices = c("MAD" = "MAD", "MSE" = "MSE", "MAPE (%)" = "MAPE")),
          numericInput(ns("max_n_auto"), "Max N to test:", value = 10, min = 2, max = 30)
        )
      ),
      mainPanel(
        textOutput(ns("next_period_forecast")),
        plotOutput(ns("ma_plot")),
        h4("Error measures"),
        tableOutput(ns("error_table")),
        h4("Forecasts by period"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # readxl kaldirildi, yalnizca R'in temel read.csv fonksiyonu kullanildi
    data_input <- reactive({
      req(input[["file"]])
      read.csv(input[["file"]][["datapath"]], stringsAsFactors = FALSE)
    })
    
    output[["col_selector"]] <- renderUI({
      df <- data_input()
      req(df)
      ns <- session[["ns"]]
      selectInput(ns("demand_col"), "Demand column", choices = names(df))
    })
    
    calc_results <- reactive({
      df <- data_input()
      req(df, input[["demand_col"]])
      demand <- as.numeric(df[[input[["demand_col"]]]])
      req(length(demand) > 0)
      
      if (input[["choice_type"]] == "manual") {
        n <- input[["n_manual"]]
        req(n > 0 && n < length(demand))
        
        forecasts <- c()
        actuals <- c()
        for (t in (n + 1):length(demand)) {
          forecasts <- c(forecasts, mean(demand[(t - n):(t - 1)]))
          actuals <- c(actuals, demand[t])
        }
        errors <- actuals - forecasts
        
        mad <- mean(abs(errors))
        mse <- mean(errors^2)
        mape <- mean(abs(errors / actuals)) * 100
        
        error_df <- data.frame(
          Measure = c("MAD", "MSE", "MAPE (%)"),
          Value = round(c(mad, mse, mape), 2),
          "Periods evaluated" = paste0((n + 1), "-", length(demand)),
          check.names = FALSE
        )
        
        forecast_df <- data.frame(
          Period = 1:length(demand),
          "Demand (D)" = demand,
          "Forecast (F)" = c(rep(NA, n), forecasts),
          "Error (e = F - D)" = c(rep(NA, n), round(forecasts - actuals, 2)),
          check.names = FALSE
        )
        
        next_fc <- mean(demand[(length(demand) - n + 1):length(demand)])
        
        list(
          n = n,
          next_fc = next_fc,
          error_df = error_df,
          forecast_df = forecast_df,
          demand = demand,
          forecasts = forecasts
        )
        
      } else {
        max_n <- min(input[["max_n_auto"]], length(demand) - 2)
        req(max_n >= 1)
        metric <- input[["metric"]]
        
        summary_table <- data.frame(N = 1:max_n, MAD = NA, MSE = NA, MAPE = NA)
        
        for (i in 1:max_n) {
          n_val <- i
          fc <- c()
          ac <- c()
          for (t in (n_val + 1):length(demand)) {
            fc <- c(fc, mean(demand[(t - n_val):(t - 1)]))
            ac <- c(ac, demand[t])
          }
          err <- ac - fc
          summary_table[["MAD"]][i] <- mean(abs(err))
          summary_table[["MSE"]][i] <- mean(err^2)
          summary_table[["MAPE"]][i] <- mean(abs(err / ac)) * 100
        }
        
        best_row <- which.min(summary_table[[metric]])
        best_n <- summary_table[["N"]][best_row]
        
        n <- best_n
        forecasts <- c()
        actuals <- c()
        for (t in (n + 1):length(demand)) {
          forecasts <- c(forecasts, mean(demand[(t - n):(t - 1)]))
          actuals <- c(actuals, demand[t])
        }
        errors <- actuals - forecasts
        
        error_df <- data.frame(
          Measure = c("MAD", "MSE", "MAPE (%)"),
          Value = round(c(summary_table[["MAD"]][best_row], summary_table[["MSE"]][best_row], summary_table[["MAPE"]][best_row]), 2),
          "Periods evaluated" = paste0((n + 1), "-", length(demand)),
          check.names = FALSE
        )
        
        forecast_df <- data.frame(
          Period = 1:length(demand),
          "Demand (D)" = demand,
          "Forecast (F)" = c(rep(NA, n), forecasts),
          "Error (e = F - D)" = c(rep(NA, n), round(forecasts - actuals, 2)),
          check.names = FALSE
        )
        
        next_fc <- mean(demand[(length(demand) - n + 1):length(demand)])
        
        list(
          n = n,
          next_fc = next_fc,
          error_df = error_df,
          forecast_df = forecast_df,
          demand = demand,
          forecasts = forecasts
        )
      }
    })
    
    output[["next_period_forecast"]] <- renderText({
      res <- calc_results()
      paste0("MA(", res[["n"]], ") - next-period forecast: ", round(res[["next_fc"]], 2))
    })
    
    output[["ma_plot"]] <- renderPlot({
      res <- calc_results()
      req(res)
      demand <- res[["demand"]]
      n <- res[["n"]]
      forecasts <- res[["forecasts"]]
      
      plot(1:length(demand), demand, type = "o", col = "black", pch = 16, lwd = 1.5,
           xlab = "Period", ylab = "Demand", main = paste0("Actual demand and MA(", n, ") forecast"))
      lines((n + 1):length(demand), forecasts, type = "o", col = "blue", lty = 2, pch = 17, lwd = 1.5)
      points(length(demand) + 1, res[["next_fc"]], col = "red", pch = 17, cex = 1.5)
      legend("topleft", legend = c("Actual demand", "Forecast", "Next-period forecast"),
             col = c("black", "blue", "red"), lty = c(1, 2, NA), pch = c(16, 17, 17), bty = "n")
    })
    
    output[["error_table"]] <- renderTable({
      res <- calc_results()
      res[["error_df"]]
    })
    
    output[["forecast_table"]] <- renderTable({
      res <- calc_results()
      res[["forecast_df"]]
    })
    
  })
}
