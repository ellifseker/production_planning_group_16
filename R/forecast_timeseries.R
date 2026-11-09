library(shiny)
library(forecast)
library(tseries)
library(readxl)

forecast_timeseries_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("3. Time Series / Box-Jenkins"),
      fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".xls", ".csv")),
      
      # KULLANICI İÇİN BİLGİLENDİRME NOTU EKLENDİ
      helpText(strong("Note:"), "The 2nd column in your uploaded file will be automatically detected and used as the Demand column."),
      
      
      helpText("Note: If no file is uploaded, the built-in AirPassengers monthly dataset is used as default."),
      hr(),
      
      selectInput(ns("method"), "Select Time Series Model:",
                  choices = c("Autoregressive (AR)" = "ar",
                              "Moving Average (MA)" = "ma",
                              "Autoregressive Moving Average (ARMA)" = "arma",
                              "Autoregressive Integrated Moving Average (ARIMA)" = "arima")),
      
      # --- AR / MA / ARMA / ARIMA Parameter Controls ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'ar'", ns("method")),
        radioButtons(ns("ar_mode"), "Parameter Estimation Mode:",
                     choices = c("Automatic (Based on PACF / MLE)" = "auto", "Manual Input" = "manual")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("ar_mode")),
          numericInput(ns("ar_p"), "AR Order (p, max 2):", value = 1, min = 1, max = 2, step = 1),
          numericInput(ns("ar_coef"), "AR Coefficient a_1 (Must satisfy |a_1| < 1):", value = 0.5, min = -0.99, max = 0.99, step = 0.05)
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'ma'", ns("method")),
        radioButtons(ns("ma_mode"), "Parameter Estimation Mode:",
                     choices = c("Automatic (Based on ACF / MLE)" = "auto", "Manual Input" = "manual")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("ma_mode")),
          numericInput(ns("ma_q"), "MA Order (q, max 2):", value = 1, min = 1, max = 2, step = 1),
          numericInput(ns("ma_coef"), "MA Coefficient b_1:", value = 0.5, min = -0.99, max = 0.99, step = 0.05)
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'arma'", ns("method")),
        radioButtons(ns("arma_mode"), "Parameter Estimation Mode:",
                     choices = c("Automatic (MLE)" = "auto", "Manual Input" = "manual")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("arma_mode")),
          numericInput(ns("arma_p"), "AR Order (p, max 2):", value = 1, min = 1, max = 2, step = 1),
          numericInput(ns("arma_q"), "MA Order (q, max 2):", value = 1, min = 1, max = 2, step = 1)
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'arima'", ns("method")),
        helpText("Auto ARIMA automatically determines optimal p, d, q parameters minimizing AIC/AICc/BIC.")
      ),
      
      hr(),
      numericInput(ns("freq"), "Frequency / Season Length (N):", value = 12, min = 1, max = 52, step = 1),
      numericInput(ns("h"), "Forecast Horizon (Periods ahead):", value = 12, min = 1, max = 36, step = 1)
    ),
    
    mainPanel(
      uiOutput(ns("alert_box")),
      tabsetPanel(
        tabPanel("Forecasts & Error Measures",
                 h4(textOutput(ns("title_text"))),
                 plotOutput(ns("forecast_plot"), height = "360px"),
                 h4("Model Summary, Parameters & Information Criteria (AIC/BIC)"),
                 verbatimTextOutput(ns("model_summary")),
                 h4("Error Measures"),
                 tableOutput(ns("err_table")),
                 h4("Forecasts by Period"),
                 div(style = "max-height: 350px; overflow-y: auto;",
                     tableOutput(ns("period_table")))
        ),
        tabPanel("ACF / PACF & Stationarity Tests",
                 h4("Autocorrelation (ACF) and Partial Autocorrelation (PACF)"),
                 plotOutput(ns("acf_plot"), height = "450px"),
                 h4("Stationarity & White Noise Tests (ADF, KPSS, Box-Pierce Q)"),
                 verbatimTextOutput(ns("stat_tests"))
        )
      )
    )
  )
}

forecast_timeseries_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    raw_data <- reactive({
      if (is.null(input$file)) {
        return(data.frame(Passengers = as.numeric(AirPassengers)))
      }
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext %in% c("xlsx", "xls")) {
        as.data.frame(readxl::read_excel(input$file$datapath))
      } else {
        read.csv(input$file$datapath, stringsAsFactors = FALSE)
      }
    })
    
    # KOD BURADA GÜNCELLENDİ: 2. SÜTUN OTOMATİK SEÇİLİYOR
    output$col_ui <- renderUI({
      df <- raw_data()
      num_cols <- names(df)[sapply(df, is.numeric)]
      
      # 2. sütunu bul, yoksa ilk numerik sütunu kullan
      default_col <- if (ncol(df) >= 2 && names(df)[2] %in% num_cols) names(df)[2] else num_cols[1]
      
      selectInput(ns("col"), "Select Series (Demand Column):", 
                  choices = num_cols, 
                  selected = default_col)
    })
    
    ts_obj <- reactive({
      df <- raw_data()
      num_cols <- names(df)[sapply(df, is.numeric)]
      hedef_sutun <- if (ncol(df) >= 2 && names(df)[2] %in% num_cols) names(df)[2] else num_cols[1]
      v <- as.numeric(na.omit(df[[hedef_sutun]]))
      ts(v, frequency = max(1, as.numeric(input$freq)))
    })
    
    model_res <- reactive({
      y <- ts_obj()
      n <- length(y)
      h <- max(1, as.integer(input$h))
      warnings_list <- character(0)
      
      # 72 observations rule for Box-Jenkins
      if (n < 72) {
        warnings_list <- c(warnings_list,
                           paste0("Information Note: Your dataset has ", n, " observations (fewer than 72). Box-Jenkins methodology suggests at least 72 observations for robust identification and parameter estimation."))
      }
      
      # Stationarity check via ADF test
      adf_p <- tryCatch({
        res_adf <- suppressWarnings(adf.test(y))
        res_adf$p.value
      }, error = function(e) 0.5)
      
      if (!is.na(adf_p) && adf_p >= 0.05 && input$method != "arima") {
        warnings_list <- c(warnings_list,
                           "Notice: Stationarity test (ADF p >= 0.05) indicates the series is non-stationary. Differencing or selecting the ARIMA model is recommended for non-stationary series.")
      }
      
      fit <- NULL
      fc  <- NULL
      
      # 1. AUTOREGRESSIVE (AR)
      if (input$method == "ar") {
        if (input$ar_mode == "manual") {
          p_ord <- as.integer(input$ar_p)
          a1 <- as.numeric(input$ar_coef)
          validate(
            need(p_ord >= 1 && p_ord <= 2, "Error: AR order p must be 1 or 2."),
            need(!is.na(a1) && abs(a1) < 1, "Error: Stationarity condition violated! The AR coefficient must satisfy |a_1| < 1. Values >= 1 produce explosive results.")
          )
          fit <- Arima(y, order = c(p_ord, 0, 0), fixed = c(a1, rep(0, p_ord - 1), mean(y)))
        } else {
          # Automatic AR based on PACF significant lags (max order 2)
          pacf_vals <- pacf(y, plot = FALSE)$acf
          sig_limit <- 2 / sqrt(n)
          p_auto <- sum(abs(pacf_vals) > sig_limit)
          p_auto <- max(1, min(2, p_auto))
          fit <- Arima(y, order = c(p_auto, 0, 0))
        }
        fc <- forecast(fit, h = h)
      }
      
      # 2. MOVING AVERAGE (MA)
      else if (input$method == "ma") {
        if (input$ma_mode == "manual") {
          q_ord <- as.integer(input$ma_q)
          b1 <- as.numeric(input$ma_coef)
          validate(
            need(q_ord >= 1 && q_ord <= 2, "Error: MA order q must be 1 or 2."),
            need(!is.na(b1) && abs(b1) < 1, "Error: Invertibility condition violated! MA coefficient must satisfy |b_1| < 1.")
          )
          fit <- Arima(y, order = c(0, 0, q_ord), fixed = c(b1, rep(0, q_ord - 1), mean(y)))
        } else {
          acf_vals <- acf(y, plot = FALSE)$acf[-1] # exclude lag 0
          sig_limit <- 2 / sqrt(n)
          q_auto <- sum(abs(acf_vals) > sig_limit)
          q_auto <- max(1, min(2, q_auto))
          fit <- Arima(y, order = c(0, 0, q_auto))
        }
        fc <- forecast(fit, h = h)
      }
      
      # 3. ARMA (p, q)
      else if (input$method == "arma") {
        if (input$arma_mode == "manual") {
          p_ord <- as.integer(input$arma_p)
          q_ord <- as.integer(input$arma_q)
          validate(
            need(p_ord >= 1 && p_ord <= 2 && q_ord >= 1 && q_ord <= 2, "Error: ARMA orders p and q must be between 1 and 2.")
          )
          fit <- Arima(y, order = c(p_ord, 0, q_ord))
        } else {
          fit <- Arima(y, order = c(1, 0, 1))
        }
        fc <- forecast(fit, h = h)
      }
      
      # 4. ARIMA (p, d, q)
      else if (input$method == "arima") {
        fit <- auto.arima(y, stepwise = FALSE, approximation = FALSE)
        fc <- forecast(fit, h = h)
        if (arimaorder(fit)[2] > 0) {
          warnings_list <- c(warnings_list,
                             paste0("Notice: Because trend or seasonality was detected, the system automatically differenced the series (d = ", arimaorder(fit)[2], ") and transitioned from standard stationary models to the ARIMA model."))
        }
      }
      
      list(y = y, fit = fit, fc = fc, h = h, warnings = warnings_list)
    })
    
    output$alert_box <- renderUI({
      res <- model_res()
      if (length(res$warnings) == 0) return(NULL)
      div(
        style = "background-color: #fff3cd; color: #856404; border: 1px solid #ffeeba; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
        lapply(res$warnings, function(w) p(strong(w), style = "margin: 0 0 5px 0;"))
      )
    })
    
    output$title_text <- renderText({
      res <- model_res()
      paste0(res$fc$method, " — Next-period forecast: ", round(as.numeric(res$fc$mean)[1], 2))
    })
    
    output$forecast_plot <- renderPlot({
      res <- model_res()
      plot(res$fc, main = paste0("Actual Demand and ", res$fc$method, " Forecast"),
           xlab = "Time / Period", ylab = "Demand", col = "black", lwd = 1.6)
      grid()
      lines(fitted(res$fit), col = "steelblue", lty = 2, lwd = 1.6)
      legend("topleft", legend = c("Actual demand", "Fitted (In-sample)", "Forecast"),
             col = c("black", "steelblue", "blue"), lty = c(1, 2, 1), bty = "n")
    })
    
    output$model_summary <- renderPrint({
      res <- model_res()
      print(summary(res$fit))
    })
    
    output$err_table <- renderTable({
      res <- model_res()
      d <- as.numeric(res$y)
      f <- as.numeric(fitted(res$fit))
      e <- f - d
      data.frame(
        Measure = c("MAD", "MSE", "RMSE", "MAPE (%)", "AIC", "AICc", "BIC"),
        Value = round(c(mean(abs(e)), mean(e^2), sqrt(mean(e^2)),
                        mean(abs(e / d)) * 100, res$fit$aic, res$fit$aicc, res$fit$bic), 2)
      )
    })
    
    output$period_table <- renderTable({
      res <- model_res()
      d <- as.numeric(res$y)
      f_in <- as.numeric(fitted(res$fit))
      f_out <- as.numeric(res$fc$mean)
      n <- length(d); h <- res$h
      d_all <- c(d, rep(NA, h))
      f_all <- c(f_in, f_out)
      e_all <- f_all - d_all
      data.frame(
        Period = 1:(n + h),
        `Demand (D)` = round(d_all, 2),
        `Forecast (F)` = round(f_all, 2),
        `Error (e = F - D)` = round(e_all, 2),
        check.names = FALSE
      )
    }, na = "")
    
    output$acf_plot <- renderPlot({
      y <- ts_obj()
      freq <- frequency(y)
      y_diff <- diff(y)
      y_sdiff <- if (freq > 1 && length(y_diff) > freq) diff(y_diff, lag = freq) else y_diff
      par(mfrow = c(2, 2))
      Acf(y, main = "ACF - Original Series")
      Pacf(y, main = "PACF - Original Series")
      Acf(y_sdiff, main = "ACF - Differenced Series")
      Pacf(y_sdiff, main = "PACF - Differenced Series")
      par(mfrow = c(1, 1))
    })
    
    output$stat_tests <- renderPrint({
      y <- ts_obj()
      cat("=== Box-Pierce Q Test (White Noise Test) ===\n")
      print(Box.test(y, lag = min(10, length(y) - 1), type = "Box-Pierce"))
      cat("\n=== Augmented Dickey-Fuller (ADF) Test (p < 0.05 -> Stationary) ===\n")
      tryCatch(print(adf.test(y)), error = function(e) cat("Insufficient observations for ADF test.\n"))
      cat("\n=== KPSS Test (p > 0.05 -> Stationary) ===\n")
      tryCatch(print(kpss.test(y)), error = function(e) cat("Insufficient observations for KPSS test.\n"))
    })
  })
}