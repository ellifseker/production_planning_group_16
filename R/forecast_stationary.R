library(shiny)
library(readxl)

forecast_stationary_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("1. Stationary Series Forecasting"),
      fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".xls", ".csv")),
      
      # KULLANICI İÇİN BİLGİLENDİRME NOTU EKLENDİ
      helpText(strong("Note:"), "The 2nd column in your uploaded file will be automatically detected and used as the Demand column."),
      
      uiOutput(ns("col_ui")),
      hr(),
      
      selectInput(ns("method"), "Select Forecasting Method:",
                  choices = c("Moving Average (MA)" = "ma",
                              "Weighted Moving Average (WMA)" = "wma",
                              "Simple Exponential Smoothing (SES)" = "ses",
                              "Seasonal Factors (Quick & Dirty)" = "qd")),
      
      # --- Moving Average (MA) Controls ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'ma'", ns("method")),
        radioButtons(ns("ma_mode"), "Parameter Selection Mode:",
                     choices = c("Manual Input" = "manual", "Find Best N (Automatic)" = "auto")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("ma_mode")),
          numericInput(ns("ma_n"), "N (Number of past periods):", value = 3, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("ma_mode")),
          selectInput(ns("ma_crit"), "Optimization Criterion:", choices = c("MSE", "MAD", "MAPE"))
        )
      ),
      
      # --- Weighted Moving Average (WMA) Controls ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'wma'", ns("method")),
        radioButtons(ns("wma_mode"), "Parameter Selection Mode:",
                     choices = c("Manual Weights" = "manual", "Automatic (Find Best n)" = "auto")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("wma_mode")),
          textInput(ns("wma_wts"), "Weights w_i (Most recent to oldest, comma-separated):", value = "3, 2, 1"),
          helpText("Note: Sum of weights cannot be 0. Weights are normalized automatically.")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("wma_mode")),
          numericInput(ns("wma_max_n"), "Max window size (n) to search:", value = 5, min = 2, max = 12, step = 1),
          selectInput(ns("wma_crit"), "Optimization Criterion:", choices = c("MSE", "MAD", "MAPE"))
        )
      ),
      
      # --- Simple Exponential Smoothing (SES) Controls ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'ses'", ns("method")),
        radioButtons(ns("ses_mode"), "Parameter Selection Mode:",
                     choices = c("Manual Alpha (α)" = "manual", "Find Best Alpha (Automatic)" = "auto")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("ses_mode")),
          numericInput(ns("ses_alpha"), "Smoothing Constant Alpha (0 < α <= 1):", value = 0.2, min = 0.01, max = 1.0, step = 0.05)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("ses_mode")),
          selectInput(ns("ses_crit"), "Optimization Criterion:", choices = c("MSE", "MAD", "MAPE"))
        ),
        radioButtons(ns("ses_init_mode"), "Initial Value (F1) Method:",
                     choices = c("Automatic (F1 = D1)" = "d1",
                                 "Fit Regression for Initial Level" = "reg",
                                 "Enter Initial Level Manually" = "manual")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("ses_init_mode")),
          numericInput(ns("ses_f1"), "Initial Forecast (F1):", value = 200)
        )
      ),
      
      # --- Quick & Dirty (Seasonal Factors) Controls ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'qd'", ns("method")),
        numericInput(ns("qd_freq"), "Season Length (N):", value = 4, min = 2, step = 1)
      ),
      
      hr(),
      numericInput(ns("h"), "Forecast Horizon (Periods ahead):", value = 1, min = 1, max = 24, step = 1)
    ),
    
    mainPanel(
      uiOutput(ns("alert_box")),
      h4(textOutput(ns("title_text"))),
      plotOutput(ns("plot"), height = "380px"),
      uiOutput(ns("model_info")),
      h4("Error Measures"),
      tableOutput(ns("err_table")),
      h4("Forecasts by Period"),
      div(style = "max-height: 400px; overflow-y: auto;",
          tableOutput(ns("period_table")))
    )
  )
}

forecast_stationary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    raw_data <- reactive({
      if (is.null(input$file)) {
        return(data.frame(Demand = c(200, 250, 175, 186, 225, 285, 305, 190)))
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
    
    # Helper to check significant trend via simple linear regression
    check_trend <- function(d) {
      if (length(d) < 4) return(FALSE)
      t_idx <- seq_along(d)
      fit <- lm(d ~ t_idx)
      p_val <- summary(fit)$coefficients[2, 4]
      r_sq  <- summary(fit)$r.squared
      (!is.na(p_val) && p_val < 0.05 && r_sq > 0.35)
    }
    
    calc_metric <- function(d, f, crit = "MSE") {
      idx <- which(!is.na(d) & !is.na(f))
      if (length(idx) == 0) return(Inf)
      e <- f[idx] - d[idx]
      if (crit == "MAD") mean(abs(e))
      else if (crit == "MSE") mean(e^2)
      else mean(abs(e / d[idx])) * 100
    }
    
    res_data <- reactive({
      req(input$col)
      d <- as.numeric(na.omit(raw_data()[[input$col]]))
      n <- length(d)
      validate(need(n >= 3, "Error: Dataset must contain at least 3 numeric observations."))
      
      h <- max(1, as.integer(input$h))
      f <- rep(NA, n)
      f_next <- rep(NA, h)
      warnings_list <- character(0)
      label <- ""
      info <- ""
      
      has_trend <- check_trend(d)
      
      # 1. MOVING AVERAGE (MA)
      if (input$method == "ma") {
        if (has_trend) {
          warnings_list <- c(warnings_list,
                             "Warning: A significant trend was detected in your data. The Moving Average model lags behind trending series. For more accurate results, please consider Holt's Method or ARIMA models.")
        }
        
        if (input$ma_mode == "manual") {
          k <- as.integer(input$ma_n)
          validate(
            need(!is.na(k) && k >= 1, "Error: N must be a positive integer (N >= 1)."),
            need(k < n, "Error: N cannot be greater than or equal to the total number of observations. Please enter a smaller period N.")
          )
        } else {
          candidates <- 1:(n - 1)
          scores <- sapply(candidates, function(k_try) {
            ft <- rep(NA, n)
            for (t in (k_try + 1):n) ft[t] <- mean(d[(t - k_try):(t - 1)])
            calc_metric(d, ft, input$ma_crit)
          })
          k <- candidates[which.min(scores)]
        }
        
        for (t in (k + 1):n) f[t] <- mean(d[(t - k):(t - 1)])
        f_next <- rep(mean(tail(d, k)), h)
        label <- paste0("MA(", k, ")")
        info <- paste0("Selected N = ", k, " | Average Data Age = (N+1)/2 = ", (k + 1) / 2,
                       " | Equivalent ES Alpha = 2/(N+1) = ", round(2 / (k + 1), 4))
      }
      
      # 2. WEIGHTED MOVING AVERAGE (WMA)
      else if (input$method == "wma") {
        if (has_trend) {
          warnings_list <- c(warnings_list,
                             "Warning: A significant trend was detected in your data. Moving average methods lag behind trends. Consider Holt's Method or ARIMA.")
        }
        
        if (input$wma_mode == "manual") {
          w_raw <- suppressWarnings(as.numeric(unlist(strsplit(input$wma_wts, "[,;\\s]+"))))
          w_raw <- w_raw[!is.na(w_raw)]
          validate(
            need(length(w_raw) >= 1, "Error: Please enter valid numeric weights separated by commas."),
            need(length(w_raw) < n, "Error: Number of weights (n) cannot exceed or equal the total number of observations."),
            need(sum(w_raw) != 0, "Error: The sum of weights (sum of w_i) cannot be 0! Please enter valid weights.")
          )
          k <- length(w_raw)
          w <- w_raw / sum(w_raw)
        } else {
          max_k <- min(as.integer(input$wma_max_n), n - 1)
          validate(need(max_k >= 2, "Error: Dataset is too small for automatic WMA search."))
          candidates <- 2:max_k
          scores <- sapply(candidates, function(k_try) {
            w_try <- (k_try:1) / sum(k_try:1)
            ft <- rep(NA, n)
            for (t in (k_try + 1):n) ft[t] <- sum(w_try * d[(t - 1):(t - k_try)])
            calc_metric(d, ft, input$wma_crit)
          })
          k <- candidates[which.min(scores)]
          w <- (k:1) / sum(k:1)
        }
        
        for (t in (k + 1):n) f[t] <- sum(w * d[(t - 1):(t - k)])
        f_next <- rep(sum(w * d[n:(n - k + 1)]), h)
        label <- paste0("WMA(", k, ")")
        avg_age <- sum((1:k) * w)
        info <- paste0("Window size n = ", k, " | Normalized Weights (Recent to Oldest): ",
                       paste(round(w, 3), collapse = ", "),
                       " | Average Data Age = ", round(avg_age, 3))
      }
      
      # 3. SIMPLE EXPONENTIAL SMOOTHING (SES)
      else if (input$method == "ses") {
        if (has_trend) {
          warnings_list <- c(warnings_list,
                             "Warning: A significant trend was detected in your data. Simple Exponential Smoothing lags behind a trend. Please consider Holt's Method.")
        }
        
        # Determine Initial Forecast F1
        if (input$ses_init_mode == "d1") {
          f1_val <- d[1]
          init_note <- paste0("Automatic (F1 = D1 = ", round(f1_val, 2), ")")
        } else if (input$ses_init_mode == "reg") {
          t_idx <- seq_along(d)
          reg_fit <- lm(d ~ t_idx)
          f1_val <- as.numeric(predict(reg_fit, newdata = data.frame(t_idx = 1)))
          init_note <- paste0("Regression Fitted Initial Level (F1 = ", round(f1_val, 2), ")")
        } else {
          validate(need(!is.na(input$ses_f1), "Error: Please enter a valid numeric initial forecast F1."))
          f1_val <- as.numeric(input$ses_f1)
          init_note <- paste0("Manual Input (F1 = ", round(f1_val, 2), ")")
        }
        
        if (input$ses_mode == "manual") {
          alpha <- as.numeric(input$ses_alpha)
          validate(need(!is.na(alpha) && alpha > 0 && alpha <= 1,
                        "Error: Smoothing constant Alpha (α) must be in the range 0 < α <= 1."))
        } else {
          grid_a <- seq(0.01, 0.99, by = 0.01)
          scores <- sapply(grid_a, function(a_try) {
            ft <- numeric(n)
            ft[1] <- f1_val
            for (t in 2:n) ft[t] <- a_try * d[t - 1] + (1 - a_try) * ft[t - 1]
            calc_metric(d[-1], ft[-1], input$ses_crit)
          })
          alpha <- grid_a[which.min(scores)]
        }
        
        f[1] <- f1_val
        for (t in 2:n) f[t] <- alpha * d[t - 1] + (1 - alpha) * f[t - 1]
        f_next <- rep(alpha * d[n] + (1 - alpha) * f[n], h)
        label <- paste0("SES (α = ", round(alpha, 3), ")")
        info <- paste0("Alpha (α) = ", round(alpha, 3), " | Initialization: ", init_note,
                       " | Average Data Age (1/α) = ", round(1 / alpha, 2))
      }
      
      # 4. SEASONAL FACTORS (QUICK & DIRTY METHOD)
      else if (input$method == "qd") {
        N <- as.integer(input$qd_freq)
        validate(
          need(!is.na(N) && N >= 2, "Error: Season length N must be an integer >= 2."),
          need(n >= 2 * N, "Error: To accurately calculate seasonal factors, your dataset must contain at least several full seasonal cycles (at least 2 x Season Length N observations).")
        )
        
        if (has_trend) {
          warnings_list <- c(warnings_list,
                             "Warning: A significant trend was detected alongside seasonality in your data. Instead of this stationary seasonal method, please select Winters' Method or Seasonal Decomposition.")
        }
        
        d_bar <- mean(d)
        ratios <- d / d_bar
        s_idx <- ((seq_len(n) - 1) %% N) + 1
        c_raw <- as.numeric(tapply(ratios, s_idx, mean))
        c_factors <- c_raw * (N / sum(c_raw)) # Normalized so sum(c_i) = N
        
        f <- d_bar * c_factors[s_idx]
        next_idx <- ((n + seq_len(h) - 1) %% N) + 1
        f_next <- d_bar * c_factors[next_idx]
        
        label <- paste0("Quick & Dirty Seasonal Factors (N = ", N, ")")
        info <- paste0("Sample Mean (D_bar) = ", round(d_bar, 2),
                       " | Seasonal Factors (c_t, sum = ", N, "): ",
                       paste(round(c_factors, 3), collapse = ", "))
      }
      
      list(d = d, f = f, f_next = f_next, label = label, info = info,
           warnings = warnings_list, h = h)
    })
    
    output$alert_box <- renderUI({
      res <- res_data()
      if (length(res$warnings) == 0) return(NULL)
      div(
        style = "background-color: #fff3cd; color: #856404; border: 1px solid #ffeeba; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
        lapply(res$warnings, function(w) p(strong(w), style = "margin: 0 0 5px 0;"))
      )
    })
    
    output$title_text <- renderText({
      res <- res_data()
      paste("Forecast Results:", res$label)
    })
    
    output$plot <- renderPlot({
      res <- res_data()
      d <- res$d; f <- res$f; f_next <- res$f_next
      n <- length(d)
      h <- length(f_next)
      
      t_all <- 1:(n + h)
      y_max <- max(c(d, f, f_next), na.rm = TRUE)
      y_min <- min(c(d, f, f_next), na.rm = TRUE)
      
      plot(1:n, d, type = "o", pch = 16, col = "black", lwd = 2,
           xlim = c(1, n + h), ylim = c(y_min, y_max),
           xlab = "Period", ylab = "Demand", main = "Actual vs. Forecast")
      
      lines(1:n, f, col = "blue", lwd = 2, lty = 2)
      lines((n + 1):(n + h), f_next, col = "red", lwd = 2, type = "o", pch = 17)
      
      legend("topleft", legend = c("Actual Demand", "Fitted Forecast", paste(h, "Period(s) Ahead Forecast")),
             col = c("black", "blue", "red"), lwd = 2, lty = c(1, 2, 1), pch = c(16, NA, 17))
    })
    
    output$model_info <- renderUI({
      res <- res_data()
      div(style = "background-color: #e9ecef; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
          strong("Model Details: "), res$info)
    })
    
    output$err_table <- renderTable({
      res <- res_data()
      d <- res$d; f <- res$f
      idx <- which(!is.na(d) & !is.na(f))
      if (length(idx) == 0) return(NULL)
      e <- d[idx] - f[idx]
      mad <- mean(abs(e))
      mse <- mean(e^2)
      mape <- mean(abs(e / d[idx])) * 100
      data.frame(Metric = c("MAD", "MSE", "MAPE (%)"),
                 Value = c(round(mad, 2), round(mse, 2), round(mape, 2)))
    }, width = "100%")
    
    output$period_table <- renderTable({
      res <- res_data()
      d <- res$d; f <- res$f; f_next <- res$f_next
      n <- length(d); h <- length(f_next)
      df <- data.frame(
        Period = 1:(n + h),
        Actual = c(d, rep(NA, h)),
        Forecast = c(f, f_next)
      )
      df$Error <- df$Actual - df$Forecast
      df$`Abs Error` <- abs(df$Error)
      df$`Pct Error (%)` <- (df$`Abs Error` / df$Actual) * 100
      df
    }, digits = 2, width = "100%", striped = TRUE, hover = TRUE, na = "-")
    
  })
}