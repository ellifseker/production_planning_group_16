library(shiny)
library(readxl)

forecast_trend_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("2. Trend & Seasonal Forecasting"),
      fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".xls", ".csv")),
      
      # KULLANICI İÇİN BİLGİLENDİRME NOTU EKLENDİ
      helpText(strong("Note:"), "The 2nd column in your uploaded file will be automatically detected and used as the Demand column."),
      
      uiOutput(ns("col_ui")),
      hr(),
      
      selectInput(ns("method"), "Select Forecasting Method:",
                  choices = c("Holt's Method (Trend Series)" = "holt",
                              "Seasonal Decomposition with MA" = "decomp",
                              "Winters' Method (Trend + Seasonal)" = "winters")),
      
      # --- Season Length N (Used for Seasonal Check in Holt, and required in Decomp/Winters) ---
      numericInput(ns("freq"), "Season Length (N - e.g., 4=Quarterly, 12=Monthly):", value = 4, min = 2, max = 52, step = 1),
      
      # --- HOLT'S METHOD CONTROLS ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'holt'", ns("method")),
        checkboxInput(ns("holt_ignore_season"), "Proceed even if seasonality is detected", value = FALSE),
        radioButtons(ns("holt_mode"), "Parameter Selection Mode (α, β):",
                     choices = c("Manual Input (0 - 1)" = "manual", "Automatic (Find Best α, β)" = "auto")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("holt_mode")),
          numericInput(ns("h_alpha"), "Alpha (α - Level, 0 to 1):", value = 0.2, min = 0.01, max = 1.0, step = 0.05),
          numericInput(ns("h_beta"), "Beta (β - Trend, 0 to 1):", value = 0.1, min = 0.01, max = 1.0, step = 0.05)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("holt_mode")),
          selectInput(ns("holt_crit"), "Optimization Criterion:", choices = c("MSE", "MAD", "MAPE"))
        ),
        radioButtons(ns("holt_init"), "Initial Values (S0, G0) Method:",
                     choices = c("First Observations (S0 = D0, G0 = D1 - D0)" = "d0",
                                 "Fit Regression Line (Least Squares)" = "reg",
                                 "Enter S0 and G0 Manually" = "manual")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("holt_init")),
          numericInput(ns("holt_s0"), "Initial Level (S0):", value = 200),
          numericInput(ns("holt_g0"), "Initial Slope (G0):", value = 10)
        )
      ),
      
      # --- WINTERS' METHOD CONTROLS ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'winters'", ns("method")),
        radioButtons(ns("w_mode"), "Parameter Selection Mode (α, β, γ):",
                     choices = c("Automatic (Find Best Parameters)" = "auto",
                                 "Manual Input (Restricted to 0.1 - 0.2)" = "manual")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("w_mode")),
          helpText("Note: For stable forecasts, manual weights are restricted between 0.1 and 0.2."),
          sliderInput(ns("w_alpha"), "Alpha (α - Level):", min = 0.10, max = 0.20, value = 0.20, step = 0.01),
          sliderInput(ns("w_beta"), "Beta (β - Trend):", min = 0.10, max = 0.20, value = 0.10, step = 0.01),
          sliderInput(ns("w_gamma"), "Gamma (γ - Seasonal):", min = 0.10, max = 0.20, value = 0.10, step = 0.01)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("w_mode")),
          selectInput(ns("w_crit"), "Optimization Criterion:", choices = c("MSE", "MAD", "MAPE"))
        ),
        radioButtons(ns("w_init"), "Initial Level & Slope (S0, G0) Method:",
                     choices = c("First Observations (S0 = D0, G0 = D1 - D0)" = "d0",
                                 "Fit Regression / Two-Season Baseline" = "reg",
                                 "Enter S0 and G0 Manually" = "manual")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("w_init")),
          numericInput(ns("w_s0"), "Initial Level (S0):", value = 20),
          numericInput(ns("w_g0"), "Initial Slope (G0):", value = 1)
        )
      ),
      
      hr(),
      numericInput(ns("tau"), "Forecast Horizon τ (Periods ahead, positive integer):", value = 4, min = 1, max = 48, step = 1)
    ),
    
    mainPanel(
      uiOutput(ns("alert_box")),
      h4(textOutput(ns("title_text"))),
      plotOutput(ns("plot"), height = "380px"),
      uiOutput(ns("model_info")),
      h4("Error Measures"),
      tableOutput(ns("err_table")),
      h4("Forecasts and Components by Period"),
      div(style = "max-height: 400px; overflow-y: auto;",
          tableOutput(ns("period_table")))
    )
  )
}

forecast_trend_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    raw_data <- reactive({
      if (is.null(input$file)) {
        # Default dataset (8 quarters of trending & seasonal data from Lecture 3)
        return(data.frame(
          Trending_Engine_Data = c(200, 250, 175, 186, 225, 285, 305, 190),
          Seasonal_Quarterly_Data = c(10, 20, 26, 17, 12, 23, 30, 22)
        ))
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
    
    # Helper to detect strong regular seasonality at lag N
    check_seasonality <- function(d, N) {
      n <- length(d)
      if (is.na(N) || N < 2 || n < 2 * N) return(FALSE)
      t_idx <- seq_len(n)
      detrended <- residuals(lm(d ~ t_idx))
      acf_vals <- acf(detrended, lag.max = N, plot = FALSE)$acf
      r_N <- acf_vals[N + 1]
      (!is.na(r_N) && r_N > 0.45)
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
      
      # Positive Integer Check for Horizon (tau)
      tau_raw <- input$tau
      validate(
        need(!is.na(tau_raw) && tau_raw > 0 && (tau_raw == floor(tau_raw)),
             "Error: Forecast step (τ) must be a positive integer (1, 2, 3...). Decimal, zero, or negative values are not allowed.")
      )
      h <- as.integer(tau_raw)
      N <- as.integer(input$freq)
      
      warnings_list <- character(0)
      
      # Horizon Erosion Warning
      if (h > max(6, floor(n / 2))) {
        warnings_list <- c(warnings_list,
                           "Warning: The forecast horizon (τ) is set too long relative to your dataset size. Forecast accuracy erodes and the margin of error increases as you project further into the future.")
      }
      
      f <- rep(NA, n)
      f_next <- rep(NA, h)
      S_vec <- rep(NA, n)
      G_vec <- rep(NA, n)
      C_vec <- rep(NA, n)
      label <- ""
      info <- ""
      
      # ========================================================================
      # 1. HOLT'S METHOD
      # ========================================================================
      if (input$method == "holt") {
        validate(need(n >= 3, "Error: At least 3 observations are required for Holt's Method."))
        
        is_seasonal <- check_seasonality(d, N)
        if (is_seasonal && !isTRUE(input$holt_ignore_season)) {
          validate(need(FALSE,
                        "Error: Regular seasonal cycles (fluctuations) were detected in your dataset. Holt's method is designed only for series with linear growth/decline and cannot model seasonality. Please select Winters' Method, which handles both trend and seasonality simultaneously (or check 'Proceed even if seasonality is detected' on the left panel)."))
        }
        
        # Determine Initial Values S0 and G0
        if (input$holt_init == "d0") {
          S0 <- d[1]
          G0 <- d[2] - d[1]
          init_str <- paste0("First Observations (S0 = D0 = ", round(S0, 2), ", G0 = D1 - D0 = ", round(G0, 2), ")")
        } else if (input$holt_init == "reg") {
          t_idx <- seq_len(n)
          reg_fit <- lm(d ~ t_idx)
          S0 <- as.numeric(coef(reg_fit)[1])
          G0 <- as.numeric(coef(reg_fit)[2])
          init_str <- paste0("Regression Fit (S0 = ", round(S0, 2), ", G0 = ", round(G0, 2), ")")
        } else {
          validate(need(!is.na(input$holt_s0) && !is.na(input$holt_g0),
                        "Error: Please enter valid numeric values for S0 and G0."))
          S0 <- as.numeric(input$holt_s0)
          G0 <- as.numeric(input$holt_g0)
          init_str <- paste0("Manual Input (S0 = ", round(S0, 2), ", G0 = ", round(G0, 2), ")")
        }
        
        run_holt <- function(a, b) {
          ft <- numeric(n); St <- numeric(n); Gt <- numeric(n)
          S_p <- S0; G_p <- G0
          for (t in 1:n) {
            ft[t] <- S_p + G_p
            S_c <- a * d[t] + (1 - a) * (S_p + G_p)
            G_c <- b * (S_c - S_p) + (1 - b) * G_p
            St[t] <- S_c; Gt[t] <- G_c
            S_p <- S_c; G_p <- G_c
          }
          list(f = ft, S = St, G = Gt)
        }
        
        if (input$holt_mode == "manual") {
          alpha <- as.numeric(input$h_alpha)
          beta  <- as.numeric(input$h_beta)
          validate(
            need(!is.na(alpha) && alpha > 0 && alpha <= 1, "Error: Alpha (α) must be between 0 and 1."),
            need(!is.na(beta) && beta > 0 && beta <= 1, "Error: Beta (β) must be between 0 and 1.")
          )
        } else {
          grid <- expand.grid(a = seq(0.05, 0.95, by = 0.05), b = seq(0.05, 0.95, by = 0.05))
          scores <- apply(grid, 1, function(row) {
            out <- run_holt(row[1], row[2])
            calc_metric(d, out$f, input$holt_crit)
          })
          best_idx <- which.min(scores)
          alpha <- grid$a[best_idx]
          beta  <- grid$b[best_idx]
        }
        
        h_out <- run_holt(alpha, beta)
        f <- h_out$f; S_vec <- h_out$S; G_vec <- h_out$G
        f_next <- S_vec[n] + (1:h) * G_vec[n]
        label <- paste0("Holt's Method (α=", round(alpha, 2), ", β=", round(beta, 2), ")")
        info <- paste0("Parameters: α = ", round(alpha, 2), ", β = ", round(beta, 2),
                       " | Initialization: ", init_str)
      }
      
      # ========================================================================
      # 2. SEASONAL DECOMPOSITION WITH MOVING AVERAGES
      # ========================================================================
      else if (input$method == "decomp") {
        validate(
          need(!is.na(N) && N >= 2, "Error: Season length N must be an integer >= 2."),
          need(n >= N + 2, "Error: Dataset length must be greater than Season Length N + 2.")
        )
        
        warnings_list <- c(warnings_list,
                           "Note: Because moving averages are centered (CMA), raw trend values cannot be directly generated at the first and last few endpoints of your dataset; this is inherent to the decomposition method (endpoints are filled using nearest interior averages to compute seasonal ratios).")
        
        cma <- as.numeric(stats::filter(d, rep(1 / N, N), sides = 2))
        if (N %% 2 == 0) {
          cma <- as.numeric(stats::filter(cma, c(0.5, 0.5), sides = 1))
          cma <- c(cma[-1], NA)
          center_note <- paste0("Even N (", N, ") -> 2x", N, " Centered Moving Average applied")
        } else {
          center_note <- paste0("Odd N (", N, ") -> Direct Symmetric Moving Average applied (no centering step needed)")
        }
        
        # Fill endpoints using nearest interior CMA values (Lecture 3 procedure)
        cma_filled <- cma
        valid_idx <- which(!is.na(cma))
        if (length(valid_idx) >= 2) {
          left_fill  <- mean(cma[valid_idx[1:2]])
          right_fill <- mean(cma[tail(valid_idx, 2)])
          cma_filled[1:(valid_idx[1] - 1)] <- left_fill
          cma_filled[(tail(valid_idx, 1) + 1):n] <- right_fill
        } else if (length(valid_idx) == 1) {
          cma_filled[is.na(cma_filled)] <- cma[valid_idx]
        }
        
        ratios <- d / cma_filled
        s_idx <- ((seq_len(n) - 1) %% N) + 1
        raw_c <- as.numeric(tapply(ratios, s_idx, mean, na.rm = TRUE))
        final_c <- raw_c * (N / sum(raw_c))
        
        deseas <- d / final_c[s_idx]
        t_vec <- seq_len(n)
        lm_fit <- lm(deseas ~ t_vec)
        a_int <- as.numeric(coef(lm_fit)[1])
        b_slp <- as.numeric(coef(lm_fit)[2])
        
        S_vec <- cma # Show true un-padded CMA in table so endpoints appear blank as expected
        G_vec <- a_int + b_slp * t_vec # Deseasonalized trend line
        C_vec <- final_c[s_idx]
        f <- G_vec * C_vec
        
        t_next <- n + seq_len(h)
        s_next <- ((t_next - 1) %% N) + 1
        f_next <- (a_int + b_slp * t_next) * final_c[s_next]
        
        label <- paste0("Seasonal Decomposition MA (N = ", N, ")")
        info <- paste0(center_note, " | Deseasonalized Trend Line: D_hat = ",
                       round(a_int, 2), " + ", round(b_slp, 3), "*t",
                       " | Rescaled Factors (c_t, sum=", N, "): ",
                       paste(round(final_c, 4), collapse = ", "))
      }
      
      # ========================================================================
      # 3. WINTERS' METHOD
      # ========================================================================
      else if (input$method == "winters") {
        validate(
          need(!is.na(N) && N >= 2, "Error: Season length N must be an integer >= 2."),
          need(n >= 2 * N, "Error: At least two full seasons (2 x Season Length N) of data are required to initialize Winters' model. Please upload more data.")
        )
        
        # Initial Level S0, Slope G0, and Seasonal Factors c_init
        if (input$w_init == "d0") {
          S0 <- d[1]
          G0 <- d[2] - d[1]
          first_season <- d[1:N]
          c_init <- (first_season / mean(first_season))
          c_init <- c_init * (N / sum(c_init))
          init_str <- paste0("First Observations (S0 = D0 = ", round(S0, 2), ", G0 = D1 - D0 = ", round(G0, 2), ")")
        } else if (input$w_init == "reg") {
          V1 <- mean(d[1:N])
          V2 <- mean(d[(N + 1):(2 * N)])
          G0 <- (V2 - V1) / N
          S0 <- V2 + G0 * ((N - 1) / 2)
          # Compute initial factors across the first 2 seasons (Lecture 3 Step 3)
          c_mat <- matrix(NA, nrow = 2, ncol = N)
          for (j in 1:N) {
            c_mat[1, j] <- d[j] / (V1 - ((N + 1) / 2 - j) * G0)
            c_mat[2, j] <- d[N + j] / (V2 - ((N + 1) / 2 - j) * G0)
          }
          c_raw <- colMeans(c_mat)
          c_init <- c_raw * (N / sum(c_raw))
          # Reset S0 to t=0 before period 1 for full in-sample tracking
          S0 <- V1 - G0 * ((N + 1) / 2)
          init_str <- paste0("Two-Season Regression Baseline (S0 = ", round(S0, 2), ", G0 = ", round(G0, 3), ")")
        } else {
          validate(need(!is.na(input$w_s0) && !is.na(input$w_g0),
                        "Error: Please enter valid numeric values for S0 and G0."))
          S0 <- as.numeric(input$w_s0)
          G0 <- as.numeric(input$w_g0)
          first_season <- d[1:N]
          c_init <- (first_season / mean(first_season)) * (N / sum(first_season / mean(first_season)))
          init_str <- paste0("Manual Input (S0 = ", round(S0, 2), ", G0 = ", round(G0, 2), ")")
        }
        
        run_winters <- function(a, b, g) {
          ft <- numeric(n); St <- numeric(n); Gt <- numeric(n); Ct <- numeric(n)
          S_p <- S0; G_p <- G0
          c_hist <- c_init
          for (t in 1:n) {
            s_pos <- ((t - 1) %% N) + 1
            c_old <- c_hist[s_pos]
            ft[t] <- (S_p + G_p) * c_old
            S_c <- a * (d[t] / c_old) + (1 - a) * (S_p + G_p)
            G_c <- b * (S_c - S_p) + (1 - b) * G_p
            c_c <- g * (d[t] / S_c) + (1 - g) * c_old
            St[t] <- S_c; Gt[t] <- G_c; Ct[t] <- c_c
            c_hist[s_pos] <- c_c
            S_p <- S_c; G_p <- G_c
          }
          list(f = ft, S = St, G = Gt, C = Ct, c_last = c_hist)
        }
        
        if (input$w_mode == "manual") {
          alpha <- as.numeric(input$w_alpha)
          beta  <- as.numeric(input$w_beta)
          gamma <- as.numeric(input$w_gamma)
          validate(
            need(alpha >= 0.1 && alpha <= 0.2 && beta >= 0.1 && beta <= 0.2 && gamma >= 0.1 && gamma <= 0.2,
                 "Error: In manual mode, α, β, and γ must be between 0.10 and 0.20.")
          )
        } else {
          grid <- expand.grid(a = seq(0.05, 0.50, by = 0.05),
                              b = seq(0.05, 0.40, by = 0.05),
                              g = seq(0.05, 0.40, by = 0.05))
          scores <- apply(grid, 1, function(row) {
            out <- run_winters(row[1], row[2], row[3])
            calc_metric(d, out$f, input$w_crit)
          })
          best_idx <- which.min(scores)
          alpha <- grid$a[best_idx]
          beta  <- grid$b[best_idx]
          gamma <- grid$g[best_idx]
        }
        
        w_out <- run_winters(alpha, beta, gamma)
        f <- w_out$f; S_vec <- w_out$S; G_vec <- w_out$G; C_vec <- w_out$C
        for (tau in 1:h) {
          s_pos <- ((n + tau - 1) %% N) + 1
          f_next[tau] <- (S_vec[n] + tau * G_vec[n]) * w_out$c_last[s_pos]
        }
        
        label <- paste0("Winters' Method (α=", round(alpha, 2), ", β=", round(beta, 2), ", γ=", round(gamma, 2), ")")
        info <- paste0("Initialization: ", init_str,
                       " | Initial Seasonal Factors (c_1..N): ", paste(round(c_init, 3), collapse = ", "))
      }
      
      list(d = d, f = f, f_next = f_next, S = S_vec, G = G_vec, C = C_vec,
           label = label, info = info, warnings = warnings_list, h = h, method = input$method)
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
      paste0(res$label, " — Next-period forecast: ", round(res$f_next[1], 2))
    })
    
    output$model_info <- renderUI({
      res <- res_data()
      wellPanel(strong("Model Summary: "), res$info)
    })
    
    output$plot <- renderPlot({
      res <- res_data()
      n <- length(res$d); h <- res$h
      all_y <- c(res$d, res$f, res$f_next)
      plot(1:n, res$d, type = "o", pch = 16, col = "black", lwd = 1.8,
           xlim = c(1, n + h), ylim = range(all_y, na.rm = TRUE),
           xlab = "Period", ylab = "Demand",
           main = paste0("Actual Demand and ", res$label, " Forecast"))
      grid()
      lines(1:n, res$f, type = "o", pch = 16, col = "steelblue", lty = 2, lwd = 1.8)
      lines((n + 1):(n + h), res$f_next, type = "o", pch = 17, col = "red3", lty = 2, lwd = 2, cex = 1.3)
      legend("topleft", legend = c("Actual demand", "Forecast", "Next-period forecast"),
             col = c("black", "steelblue", "red3"), pch = c(16, 16, 17), lty = c(1, 2, 2), bty = "n")
    })
    
    output$err_table <- renderTable({
      res <- res_data()
      idx <- which(!is.na(res$d) & !is.na(res$f))
      e <- res$f[idx] - res$d[idx]
      per <- paste0(min(idx), "–", max(idx))
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = round(c(mean(abs(e)), mean(e^2), mean(abs(e / res$d[idx])) * 100), 2),
        `Periods evaluated` = rep(per, 3),
        check.names = FALSE
      )
    })
    
    output$period_table <- renderTable({
      res <- res_data()
      n <- length(res$d); h <- res$h
      d_all <- c(res$d, rep(NA, h))
      f_all <- c(res$f, res$f_next)
      e_all <- f_all - d_all
      S_all <- c(res$S, rep(NA, h))
      G_all <- c(res$G, rep(NA, h))
      C_all <- c(res$C, rep(NA, h))
      
      if (res$method == "holt") {
        data.frame(
          Period = 1:(n + h),
          `Demand (D)` = round(d_all, 2),
          `Level (S_t)` = round(S_all, 2),
          `Slope (G_t)` = round(G_all, 2),
          `Forecast (F)` = round(f_all, 2),
          `Error (e = F - D)` = round(e_all, 2),
          check.names = FALSE
        )
      } else if (res$method == "decomp") {
        data.frame(
          Period = 1:(n + h),
          `Demand (D)` = round(d_all, 2),
          `Centered MA (CMA)` = round(S_all, 2),
          `Trend Line` = round(G_all, 2),
          `Seasonal Factor (c_t)` = round(C_all, 4),
          `Forecast (F)` = round(f_all, 2),
          `Error (e = F - D)` = round(e_all, 2),
          check.names = FALSE
        )
      } else {
        data.frame(
          Period = 1:(n + h),
          `Demand (D)` = round(d_all, 2),
          `Level (S_t)` = round(S_all, 2),
          `Trend (G_t)` = round(G_all, 2),
          `Seasonal (c_t)` = round(C_all, 3),
          `Forecast (F)` = round(f_all, 2),
          `Error (e = F - D)` = round(e_all, 2),
          check.names = FALSE
        )
      }
    }, na = "")
  })
}