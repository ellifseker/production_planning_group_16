library(shiny)
library(ggplot2)
library(dplyr)

ap_heuristics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             wellPanel(
               h4("Strategy Settings"),
               selectInput(ns("strategy"), "Select Heuristic Strategy:",
                           choices = c("Zero-Inventory (Chase)" = "chase",
                                       "Constant-Workforce (Level)" = "level",
                                       "Mixed Plan" = "mixed")),
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'mixed'", ns("strategy")),
                 uiOutput(ns("mixed_periods_ui")),
                 helpText("Select the periods where the workforce is allowed to change. In unselected periods, workforce remains constant.")
               )
             )
      ),
      column(8,
             tabsetPanel(
               tabPanel("Exact (Fractional) Plan",
                        br(), 
                        h4("Plan Details"),
                        tableOutput(ns("tbl_frac")),
                        h4("Cost Summary"),
                        verbatimTextOutput(ns("cost_frac"))),
               
               tabPanel("Rounded Up Plan (Whole Workers)",
                        br(),
                        h4("Plan Details"),
                        tableOutput(ns("tbl_round")),
                        h4("Cost Summary"),
                        verbatimTextOutput(ns("cost_round"))),
               
               tabPanel("Cumulative Plot",
                        br(),
                        plotOutput(ns("plot_cum")))
             )
      )
    )
  )
}

ap_heuristics_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # UI for selecting changing periods in Mixed Plan
    output$mixed_periods_ui <- renderUI({
      df <- shared_data$df()
      req(nrow(df) > 0)
      # First period is always a starting point, user selects subsequent changes
      choices <- df$Period[-1] 
      checkboxGroupInput(ns("mixed_periods"), "Periods to Change Workforce:",
                         choices = choices)
    })
    
    # Core Logic
    plan_results <- reactive({
      df <- shared_data$df()
      costs <- shared_data$costs()
      params <- shared_data$params()
      
      req(nrow(df) > 0, params$K > 0)
      
      n <- nrow(df)
      D <- df$Demand
      Days <- df$WorkingDays
      W0 <- params$W0
      I0 <- params$I0
      Iend <- params$Iend
      K <- params$K
      
      # Calculate Net Requirements
      NetD <- numeric(n)
      NetD[1] <- D[1] - I0
      if(n > 1) NetD[2:(n-1)] <- D[2:(n-1)]
      NetD[n] <- D[n] + Iend
      NetD <- pmax(0, NetD) # Avoid negative requirements
      
      # Function to calculate with or without rounding
      calc <- function(round_up) {
        W <- numeric(n); H <- numeric(n); F_workers <- numeric(n)
        P <- numeric(n); I <- numeric(n)
        
        if (input$strategy == "chase") {
          for (t in 1:n) {
            req_w <- NetD[t] / (K * Days[t])
            if (round_up) req_w <- ceiling(req_w)
            W[t] <- req_w
          }
          
        } else if (input$strategy == "level") {
          req_w <- sum(NetD) / sum(K * Days)
          if (round_up) req_w <- ceiling(req_w)
          W[] <- req_w
          
        } else if (input$strategy == "mixed") {
          change_periods <- c(df$Period[1], input$mixed_periods)
          
          # Group periods into constant workforce blocks
          blocks <- rep(NA, n)
          current_block <- 1
          for(t in 1:n) {
            if (df$Period[t] %in% change_periods && t != 1) {
              current_block <- current_block + 1
            }
            blocks[t] <- current_block
          }
          
          # Calculate level workforce for each block
          for (b in unique(blocks)) {
            idx <- which(blocks == b)
            req_w <- sum(NetD[idx]) / sum(K * Days[idx])
            if (round_up) req_w <- ceiling(req_w)
            W[idx] <- req_w
          }
        }
        
        # Simulate the plan
        for (t in 1:n) {
          P[t] <- W[t] * K * Days[t]
          I[t] <- if (t==1) I0 + P[t] - D[t] else I[t-1] + P[t] - D[t]
          
          prev_W <- if (t==1) W0 else W[t-1]
          H[t] <- max(0, W[t] - prev_W)
          F_workers[t] <- max(0, prev_W - W[t])
        }
        
        # Calculate Costs
        Cost_H <- sum(H) * costs$Hire
        Cost_F <- sum(F_workers) * costs$Fire
        Cost_I <- sum(pmax(0, I)) * costs$Hold # Only positive inventory is held
        Cost_RT <- sum(P) * costs$RT
        
        Total <- Cost_H + Cost_F + Cost_I + Cost_RT
        
        res_df <- data.frame(
          Period = df$Period,
          Demand = D,
          NetReq = NetD,
          Workers = W,
          Hired = H,
          Fired = F_workers,
          Production = P,
          Inventory = I
        )
        
        cost_str <- sprintf("Total Cost: $%s \n(Hiring: $%s, Firing: $%s, Holding: $%s, Regular Time: $%s)",
                            formatC(Total, format="f", big.mark=",", digits=2),
                            formatC(Cost_H, format="f", big.mark=",", digits=2),
                            formatC(Cost_F, format="f", big.mark=",", digits=2),
                            formatC(Cost_I, format="f", big.mark=",", digits=2),
                            formatC(Cost_RT, format="f", big.mark=",", digits=2))
        
        list(table = res_df, cost = cost_str, P = P, NetD = NetD)
      }
      
      list(frac = calc(FALSE), round = calc(TRUE))
    })
    
    # Render Outputs
    output$tbl_frac <- renderTable({ plan_results()$frac$table }, digits = 2)
    output$cost_frac <- renderText({ plan_results()$frac$cost })
    
    output$tbl_round <- renderTable({ plan_results()$round$table }, digits = 2)
    output$cost_round <- renderText({ plan_results()$round$cost })
    
    output$plot_cum <- renderPlot({
      res <- plan_results()$round
      cum_P <- cumsum(res$P)
      cum_D <- cumsum(res$NetD)
      n <- length(cum_P)
      
      plot_df <- data.frame(
        Period_Index = rep(1:n, 2),
        Value = c(cum_P, cum_D),
        Type = rep(c("Cumulative Production", "Cumulative Net Demand"), each = n)
      )
      
      ggplot(plot_df, aes(x = Period_Index, y = Value, color = Type, group = Type)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(x = "Period", y = "Units", title = "Cumulative Production vs Net Demand (Whole Workers Plan)") +
        scale_color_manual(values = c("Cumulative Production" = "blue", "Cumulative Net Demand" = "red"))
    })
    
  })
}