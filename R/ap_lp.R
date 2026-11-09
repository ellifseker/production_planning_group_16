library(shiny)
library(ggplot2)
library(dplyr)
library(lpSolve)

ap_lp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             wellPanel(
               h4("LP Model Options"),
               helpText("The Linear Programming model minimizes total cost while satisfying demand, capacity, and workforce constraints."),
               checkboxInput(ns("allow_ot"), "Allow Overtime (OT)", value = TRUE),
               checkboxInput(ns("allow_sub"), "Allow Subcontracting (SUB)", value = TRUE),
               hr(),
               helpText(strong("Note:")),
               helpText("Idle time is naturally accounted for when regular production is less than total workforce capacity.")
             )
      ),
      column(8,
             uiOutput(ns("lp_status")),
             tabsetPanel(
               tabPanel("Exact (Fractional) LP Plan",
                        br(),
                        tableOutput(ns("tbl_frac")),
                        h4("Cost Summary"),
                        verbatimTextOutput(ns("cost_frac"))),
               
               tabPanel("Integer LP Plan (Whole Workers)",
                        br(),
                        helpText("Solved using Mixed Integer Linear Programming (MILP) forcing workforce to be whole numbers."),
                        tableOutput(ns("tbl_int")),
                        h4("Cost Summary"),
                        verbatimTextOutput(ns("cost_int"))),
               
               tabPanel("Cumulative Plot",
                        br(),
                        plotOutput(ns("plot_cum")))
             )
      )
    )
  )
}

ap_lp_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # LP Solver Function
    solve_lp <- function(force_integer = FALSE) {
      df <- shared_data$df()
      costs <- shared_data$costs()
      params <- shared_data$params()
      
      n <- nrow(df)
      D <- df$Demand
      Days <- df$WorkingDays
      
      # We have 7 variables per period: W (Workforce), H (Hire), F (Fire), P (Production), O (Overtime), S (Subcontract), I (Inventory)
      # Index formula for period t: (t-1)*7 + var_offset
      # Offsets: 1=W, 2=H, 3=F, 4=P, 5=O, 6=S, 7=I
      num_vars <- 7 * n
      
      # 1. Objective Function (Minimize Cost)
      obj <- rep(0, num_vars)
      for (t in 1:n) {
        idx <- (t - 1) * 7
        obj[idx + 2] <- costs$Hire
        obj[idx + 3] <- costs$Fire
        obj[idx + 4] <- costs$RT
        obj[idx + 5] <- costs$OT
        obj[idx + 6] <- costs$Sub
        obj[idx + 7] <- costs$Hold
      }
      
      const_mat <- matrix(0, nrow = 0, ncol = num_vars)
      const_dir <- character()
      const_rhs <- numeric()
      
      add_constraint <- function(row_vec, dir, rhs) {
        const_mat <<- rbind(const_mat, row_vec)
        const_dir <<- c(const_dir, dir)
        const_rhs <<- c(const_rhs, rhs)
      }
      
      for (t in 1:n) {
        idx <- (t - 1) * 7
        
        # C1: Workforce Balance (W_t - H_t + F_t - W_{t-1} = 0)
        row <- rep(0, num_vars)
        row[idx + 1] <- 1   # W_t
        row[idx + 2] <- -1  # H_t
        row[idx + 3] <- 1   # F_t
        if (t == 1) {
          add_constraint(row, "=", params$W0)
        } else {
          row[idx - 7 + 1] <- -1 # W_{t-1}
          add_constraint(row, "=", 0)
        }
        
        # C2: Inventory Balance (I_t - P_t - O_t - S_t - I_{t-1} = -D_t)
        row <- rep(0, num_vars)
        row[idx + 7] <- 1   # I_t
        row[idx + 4] <- -1  # P_t
        row[idx + 5] <- -1  # O_t
        row[idx + 6] <- -1  # S_t
        if (t == 1) {
          add_constraint(row, "=", -D[t] - params$I0)
        } else {
          row[idx - 7 + 7] <- -1 # I_{t-1}
          add_constraint(row, "=", -D[t])
        }
        
        # C3: Capacity Constraint (P_t <= W_t * K * Days_t)  => P_t - (K*Days_t)*W_t <= 0
        row <- rep(0, num_vars)
        row[idx + 4] <- 1
        row[idx + 1] <- -(params$K * Days[t])
        add_constraint(row, "<=", 0)
        
        # C4: Overtime Restriction
        if (!input$allow_ot) {
          row <- rep(0, num_vars)
          row[idx + 5] <- 1
          add_constraint(row, "=", 0)
        }
        
        # C5: Subcontracting Restriction
        if (!input$allow_sub) {
          row <- rep(0, num_vars)
          row[idx + 6] <- 1
          add_constraint(row, "=", 0)
        }
      }
      
      # C6: Ending Inventory Target
      if (params$Iend > 0) {
        row <- rep(0, num_vars)
        row[(n - 1) * 7 + 7] <- 1 # I_n
        add_constraint(row, ">=", params$Iend)
      }
      
      # Integer variables setup (Force W, H, F to be integers if required)
      int_vec <- 0
      if (force_integer) {
        # Indices of W, H, F across all periods
        int_vec <- c(seq(1, num_vars, by=7), seq(2, num_vars, by=7), seq(3, num_vars, by=7))
      }
      
      # Solve!
      sol <- lp("min", obj, const_mat, const_dir, const_rhs, all.int = FALSE, int.vec = int_vec)
      
      if (sol$status == 0) {
        res <- matrix(sol$solution, nrow = n, byrow = TRUE)
        res_df <- data.frame(
          Period = df$Period,
          Demand = D,
          Workers = res[, 1],
          Hired = res[, 2],
          Fired = res[, 3],
          Prod_Reg = res[, 4],
          Prod_OT = res[, 5],
          Subcontract = res[, 6],
          Inventory = res[, 7]
        )
        
        c_h <- sum(res_df$Hired) * costs$Hire
        c_f <- sum(res_df$Fired) * costs$Fire
        c_p <- sum(res_df$Prod_Reg) * costs$RT
        c_o <- sum(res_df$Prod_OT) * costs$OT
        c_s <- sum(res_df$Subcontract) * costs$Sub
        c_i <- sum(res_df$Inventory) * costs$Hold
        
        cost_str <- sprintf("Total Cost: $%s \n(Hire: $%s | Fire: $%s | Reg. Prod: $%s | Overtime: $%s | Subcontract: $%s | Hold: $%s)",
                            formatC(sol$objval, format="f", big.mark=",", digits=2),
                            formatC(c_h, format="f", big.mark=",", digits=2),
                            formatC(c_f, format="f", big.mark=",", digits=2),
                            formatC(c_p, format="f", big.mark=",", digits=2),
                            formatC(c_o, format="f", big.mark=",", digits=2),
                            formatC(c_s, format="f", big.mark=",", digits=2),
                            formatC(c_i, format="f", big.mark=",", digits=2))
        
        return(list(status = "Success", table = res_df, cost = cost_str, objval = sol$objval))
      } else {
        return(list(status = "Infeasible", message = "No feasible solution found with current constraints. Try allowing Overtime or Subcontracting."))
      }
    }
    
    lp_results <- reactive({
      req(nrow(shared_data$df()) > 0)
      list(
        frac = solve_lp(force_integer = FALSE),
        int  = solve_lp(force_integer = TRUE)
      )
    })
    
    # Render Outputs
    output$lp_status <- renderUI({
      res <- lp_results()$frac
      if (res$status == "Infeasible") {
        div(style = "color: red; font-weight: bold; margin-bottom: 15px;", res$message)
      } else {
        NULL
      }
    })
    
    output$tbl_frac <- renderTable({ 
      res <- lp_results()$frac
      if(res$status == "Success") res$table else NULL 
    }, digits = 2)
    
    output$cost_frac <- renderText({ 
      res <- lp_results()$frac
      if(res$status == "Success") res$cost else "N/A" 
    })
    
    output$tbl_int <- renderTable({ 
      res <- lp_results()$int
      if(res$status == "Success") res$table else NULL 
    }, digits = 0) # Show without decimals
    
    output$cost_int <- renderText({ 
      res <- lp_results()$int
      if(res$status == "Success") res$cost else "N/A" 
    })
    
    output$plot_cum <- renderPlot({
      res <- lp_results()$int
      req(res$status == "Success")
      
      tbl <- res$table
      Total_Supply <- tbl$Prod_Reg + tbl$Prod_OT + tbl$Subcontract
      Net_Demand <- tbl$Demand # For LP, cumulative supply covers cumulative demand directly
      
      cum_S <- cumsum(Total_Supply)
      cum_D <- cumsum(Net_Demand)
      n <- length(cum_S)
      
      plot_df <- data.frame(
        Period_Index = rep(1:n, 2),
        Value = c(cum_S, cum_D),
        Type = rep(c("Cumulative Supply (Prod+OT+Sub)", "Cumulative Demand"), each = n)
      )
      
      ggplot(plot_df, aes(x = Period_Index, y = Value, color = Type, group = Type)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(x = "Period", y = "Units", title = "Cumulative Supply vs Cumulative Demand (MILP Plan)") +
        scale_color_manual(values = c("Cumulative Supply (Prod+OT+Sub)" = "blue", "Cumulative Demand" = "red"))
    })
    
  })
}