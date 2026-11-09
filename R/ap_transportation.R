library(shiny)
library(ggplot2)
library(dplyr)
library(lpSolve)

ap_transportation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             helpText("The Transportation Method structures Aggregate Planning as a matrix where rows are Supply Sources (Inventory, Regular Time, Overtime, Subcontracting) and columns are Demand Periods."),
             tabsetPanel(
               tabPanel("Optimal Solution (Least Cost)",
                        br(),
                        h4("Transportation Matrix (Optimal)"),
                        tableOutput(ns("tbl_opt")),
                        h4("Cost Summary"),
                        verbatimTextOutput(ns("cost_opt")),
                        br(),
                        h4("Optimal Production Plan"),
                        tableOutput(ns("tbl_opt_plan")),
                        plotOutput(ns("plot_opt"))
               ),
               tabPanel("North-West Corner (Initial Solution)",
                        br(),
                        h4("Transportation Matrix (NW Corner)"),
                        tableOutput(ns("tbl_nw")),
                        h4("Cost Summary"),
                        verbatimTextOutput(ns("cost_nw"))
               )
             )
      )
    )
  )
}

ap_transportation_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Transportation Logic
    trans_results <- reactive({
      df <- shared_data$df()
      costs <- shared_data$costs()
      params <- shared_data$params()
      
      req(nrow(df) > 0)
      
      n <- nrow(df)
      D <- df$Demand
      Days <- df$WorkingDays
      K <- params$K
      
      # We assume Level workforce (W0) remains constant for Capacity calculations
      # Capacity sources per period: RT, OT, SUB
      cap_RT <- params$W0 * K * Days
      cap_OT <- cap_RT * 0.25 # Assumption: Max OT is 25% of RT (Common in academia unless specified)
      cap_SUB <- rep(999999, n) # Unlimited subcontracting
      
      # Number of Supply Rows = 1 (Beg Inv) + 3*n (RT, OT, SUB for each period) + 1 (Dummy for excess supply)
      num_rows <- 1 + 3 * n + 1
      # Number of Demand Cols = n + 1 (Dummy for excess supply)
      num_cols <- n + 1
      
      # Setup Supply Vector
      Supply <- numeric(num_rows)
      Supply[1] <- params$I0
      for (t in 1:n) {
        Supply[1 + (t-1)*3 + 1] <- cap_RT[t]
        Supply[1 + (t-1)*3 + 2] <- cap_OT[t]
        Supply[1 + (t-1)*3 + 3] <- cap_SUB[t]
      }
      Supply[num_rows] <- 999999 # Dummy supply to balance
      
      # Setup Demand Vector
      Demand <- numeric(num_cols)
      Demand[1:n] <- D
      Demand[n] <- Demand[n] + params$Iend # Add target ending inventory to last period
      Demand[num_cols] <- max(sum(Supply) - sum(Demand[1:n]), 0) # Balance with dummy demand
      
      # Build Cost Matrix
      CostMat <- matrix(999999, nrow = num_rows, ncol = num_cols)
      row_names <- character(num_rows)
      row_names[1] <- "Initial Inventory"
      row_names[num_rows] <- "Dummy Supply"
      
      # Populate Cost Matrix
      for (i in 1:num_rows) {
        for (j in 1:num_cols) {
          
          # Dummy Demand Column (Idle Capacity)
          if (j == num_cols) {
            CostMat[i, j] <- 0
            next
          }
          
          # Initial Inventory Row
          if (i == 1) {
            CostMat[i, j] <- (j - 1) * costs$Hold
          }
          # Dummy Supply Row
          else if (i == num_rows) {
            CostMat[i, j] <- 0
          }
          # Production Rows (RT, OT, SUB)
          else {
            # Determine which period this row belongs to (Source Period)
            src_p <- ceiling((i - 1) / 3)
            src_type <- (i - 2) %% 3 # 0=RT, 1=OT, 2=SUB
            
            # Cannot produce for past periods (Backordering not allowed in basic model)
            if (src_p > j) {
              CostMat[i, j] <- 999999
            } else {
              base_cost <- if(src_type == 0) costs$RT else if(src_type == 1) costs$OT else costs$Sub
              holding_periods <- j - src_p
              CostMat[i, j] <- base_cost + (holding_periods * costs$Hold)
            }
            
            if (j == 1) { # Only set names once
              name_type <- if(src_type == 0) "RT" else if(src_type == 1) "OT" else "SUB"
              row_names[i] <- paste0("Period ", src_p, " (", name_type, ")")
            }
          }
        }
      }
      
      col_names <- c(paste0("Dem ", 1:n), "Dummy Dem")
      
      # 1. Solve via lpSolve (Optimal)
      # Flatten cost matrix by rows
      obj <- as.vector(t(CostMat))
      
      # Constraints
      con_mat <- matrix(0, nrow = num_rows + num_cols, ncol = num_rows * num_cols)
      con_dir <- rep("=", num_rows + num_cols)
      con_rhs <- c(Supply, Demand)
      
      # Row constraints (Supply)
      for (i in 1:num_rows) {
        start_col <- (i - 1) * num_cols + 1
        con_mat[i, start_col:(start_col + num_cols - 1)] <- 1
      }
      # Column constraints (Demand)
      for (j in 1:num_cols) {
        cols <- seq(j, num_rows * num_cols, by = num_cols)
        con_mat[num_rows + j, cols] <- 1
      }
      
      sol_opt <- lp("min", obj, con_mat, con_dir, con_rhs)
      Opt_Mat <- matrix(sol_opt$solution, nrow = num_rows, byrow = TRUE)
      dimnames(Opt_Mat) <- list(row_names, col_names)
      
      # Clean matrix for display (hide large numbers and dummy cells)
      disp_opt <- as.data.frame(Opt_Mat)
      disp_opt[disp_opt == 0] <- ""
      disp_opt$Source <- row_names
      disp_opt <- disp_opt %>% select(Source, everything(), -`Dummy Dem`) %>% head(-1)
      
      # 2. North-West Corner Heuristic (Initial Solution)
      NW_Mat <- matrix(0, nrow = num_rows, ncol = num_cols)
      s_rem <- Supply
      d_rem <- Demand
      
      i <- 1; j <- 1
      while (i <= num_rows && j <= num_cols) {
        alloc <- min(s_rem[i], d_rem[j])
        NW_Mat[i, j] <- alloc
        s_rem[i] <- s_rem[i] - alloc
        d_rem[j] <- d_rem[j] - alloc
        
        if (s_rem[i] == 0) i <- i + 1
        else j <- j + 1
      }
      
      disp_nw <- as.data.frame(NW_Mat)
      names(disp_nw) <- col_names
      disp_nw[disp_nw == 0] <- ""
      disp_nw$Source <- row_names
      disp_nw <- disp_nw %>% select(Source, everything(), -`Dummy Dem`) %>% head(-1)
      
      # Calculate Costs (ignoring dummy columns and 999999 costs)
      calc_real_cost <- function(alloc_mat) {
        total <- 0
        for (i in 1:(num_rows-1)) {
          for (j in 1:n) {
            if (alloc_mat[i, j] > 0 && CostMat[i, j] < 999999) {
              total <- total + (alloc_mat[i, j] * CostMat[i, j])
            }
          }
        }
        return(total)
      }
      
      cost_opt_val <- calc_real_cost(Opt_Mat)
      cost_nw_val <- calc_real_cost(NW_Mat)
      
      cost_opt_str <- sprintf("Optimal Total Cost: $%s", formatC(cost_opt_val, format="f", big.mark=",", digits=2))
      cost_nw_str <- sprintf("NW Corner Total Cost: $%s", formatC(cost_nw_val, format="f", big.mark=",", digits=2))
      
      # Extract Plan from Optimal Matrix
      plan_df <- data.frame(
        Period = df$Period,
        Demand = D,
        Prod_RT = numeric(n),
        Prod_OT = numeric(n),
        Prod_SUB = numeric(n)
      )
      
      for(t in 1:n) {
        r_start <- 1 + (t-1)*3
        plan_df$Prod_RT[t] <- sum(Opt_Mat[r_start + 1, 1:n])
        plan_df$Prod_OT[t] <- sum(Opt_Mat[r_start + 2, 1:n])
        plan_df$Prod_SUB[t] <- sum(Opt_Mat[r_start + 3, 1:n])
      }
      
      list(
        opt_table = disp_opt, cost_opt = cost_opt_str, plan_df = plan_df,
        nw_table = disp_nw, cost_nw = cost_nw_str
      )
    })
    
    # Renders
    output$tbl_opt <- renderTable({ trans_results()$opt_table }, striped = TRUE, hover = TRUE, align = 'c')
    output$cost_opt <- renderText({ trans_results()$cost_opt })
    
    output$tbl_nw <- renderTable({ trans_results()$nw_table }, striped = TRUE, hover = TRUE, align = 'c')
    output$cost_nw <- renderText({ trans_results()$cost_nw })
    
    output$tbl_opt_plan <- renderTable({ trans_results()$plan_df }, digits = 0)
    
    output$plot_opt <- renderPlot({
      plan <- trans_results()$plan_df
      n <- nrow(plan)
      
      plot_df <- data.frame(
        Period = rep(1:n, 3),
        Value = c(plan$Prod_RT, plan$Prod_OT, plan$Prod_SUB),
        Type = factor(rep(c("Regular Time", "Overtime", "Subcontract"), each = n), 
                      levels = c("Regular Time", "Overtime", "Subcontract"))
      )
      
      ggplot(plot_df, aes(x = factor(Period), y = Value, fill = Type)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "Period", y = "Units Produced", title = "Optimal Production Mix by Source") +
        scale_fill_brewer(palette = "Blues")
    })
    
  })
}