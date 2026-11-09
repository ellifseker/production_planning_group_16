library(shiny)
library(ggplot2)
library(dplyr)

ap_disaggregation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             wellPanel(
               h4("Product Families"),
               helpText("Split the aggregate production plan into product families using the Square-Root Rule."),
               numericInput(ns("num_families"), "Number of Product Families:", value = 2, min = 2, max = 5, step = 1),
               uiOutput(ns("family_inputs")),
               hr(),
               helpText("Select a period to see how its aggregate production is divided among families."),
               selectInput(ns("target_period"), "Select Period to Disaggregate:", choices = NULL)
             )
      ),
      column(8,
             h4("Disaggregation Plan (Square-Root Rule)"),
             helpText("Allocation is proportional to the square root of each family's expected demand share."),
             tableOutput(ns("tbl_disagg")),
             plotOutput(ns("plot_disagg"), height = "350px")
      )
    )
  )
}

ap_disaggregation_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update Period Choices based on uploaded data
    observe({
      df <- shared_data$df()
      req(nrow(df) > 0)
      updateSelectInput(session, "target_period", choices = df$Period)
    })
    
    # Generate dynamic inputs for families
    output$family_inputs <- renderUI({
      n_fam <- input$num_families
      req(n_fam > 0)
      
      lapply(1:n_fam, function(i) {
        fluidRow(
          column(6, textInput(ns(paste0("fam_name_", i)), paste("Family", i, "Name:"), value = paste("Fam", i))),
          column(6, numericInput(ns(paste0("fam_dem_", i)), "Annual Demand:", value = 1000 * i, min = 1))
        )
      })
    })
    
    # Disaggregation Logic
    disagg_results <- reactive({
      df <- shared_data$df()
      req(nrow(df) > 0, input$target_period)
      
      # For demonstration, we use the Level Plan's production amount for the selected period
      # (Ideally, you'd let the user choose which plan's production to disaggregate)
      # We calculate the level production quickly here:
      NetD <- pmax(0, df$Demand) 
      NetD[1] <- df$Demand[1] - shared_data$params()$I0
      NetD[nrow(df)] <- df$Demand[nrow(df)] + shared_data$params()$Iend
      level_w <- sum(NetD) / sum(shared_data$params()$K * df$WorkingDays)
      level_w <- ceiling(level_w) # Using rounded up workers
      
      period_idx <- which(df$Period == input$target_period)
      req(length(period_idx) > 0)
      
      total_aggregate_production <- level_w * shared_data$params()$K * df$WorkingDays[period_idx]
      
      # Read family inputs
      n_fam <- input$num_families
      fam_names <- character(n_fam)
      fam_demands <- numeric(n_fam)
      
      for(i in 1:n_fam) {
        name_val <- input[[paste0("fam_name_", i)]]
        dem_val <- input[[paste0("fam_dem_", i)]]
        fam_names[i] <- if (is.null(name_val) || name_val == "") paste("Fam", i) else name_val
        fam_demands[i] <- if (is.null(dem_val)) 0 else dem_val
      }
      
      # Square-Root Rule Logic
      sqrt_demands <- sqrt(fam_demands)
      sum_sqrt <- sum(sqrt_demands)
      
      allocation_ratios <- sqrt_demands / sum_sqrt
      allocated_prod <- total_aggregate_production * allocation_ratios
      
      res_df <- data.frame(
        Family = fam_names,
        Demand = fam_demands,
        `Sqrt(Demand)` = round(sqrt_demands, 2),
        `Allocation Ratio (%)` = round(allocation_ratios * 100, 2),
        `Disaggregated Production` = round(allocated_prod, 0),
        check.names = FALSE
      )
      
      list(table = res_df, total_prod = total_aggregate_production, period_name = input$target_period)
    })
    
    output$tbl_disagg <- renderTable({
      req(disagg_results())
      disagg_results()$table
    }, striped = TRUE, hover = TRUE, align = 'c')
    
    output$plot_disagg <- renderPlot({
      req(disagg_results())
      res <- disagg_results()
      
      ggplot(res$table, aes(x = Family, y = `Disaggregated Production`, fill = Family)) +
        geom_bar(stat = "identity", color = "black") +
        geom_text(aes(label = `Disaggregated Production`), vjust = -0.5, fontface = "bold") +
        theme_minimal() +
        labs(title = paste("Production Allocation for", res$period_name, "(Total:", round(res$total_prod, 0), "units)"),
             y = "Allocated Units", x = "Product Family") +
        theme(legend.position = "none")
    })
    
  })
}