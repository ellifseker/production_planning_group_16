library(shiny)
library(readxl)

# Source all sub-modules
source("R/ap_heuristics.R")
source("R/ap_lp.R")
source("R/ap_transportation.R")
source("R/ap_disaggregation.R")

aggregate_planning_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Aggregate Planning Inputs"),
      fileInput(ns("file"), "Upload Data (.xlsx or .csv)", accept = c(".xlsx", ".xls", ".csv")),
      
      # FORECASTING MODÜLÜNDEKİ GİBİ NOT EKLENDİ
      helpText(strong("Note:"), "The first three columns of your uploaded file will be automatically detected as Period, Demand, and Working Days, respectively."),
      
      hr(),
      
      h5("Unit Costs"),
      fluidRow(
        column(6, numericInput(ns("c_hire"), "Hire ($):", 500, min=0)),
        column(6, numericInput(ns("c_fire"), "Fire ($):", 1000, min=0))
      ),
      fluidRow(
        column(6, numericInput(ns("c_hold"), "Hold ($):", 10, min=0)),
        column(6, numericInput(ns("c_rt"), "Reg. Time ($):", 50, min=0))
      ),
      fluidRow(
        column(6, numericInput(ns("c_ot"), "Overtime ($):", 75, min=0)),
        column(6, numericInput(ns("c_sub"), "Subcontract ($):", 90, min=0))
      ),
      
      h5("Plant Data & Productivity"),
      fluidRow(
        column(6, numericInput(ns("w0"), "Start Workforce:", 50, min=0)),
        column(6, numericInput(ns("i0"), "Start Inventory:", 100, min=0))
      ),
      fluidRow(
        column(6, numericInput(ns("iend"), "End Inv Target:", 0, min=0)),
        column(6, numericInput(ns("k"), "Productivity (K):", 10, min=0.1))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Heuristic Plans", br(), ap_heuristics_ui(ns("heuristics_mod"))),
        tabPanel("Linear Programming", br(), ap_lp_ui(ns("lp_mod"))),
        tabPanel("Transportation Method", br(), ap_transportation_ui(ns("trans_mod"))),
        tabPanel("Disaggregation", br(), ap_disaggregation_ui(ns("disagg_mod")))
      )
    )
  )
}

aggregate_planning_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Read Data
    raw_df <- reactive({
      if (is.null(input$file)) {
        return(data.frame(
          Period = paste("P", 1:6, sep=""),
          Demand = c(12000, 15000, 18000, 24000, 16000, 14000),
          WorkingDays = c(20, 18, 22, 21, 20, 21)
        ))
      }
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext %in% c("xlsx", "xls")) {
        df <- as.data.frame(readxl::read_excel(input$file$datapath))
      } else {
        df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      }
      colnames(df)[1:3] <- c("Period", "Demand", "WorkingDays")
      return(df)
    })
    
    # 2. Package inputs to share with sub-modules
    shared_data <- list(
      df = reactive({ raw_df() }),
      costs = reactive({
        list(Hire = input$c_hire, Fire = input$c_fire, Hold = input$c_hold,
             RT = input$c_rt, OT = input$c_ot, Sub = input$c_sub)
      }),
      params = reactive({
        list(W0 = input$w0, I0 = input$i0, Iend = input$iend, K = input$k)
      })
    )
    
    # 3. Call Sub-Module Servers
    ap_heuristics_server("heuristics_mod", shared_data)
    ap_lp_server("lp_mod", shared_data)
    ap_transportation_server("trans_mod", shared_data)
    ap_disaggregation_server("disagg_mod", shared_data)
    
  })
}