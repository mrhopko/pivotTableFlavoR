pivot_table_moduleOutput <- function(id) {
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::fluidRow(
      shiny::actionButton(ns("controls"), "Controls")
    ),
    shiny::conditionalPanel(condition = paste0("input.[",ns("controls"),"]%2==0"),
  shiny::fluidRow(
    shiny::column(width = 2,
                  shiny::uiOutput(ns("rows_o"))
               ),
   shiny::column(width = 2,
                 shiny::uiOutput(ns("cols_o"))
               ),
   shiny::column(width = 2,
                 shiny::uiOutput(ns("sort_o"))
               ),
   shiny::column(width = 2,
                 shiny::textInput(ns("filter", label = "filter"))
               ),
   shiny::column(width = 2,
                 shiny::textInput(ns("metric", label = "metric"))
               )
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 12,
             DT::dataTableOutput(ns("dataTable"), width = "100%", height = 800)
             )
    )
  )

}

pivot_table_module <- function(input, output, session, rf_data_table) {
  
  ns <- session$ns
  
  data_names <- shiny::reactive({
    names(rf_data_table())
  })
  
  output$rows_o <- shiny::renderUI({
    shiny::selectizeInput(ns("rows"), choices = data_names(), multiple = TRUE)
  })
  
  output$cols_o <- shiny::renderUI({
    shiny::selectizeInput(ns("cols"), choices = data_names(), multiple = TRUE)
  })
  
  output$sort_o <- shiny::renderUI({
    shiny::selectizeInput(ns("sort"), choices = data_names(), multiple = TRUE)
  })
  
  pt_selection <- reactive({
    new_pt_options(
      pt_filter = input$filter,
      pt_sort = input$sort,
      pt_metric = input$metric,
      pt_row = input$rows,
      pt_col = input$cols
    )
  })
  
  pt_data <- shiny::reactive({
    pt_calc(rf_data_table(), pt_selection())
  })
  
  output$pt <- DT::renderDataTable({
    
    d <- pt_data()
    names(d) <- gsub("__","<br>",names(d))
    
    DT::datatable(d, rownames = FALSE, filter = "top", escape = FALSE, options = list(
      scrollY = TRUE, scrollX = TRUE, searching = TRUE
    ))
  })
  
  return(pt_data)

}