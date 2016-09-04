pivot_table_gadget <- function(x, refresh_on_start = TRUE) {

  ui <- miniUI::miniPage(title = "pivotTableFlavoR",
    
    miniUI::miniButtonBlock(
      miniUI::miniTitleBarButton("controls","Controls", primary = FALSE),
      miniUI::miniTitleBarButton("refresh","Refresh", primary = FALSE),
      miniUI::miniTitleBarButton("help","Help", primary = FALSE),
      miniUI::miniTitleBarButton("done","Done", primary = TRUE)
    ),
    
#    miniUI::gadgetTitleBar("Pivot Table", 
#                           left = miniUI::miniTitleBarButton("controls","controls", primary = FALSE), 
#                           right = miniUI::miniTitleBarButton("done","Done", primary = TRUE)),
    
    miniUI::miniContentPanel(
      shiny::conditionalPanel(
        condition = "input.controls%2==0",
          shiny::fluidRow(
            shiny::column(width = 4,
                          shiny::selectizeInput("rows", label = "Pivot Row", choices = names(x), multiple = TRUE),
                          shiny::conditionalPanel(condition = "input.help%2==1",
                                                  shiny::p(
                                                    "Pivot Row: Rows to group by."
                                                  )),
                          shiny::textInput("filter", label = "Filter"),
                          shiny::conditionalPanel(condition = "input.help%2==1",
                            shiny::p(
                              "Filter: Filter condition e.g col1 == 'value'. Use & | for multiple conditions"
                            ))
                          ),
            shiny::column(width = 4,
                          shiny::selectizeInput("cols", label = "Pivot Column", choices = names(x), multiple = TRUE),
                          shiny::conditionalPanel(condition = "input.help%2==1",
                                                  shiny::p(
                                                    "Pivot Column: Columns to group by and Pivot accross."
                                                  )),
                          shiny::textInput("calc_col", label = "Calculated Column"),
                          shiny::conditionalPanel(condition = "input.help%2==1",
                                                  shiny::p(
                                                    "Calculated Column: Create new columns e.g NewCol1 = col1 + col2, NewCol2 = col1__a - col__2a. Note: if a column heading is split among lines, use __ inbetween words"
                                                  ))
                          ),
            shiny::column(width = 4,
                          shiny::textInput("metric", label = "Pivot Metric"),
                          shiny::conditionalPanel(condition = "input.help%2==1",
                                                  shiny::p(
                                                    "Pivot Metric: Metric used to summarise group by eg MySum = sum(col1), MyMean = mean(col1)"
                                                  ))
                          )
                          #shiny::selectizeInput("sort", label = "Sort", choices = names(x), multiple = TRUE),
                          
            )
          
      ),
      
      shiny::div(
        DT::dataTableOutput("pt", width = "100%", height = "100%")
      )
    )
    

  )

  server <- function(input, output, session) {
    
    pt_selection <- shiny::reactive({
      new_pt_options(
        pt_filter = input$filter,
        #pt_sort = input$sort,
        pt_metric = input$metric,
        pt_row = input$rows,
        pt_col = input$cols,
        pt_calc_col = input$calc_col
      )
    })
    
    pt_data <- shiny::reactive({
      pt_calc(x, pt_selection())
    })
    
    
    output$pt <- DT::renderDataTable({
      
      d <- pt_data()
      names(d) <- gsub("__","<br>",names(d))
      
      DT::datatable(d, rownames = FALSE, filter = "none", escape = FALSE, options = list(
        scrollY = TRUE, scrollX = TRUE, searching = FALSE, lengthChange = FALSE
      ))
    })
    
    shiny::observeEvent(input$done, {
      shiny::stopApp(pt_data())
    })
    
  }
  
  shiny::runGadget(ui, server,viewer = shiny::dialogViewer("Pivot Table", width = 1000, height = 600))
}



