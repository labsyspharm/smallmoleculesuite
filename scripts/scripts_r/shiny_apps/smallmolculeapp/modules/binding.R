bindingDataUI <- function(id) {
  ns <- NS(id)
  
  columns(
    column(
      width = 6,
      card(
        formGroup(
          label = "Select target name",
          input = selectInput(
            id = ns("target_name")
          ),
          help = "Search for a name"
        ),
        div(
          dataTableOutput(
            outputId = ns("table_by_name")
          )
        ),
        downloadButton(
          outputId = ns("export_name_data"),
          label = "Download binding data"
        ) %>%
          margin(top = 2) %>% 
          background("blue") %>%
          font("white")
      ) %>% 
        shadow("small")
    ),
    column(
      width = 6,
      card(
        formGroup(
          label = "Select target symbol",
          input = selectInput(
            id = ns("target_symbol")
          ),
          help = "Search for a symbol"
        ),
        div(
          dataTableOutput(
            outputId = ns("table_by_symbol")
          )
        ),
        downloadButton(
          outputId = ns("export_symbol_data"),
          label = "Download binding data"
        ) %>%
          margin(top = 2) %>% 
          background("blue") %>%
          font("white")
      ) %>% 
        shadow("small")
    )
  )
}

bindingDataServer <- function(input, output, session) {
  observe({
    updateSelectInput(
      id = "target_name",
      choices = data_affinity_by_name %>% dplyr::distinct(name) %>% dplyr::pull(),
      selected = data_affinity_by_name$name[1],
      session = session
    )
    
    updateSelectInput(
      id = "target_symbol",
      choices = data_affinity_by_symbol %>% dplyr::distinct(symbol) %>% dplyr::pull(),
      selected = data_affinity_by_symbol$symbol[1],
      session = session
    )
  })
  
  r_affinity_by_name <- reactive({
    req(input$target_name)
    
    data_affinity_by_name %>% 
      dplyr::filter(name == input$target_name) %>%
      dplyr::select(-name)
      # dplyr::select(symbol, selectivity_class, `mean_Kd_(nM)`) %>% 
      # dplyr::mutate(
      #   selectivity_class = factor(selectivity_class, SELECTIVITY_ORDER)
      # ) %>%
      # dplyr::arrange(selectivity_class, `mean_Kd_(nM)`)
  })
  
  r_affinity_by_symbol <- reactive({
    req(input$target_symbol)
    
    data_affinity_by_symbol %>% 
      dplyr::filter(symbol == input$target_symbol) %>% 
      dplyr::select(-symbol)
  })
  
  output$table_by_name <- DT::renderDataTable({
    DT::datatable(
      r_affinity_by_name(),
      class = "font-size-sm datatable--compact",
      options = list(
        dom = "tp",
        ordering = FALSE,
        pagingType = "numbers",
        searchHighlight = TRUE
      ),
      rownames = FALSE,
      selection = "none"
    )
  })
  
  output$table_by_symbol <- DT::renderDataTable({
    DT::datatable(
      r_affinity_by_symbol(),
      class = "font-size-sm datatable--compact",
      options = list(
        dom = "tp",
        ordering = FALSE,
        pagingType = "numbers",
        searchHighlight = TRUE
      ),
      rownames = FALSE,
      selection = "none"
    )
  })
  
  output$export_name_data <- downloadHandler(
    filename = function() {
      sprintf("%s-.zip", create_download_filename(
        c("binding", "xyz", input$target_name)
      ))
    },
    content = function(path) {
      contents <- list(
        dplyr::filter(data_affinity_selectivity, name == input$target_name)
      )
      names(contents) <- sprintf("%s.csv", create_download_filename(
        c("binding", "xyz", input$target_name)
      ))

      write_zip(contents, path)
    }
  )
  
  output$export_symbol_data <- downloadHandler(
    filename = function() {
      sprintf("%s-.zip", create_download_filename(
        c("binding", "xyz", input$target_symbol)
      ))
    },
    content = function(path) {
      contents <- list(
        dplyr::filter(data_affinity_selectivity, symbol == input$target_symbol)
      )
      names(contents) <- sprintf("%s.csv", create_download_filename(
        c("binding", "xyz", input$target_symbol)
      ))
      
      write_zip(contents, path)
    }
  )
}
