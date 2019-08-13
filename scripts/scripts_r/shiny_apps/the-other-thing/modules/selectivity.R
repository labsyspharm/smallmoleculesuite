selectivityUI <- function(id) {
  ns <- NS(id)
  
  columns(
    column(
      width = 4,
      card(
        header = navInput(
          appearance = "tabs",
          id = ns("nav"),
          choices = c("Filters", "Instructions"),
          values = c("filters", "instructions"),
          selected = "filters"
        ),
        navContent(
          navPane(
            id = ns("pane_filters"),
            fade = FALSE,
            formGroup(
              label = "Gene target",
              checkboxInput(
                id = ns("include_genes"),
                choices = "Include non-human genes",
                values = TRUE
              )
            ) %>% 
              margin(b = -3),
            formGroup(
              label = NULL,
              input = selectInput(
                id = ns("query_gene")
              ),
              help = "Search for a gene target"
            ),
            formGroup(
              label = "Minimum/maximum affinity",
              sliderInput(
                inputId = ns("affinity"),
                label = NULL, 
                min = -3,
                max = 10,
                step = 1,
                value = c(-3, 6)
              )
            ),
            formGroup(
              label = "Maximum std. dev. of affinity",
              sliderInput(
                inputId = ns("sd"),
                label = NULL, 
                min = 0,
                max = 10,
                step = 1,
                value = 5
              )
            ),
            formGroup(
              label = "Minimum number of measurements",
              sliderInput(
                inputId = ns("min_measurements"),
                label = NULL, 
                min = 1,
                max = 15,
                step = 1,
                value = 2
              )
            )
          ),
          navPane(
            id = ns("pane_instructions"),
            fade = FALSE,
            p("To use the Selectivity portion of the application, first select your target of interest and binding criteria. Subsequently, select a region in the main plot with compounds of your interest. You can then select three compounds in the bottom table and view their known binding affinities in detail.")
          )
        )
      )
    ),
    column(
      width = 8,
      card(
        h3("Affinity and selectivity plot"),
        h6(textOutput(ns("subtitle_plot"))) %>% 
          margin(b = 3),
        div(
          plotly::plotlyOutput(
            outputId = ns("mainplot"),
            height = "400px"
          )
        )
      ) %>% 
        margin(bottom = 3),
      card(
        h3("Affinity and selectivity data"),
        h6(textOutput(ns("subtitle_data"))) %>% 
          margin(b = 3),
        div(
          dataTableOutput(
            outputId = ns("output_table"),
            height = "575px"
          )
        )
      ) %>% 
        margin(bottom = 3),
      card(
        h3("Affinity and selectivity reference"),
        div(
        columns(
          column(
            width = 3,
            listGroupInput(
              id = ns("select_gene"),
              class = "active--pink"
            )
          ),
          column(
            navContent(
              navPane(
                id = ns("pane_select_1"),
                fade = FALSE,
                dataTableOutput(
                  outputId = ns("select_1"),
                  height = "500px"
                )
              ),
              navPane(
                id = ns("pane_select_2"),
                fade = FALSE,
                dataTableOutput(
                  outputId = ns("select_2"),
                  height = "500px"
                )
              ),
              navPane(
                id = ns("pane_select_3"),
                fade = FALSE,
                dataTableOutput(
                  outputId = ns("select_3"),
                  height = "500px"
                )
              )
            )
          )
        )
        )
      ) %>% 
        margin(b = 2)
    )
  )
}

selectivityServer <- function(input, output, session) {
  ns <- session$ns
  
  observe({
    req(input$nav)
    
    switch(
      input$nav,
      filters = showNavPane(ns("pane_filters")),
      instructions = showNavPane(ns("pane_instructions"))
    )
  })
  
  include_non_human <- reactive({
    !is.null(input$include_genes)
  })
  
  # update query gene select ----
  observeEvent(selection_genes(), {
    updateSelectInput(
      id = "query_gene",
      choices = selection_genes(),
      values = selection_genes(),
      selected = input$query_gene %||% selection_genes()[1],
      session = session
    )
  })
  
  # server state ----
  state = reactiveValues(
    selection_table = NULL,
    num_selected = 0
    # query_gene = NULL # THIS DOES NOT NEED TO EXIST USE `input$query_gene`
  )
  
  c_binding_data <- reactive({
    req(input$query_gene)
    
    data_aff <- data_affinity_selectivity %>%
      filter(symbol == input$query_gene) %>%
      filter(`mean_Kd_(nM)` >= (10 ^ input$affinity[1]) | is.na(`mean_Kd_(nM)`)) %>%
      filter(`mean_Kd_(nM)` <= (10 ^ input$affinity[2]) | is.na(`mean_Kd_(nM)`)) %>%
      filter(`SD_Kd_(nM)` <= (10 ^ input$sd) | is.na(`SD_Kd_(nM)`)) %>%
      filter(n_measurements >= input$min_measurements) %>%
      mutate(selectivity_class = factor(selectivity_class, levels = SELECTIVITY_ORDER)) %>%
      arrange(selectivity_class, `mean_Kd_(nM)`) %>%
      mutate(selectivity_plot = coalesce(selectivity, -0.5))
    
    if (!include_non_human()) {
      data_aff <- filter(data_aff, tax_id == 9606)
    }
    
    data_aff
  })
  
  if (isTRUE(getOption("sms.debug"))) {
    observe({
      message("[ c_binding_data ]")
      print(c_binding_data())
    })
  }
  
  data_shared <- reactive({
    # SharedData$new(c_binding_data(), ~ name)
    c_binding_data()
  })
  
  selection_genes <- reactive({
    if (include_non_human()) { # all genes
      sort(unique(data_affinity_selectivity$symbol))
    } else { # just human genes
      data_affinity_selectivity %>%
        dplyr::filter(tax_id == 9606) %>% 
        dplyr::distinct(symbol) %>% 
        dplyr::arrange(symbol) %>% 
        dplyr::pull()
    }
  })
  
  # titles ----
  r_subtitle <- reactive({
    req(input$query_gene)
    paste("Drugs targeting", input$query_gene)
  })
  
  output$subtitle_plot <- renderText(r_subtitle())
  output$subtitle_data <- renderText(r_subtitle())
  
  # mainplot ----
  output$mainplot <- renderPlotly({
    p <- data_shared() %>%
      plot_ly(
        x = ~ selectivity_plot, 
        y = ~ `mean_Kd_(nM)`, 
        type = "scatter",
        mode = "markers", 
        source = "Z",
        color = ~ selectivity_class, 
        text = ~ paste(
          sep = "",
          "Drug name: ", name, "\n", 
          "Drug HMS ID: ", hms_id, "\n", 
          "Gene symbol: ", symbol,"\n", 
          "x: ", selectivity, "\n",
          "y: ", `mean_Kd_(nM)`
        ), 
        hoverinfo = "text"
      ) %>%
      layout(
        showlegend = T,
        shapes = list(
          list(
            type='line', x0= -0.5, x1= -0.5,
            y0= 10^(input$affinity[1]), y1= 10^(input$affinity[2]),
            line=list(dash='dot', width=2, color = "red")
          )
        ),
        xaxis = list(
          range = c(-0.6, 1.3),
          title = "Selectivity",
          tickmode = "array",
          tickvals = c(-0.5, seq(-0.25, 1.25, .25)),
          ticktext = c("NA", as.character(seq(-0.25, 1.25, .25)))),
        yaxis = list(
          range = c(input$affinity[1], input$affinity[2]),
          title = "Mean Kd (nM)",
          type = "log"
        )
      )
      # highlight("plotly_selected", color = I('red'), hoverinfo = "text")
    
    # if restoring from a bookmark, select previously selected points
    # p$x$highlight$defaultValues <- c_binding_data()$name[state$points_selected]
    # p$x$highlight$color <- "rgba(255,0,0,1)"
    # p$x$highlight$off <- "plotly_deselect"
    
    # p %>% 
    #   layout(dragmode = "select")
    p
  })
  
  # output_table ----
  data_tbl <- reactive({
    c_binding_data() %>%
      dplyr::select(-selectivity_plot)
  })
  
  output$output_table <- DT::renderDataTable(
    expr = data_tbl(),
    server = TRUE,
    extensions = 'Buttons',
    fillContainer = TRUE,
    rownames = FALSE,
    options = list(
      autoWidth = TRUE,
      buttons = c('copy', 'csv', 'excel', 'colvis'),
      columnDefs = list(
        list(
          visible = FALSE,
          targets = match(c("investigation_bias", "wilcox_pval", "IC50_diff"), names(c_binding_data())) - 1
        )
      ),
      dom = 'lfrtipB',
      pagingType = "numbers",
      scrollX = TRUE,
      searchHighlight = TRUE
    )
  )
  
  # table row selection ----
  observe({
    if (is.null(input$output_table_rows_selected)) {
      hideNavPane(ns("pane_selections"))
    } else if (!is.null(input$output_table_rows_selected)) {
      showNavPane(ns("pane_selections"))
    }
  })
  
  r_selection_drugs <- reactive({
    req(data_tbl(), cancelOutput = FALSE)
    
    if (is.null(input$output_table_rows_selected)) {
      return(NULL)
    }
    
    drug_names <- data_tbl()$name[input$output_table_rows_selected]
    
    head(drug_names, 3)
  })
  
  observeEvent(r_selection_drugs(), {
    if (isTRUE(input$select_gene %in% r_selection_drugs())) {
      x_selected <- input$select_gene
    } else {
      x_selected <- r_selection_drugs()[1]
    }
    
    updateListGroupInput(
      id = "select_gene",
      choices = r_selection_drugs(),
      selected = x_selected,
      session = session
    )
  })
  
  observe({
    if (is.null(input$query_gene)) {
      showNavPane(ns("pane_select_1"))
    } else {
      i <- which(input$select_gene == r_selection_drugs())
      showNavPane(ns(paste0("pane_select_", i)))
    }
  })
  
  get_selection_data <- function(drug) {
    if (is.na(drug) || is.null(drug) || length(drug) < 1) {
      return(
        data_affinity_selectivity %>% 
          dplyr::select(symbol, selectivity_class, `mean_Kd_(nM)`) %>% 
          dplyr::slice(0)
      )
    }
    
    data_affinity_selectivity %>%
      filter(name == drug) %>%
      filter(`mean_Kd_(nM)` >= 10^input$affinity[1] | is.na(`mean_Kd_(nM)`)) %>%
      filter(`mean_Kd_(nM)` <= 10^input$affinity[2] | is.na(`mean_Kd_(nM)`)) %>%
      filter(`SD_Kd_(nM)` <= 10^input$sd | is.na(`SD_Kd_(nM)`)) %>%
      filter(n_measurements >= input$min_measurements) %>%
      mutate(selectivity_class = factor(selectivity_class, levels = SELECTIVITY_ORDER)) %>%
      arrange(selectivity_class, `mean_Kd_(nM)`) %>%
      mutate(`mean_Kd_(nM)` = round(`mean_Kd_(nM)`, 3)) %>% 
      dplyr::select(symbol, selectivity_class, `mean_Kd_(nM)`)
  }
  
  selection_tbl_options <- list(
    dom = 'tpB',
    buttons = c('copy', 'csv', 'excel', 'colvis'),
    language = list(
      emptyTable = "Please select row(s) from the data above."
    ),
    pagingType = "numbers",
    scrollX = TRUE
    # autoWidth = TRUE
  )
  
  r_selection_data_1 <- reactive({
    get_selection_data(r_selection_drugs()[1])
  })
  output$select_1 <- DT::renderDataTable(
    r_selection_data_1(),
    extensions = "Buttons",
    rownames = FALSE,
    options = selection_tbl_options
  )
  outputOptions(output, "select_1", suspendWhenHidden = FALSE)

  r_selection_data_2 <- reactive({
    get_selection_data(r_selection_drugs()[2])
  })
  output$select_2 <- DT::renderDataTable(
    r_selection_data_2(),
    extensions = "Buttons",
    rownames = FALSE,
    options = selection_tbl_options
  )
  outputOptions(output, "select_2", suspendWhenHidden = FALSE)

  r_selection_data_3 <- reactive({
    get_selection_data(r_selection_drugs()[3])
  })
  output$select_3 <- DT::renderDataTable(
    r_selection_data_3(),
    extensions = "Buttons",
    rownames = FALSE,
    options = selection_tbl_options
  )
  outputOptions(output, "select_3", suspendWhenHidden = FALSE)
}
