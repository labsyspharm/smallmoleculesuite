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
              input = div(
                class = "logify-slider active--pink",
                shiny::sliderInput(
                  inputId = ns("affinity"),
                  label = NULL, 
                  min = -3,
                  max = 10,
                  step = 1,
                  value = c(-3, 6)
                )
              )
            ),
            formGroup(
              label = "Maximum std. dev. of affinity",
              input = div(
                class = "logify-slider active--pink",
                shiny::sliderInput(
                  inputId = ns("sd"),
                  label = NULL, 
                  min = 0,
                  max = 10,
                  step = 1,
                  value = 5
                )
              )
            ),
            formGroup(
              label = "Minimum number of measurements",
              input = div(
                class = "logify-slider active--pink",
                shiny::sliderInput(
                  inputId = ns("min_measurements"),
                  label = NULL, 
                  min = 1,
                  max = 15,
                  step = 1,
                  value = 2
                )
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
              dataTableOutput(
                outputId = ns("selection_table"),
                height = "500px"
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
      dplyr::filter(
        symbol == input$query_gene
      ) %>% 
      dplyr::mutate(
        is_filter_match = 
          (`mean_Kd_(nM)` >= (10 ^ input$affinity[1]) | is.na(`mean_Kd_(nM)`)) &
          (`mean_Kd_(nM)` <= (10 ^ input$affinity[2]) | is.na(`mean_Kd_(nM)`)) &
          (`SD_Kd_(nM)` <= (10 ^ input$sd) | is.na(`SD_Kd_(nM)`)) &
          (n_measurements >= input$min_measurements)
      ) %>%
      dplyr::mutate(
        selectivity_class = factor(selectivity_class, SELECTIVITY_ORDER)
      ) %>%
      dplyr::arrange(selectivity_class, `mean_Kd_(nM)`) %>%
      dplyr::mutate(selectivity_plot = coalesce(selectivity, -0.5))
    
    if (!include_non_human()) {
      data_aff <- dplyr::filter(data_aff, tax_id == 9606)
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
    p <- plot_ly() %>% 
      add_markers(
        data = dplyr::filter(c_binding_data(), !is_filter_match),
        x = ~ selectivity_plot,
        y = ~ `mean_Kd_(nM)`,
        type = "scatter",
        mode = "markers",
        color = I("black"),
        hoverinfo = "skip",
        showlegend = FALSE,
        marker = list(
          opacity = 0.25,
          size = 8
        )
      ) %>% 
      add_markers(
        data = dplyr::filter(c_binding_data(), is_filter_match),
        x = ~ selectivity_plot, 
        y = ~ `mean_Kd_(nM)`, 
        type = "scatter",
        mode = "markers",
        hoverinfo = "text",
        color = ~ selectivity_class, 
        text = ~ paste(
          sep = "",
          "Drug name: ", name, "\n", 
          "Drug HMS ID: ", hms_id, "\n", 
          "Gene symbol: ", symbol,"\n", 
          "x: ", selectivity, "\n",
          "y: ", `mean_Kd_(nM)`
        ),
        marker = list(
          size = 8
        )
      ) %>%
      layout(
        showlegend = TRUE,
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
  tbl_data <- reactive({
    c_binding_data() %>%
      dplyr::filter(is_filter_match) %>% 
      dplyr::select(-selectivity_plot)
  })
  
  tbl_table <- reactive({
    tbl_data() %>% 
      dplyr::mutate(
        name = glue(
          "<a target='_blank' 
              href='https://www.ebi.ac.uk/chembl/g/#search_results/compounds/query={ name }'
            >{ name }<sup class='ml-1'><i class='fa fa-external-link'></i></sup>
           </a>"
        ),
        name = lapply(name, HTML),
        ` ` = NA_character_
      ) %>% 
      dplyr::select(` `, dplyr::everything()) %>% 
      DT::datatable(
        extensions = c('Buttons', "Select"),
        fillContainer = TRUE,
        rownames = FALSE,
        selection = "multiple",
        options = list(
          autoWidth = TRUE,
          buttons = list(
            list(extend = "copy"),
            list(extend = "csv"),
            list(extend = "excel"),
            list(
              extend = "colvis",
              columns = ":not(.select-checkbox)"
            )
          ),
          columnDefs = list(
            list(
              className = "select-checkbox",
              orderable = FALSE,
              targets = 0
            ),
            list(
              visible = FALSE,
              targets = match(c("investigation_bias", "wilcox_pval", "IC50_diff"), names(c_binding_data())) - 1
            )
          ),
          dom = 'lfrtipB',
          pagingType = "numbers",
          scrollX = TRUE,
          searchHighlight = TRUE,
          select = list(
            style = "os",
            selector = "td.select-checkbox"
          )
        )
      )
  })
  
  output$output_table <- DT::renderDataTable(
    tbl_table(),
    server = FALSE
  )
  
  # table row selection ----
  tbl_selection <- reactive({
    sort(input$output_table_rows_selected)
  })
  
  r_selection_drugs <- reactive({
    if (is.null(tbl_selection())) {
      return(NULL)
    }
    
    drug_names <- tbl_data()$name[tbl_selection()]
    
    head(drug_names, 3)
  })
  
  r_selection_titles <- reactive({
    req(r_selection_drugs())
    
    hms_id <- head(tbl_data()$hms_id[tbl_selection()], 3)
    
    paste0(hms_id, "; ", r_selection_drugs())
  })
  
  observeEvent(r_selection_drugs(), ignoreNULL = FALSE, {
    if (is.null(r_selection_drugs())) {
      updateListGroupInput(
        id = "select_gene",
        choices = "N/A",
        values = "",
        session = session
      )
      
      return()
    }
    
    if (isTRUE(input$select_gene %in% r_selection_drugs())) {
      x_selected <- input$select_gene
    } else {
      x_selected <- tail(r_selection_drugs(), 1)
    }
    
    updateListGroupInput(
      id = "select_gene",
      choices = r_selection_titles(),
      values = r_selection_drugs(),
      selected = x_selected,
      session = session
    )
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
  
  tbl_selection_table <- reactive({
    get_selection_data(input$select_gene) %>% 
      DT::datatable(
        rownames = FALSE,
        options = list(
          dom = "tpB",
          buttons = c("copy", "csv", "excel"),
          language = list(
            emptyTable = if (is.null(tbl_selection())) {
              "Please select row(s) from the data above."
            } else {
              if (is.null(input$select_gene)) {
                "Please make a selection."
              } else {
                "No data available."
              }
            }
          ),
          pagingType = "numbers",
          scrollX = TRUE
          # autoWidth = TRUE
        )
      )
  })
  
  output$selection_table <- DT::renderDataTable(
    tbl_selection_table(),
    server = FALSE
  )
}
