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
              label = "Minimum number of measurements",
              input = div(
                class = "active--pink",
                shiny::sliderInput(
                  inputId = ns("min_measurements"),
                  label = NULL,
                  min = 1,
                  max = 40,
                  step = 1,
                  value = 2
                )
              )
            ),
            formGroup(
              label = "Use linked data",
              input = div(
                class = "active--pink",
                switchInput(
                  id = ns("use_shared"),
                  choices = "Using linked data may slow down graph load times, but will allow interaction between the graphs and tables.",
                  values = "use",
                  selected = "use"
                )
              )
            )
          ),
          navPane(
            id = ns("pane_instructions"),
            fade = FALSE,
            p("The Selectivity app helps you find selective and potent small molecules against your target of interest."),
            p("To use the Selectivity app:"),
            tags$ol(
              class = "pl-4",
              tags$li("Select a gene of interest in the top left corner of the",
                      "application"),
              tags$li("Change the filter settings as needed"),
              tags$li("Look at the 'Affinity and selectivity plot' and select",
                      "a region of compounds you are interested in"),
              tags$li("The 'Affinity and selectivity data' will change upon",
                      "your selection in (3), select the compound you are most",
                      "interested in to see all its known targets in the",
                      "'Affinity and selectivity reference' (you may have to",
                      "scroll down)")
            )
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
        textOutput(ns("subtitle_data"), h6) %>%
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
        textOutput(ns("subtitle_reference"), h6) %>%
          margin(b = 3),
        div(
          dataTableOutput(
            outputId = ns("selection_table"),
            height = "375px"
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

  # update query gene select ----
  observeEvent(selection_genes(), {
    updateSelectInput(
      id = "query_gene",
      choices = names(symbol_gene_id_map),
      selected = input$query_gene %||% "BRAF",
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

    copy(data_affinity_selectivity)[
      gene_id %in% symbol_gene_id_map[[input$query_gene]]
    ][
      is_filter_match := !is.na(Kd_Q1) &
        Kd_Q1 >= (2**input$affinity[1]) &
        Kd_Q1 <= (2**input$affinity[2]) &
        n_measurement_kd >= input$min_measurements
    ][
      order(selectivity_class, Kd_Q1)
    ]
  })

  c_shared_data <- crosstalk::SharedData$new(c_binding_data)

  if (isTRUE(getOption("sms.debug"))) {
    observe({
      message("[ c_binding_data ]")
      print(c_binding_data())
    })
  }

  use_shared_data <- reactive({
    !is.null(input$use_shared)
  })

  selection_genes <- reactive({
    data_affinity_selectivity[["symbol"]] %>%
      unique()
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
    if (use_shared_data()) {
      matched_data <- c_shared_data
    } else {
      matched_data <- c_binding_data()
    }

    p <- matched_data %>%
      plot_ly(
        source = "mainplot"
      ) %>%
      add_markers(
        x = ~ selectivity_plot,
        y = ~ `mean_Kd_(nM)`,
        type = "scatter",
        mode = "markers",
        hoverinfo = "text",
        color = ~ ifelse(is_filter_match, as.character(selectivity_class), "Outside of filters"),
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
        dragmode = "select",
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
          ticktext = c("NA", as.character(seq(-0.25, 1.25, .25)))
        ),
        yaxis = list(
          range = c(input$affinity[1], input$affinity[2]),
          title = "Mean Kd (nM)",
          type = "log"
        )
      ) %>%
      highlight(
        on = "plotly_selected",
        off = "plotly_deselect",
        color = I('#ec4353')
      )

    # if restoring from a bookmark, select previously selected points
    # p$x$highlight$defaultValues <- c_binding_data()$name[state$points_selected]
    # p$x$highlight$color <- "rgba(255,0,0,1)"
    # p$x$highlight$off <- "plotly_deselect"

    # p %>%
    #   layout(dragmode = "select")
    p
  })

  # output_table ----
  state <- reactiveValues(selected_rows = NULL)

  observe({
    state$selected_rows <- event_data("plotly_selected", "mainplot")$key
  })

  observeEvent(input$query_gene, {
    state$selected_rows <- NULL
  })

  tbl_data <- reactive({
    # c_binding_data() %>%
    #   dplyr::filter(is_filter_match) %>%
    #   dplyr::select(-selectivity_plot)

    .data <- if (use_shared_data()) {
      if (is.null(state$selected_rows)) {
        c_binding_data()
      } else {
        c_binding_data()[state$selected_rows]
      }
    } else {
      c_binding_data()
    }

    .data %>%
      dplyr::select(-selectivity_plot)
  })

  tbl_table <- reactive({

    .data <- tbl_data() %>%
      dplyr::mutate(
        chembl_id = unname(data_cmpd_map[name]),
        name = ifelse(is.na(chembl_id), name, glue(
          "<a target='_blank'
              href='https://www.ebi.ac.uk/chembl/compound_report_card/{ chembl_id }'
              >{ name }<sup class='ml-1'><i class='fa fa-external-link'></i></sup>
           </a>"
        )),
        name = lapply(name, HTML),
        ` ` = NA_character_
      ) %>%
      dplyr::select(` `, dplyr::everything()) %>%
      dplyr::select(-chembl_id)

    download_name <- create_download_filename(
      c("compounds", "targeting", input$query_gene)
    )

    DT::datatable(
      .data,
      extensions = c("Buttons", "Select"),
      fillContainer = FALSE,
      rownames = FALSE,
      selection = "single",
      options = list(
        # autoWidth = TRUE,
        buttons = list(
          list(extend = "copy"),
          list(
            extend = "csv",
            title = download_name
          ),
          list(
            extend = "excel",
            title = download_name
          ),
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
            targets = grep(
              pattern = "^( |name|symbol|selectivity_class|On_target_Q1|off_target_Q1)$",
              x = names(.data),
              invert = TRUE
            ) - 1,
            visible = FALSE
          ),
          list(
            visible = FALSE,
            targets = match(c("investigation_bias", "wilcox_pval", "IC50_diff"), names(.data)) - 1
          )
        ),
        dom = 'lfrtipB',
        pagingType = "numbers",
        scrollX = FALSE,
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

  r_selection_title <- reactive({
    req(r_selection_drugs())

    hms_id <- tbl_data()$hms_id[tbl_selection()]

    paste0(hms_id, "; ", r_selection_drugs())
  })

  output$subtitle_reference <- renderText({
    r_selection_title()
  })

  get_selection_data <- function(drug) {
    if (is.na(drug) || is.null(drug) || length(drug) < 1) {
      return(
        data_affinity_selectivity[
          FALSE
        ]
      )
    }

    data_affinity_selectivity[
      lspci_id == name_lspci_id_map[drug] &
        (Kd_Q1 >= 10**input$affinity[1] | is.na(Kd_Q1)) &
        (Kd_Q1 <= 10**input$affinity[2] | is.na(Kd_Q1)) &
        n_measurement_kd >= input$min_measurements
    ][
      order(selectivity_class, Kd_Q1)
    ]
  }

  tbl_selection_table <- reactive({
    hms_id <- tbl_data()$hms_id[tbl_selection()]

    download_name <- create_download_filename(
      c("affinity", "spectrum", r_selection_drugs(), hms_id)
    )

    get_selection_data(r_selection_drugs()) %>%
      DT::datatable(
        rownames = FALSE,
        options = list(
          dom = "tpB",
          buttons = list(
            list(
              extend = "copy"
            ),
            list(
              extend = "csv",
              title = download_name
            ),
            list(
              extend = "excel",
              title = download_name
            )
          ),
          language = list(
            emptyTable = if (is.null(tbl_selection())) {
              "Please select a row from the data above."
            } else {
              "No data available."
            }
          ),
          pagingType = "numbers",
          scrollX = FALSE
          # autoWidth = TRUE
        )
      )
  })

  output$selection_table <- DT::renderDataTable(
    tbl_selection_table(),
    server = FALSE
  )
}
