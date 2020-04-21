
axis_choices <- tribble(
  ~label, ~value, ~type,
  "Kd Q1 (nM)", "Kd_Q1", "log",
  "Selectivity", "selectivity", "linear",
  "Tool score", "toolscore", "linear",
  "Ontarget IC50 Q1 (nM)", "ontarget_IC50_Q1", "log",
  "Offtarget IC50 Q1 (nM)", "offtarget_IC50_Q1", "log",
  "IC50 difference (nM)", "IC50_diff", "linear"
)

axis_choice_map <- axis_choices %>%
  {set_names(map2(.[["label"]], .[["type"]], ~list(label = .x, type = .y)), .[["value"]])}

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
              label = "Target gene",
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
              help = "Search for a target gene"
            ),
            formGroup(
              label = "Minimum/maximum affinity",
              input = div(
                class = "logify-slider-10 active--pink",
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
      ),
      mod_ui_chembl_tabs(ns("chembl_tabs_1"))
    ),
    column(
      width = 8,
      card(
        h3("Affinity and selectivity plot"),
        h6(textOutput(ns("subtitle_plot"))) %>%
          margin(b = 3),
        div(
          tags$label("x-axis", `for` = ns("x_var"), style = "width: 7em;") %>%
            margin(b = 0),
          selectInput(
            ns("x_var"),
            choices = axis_choices[["label"]],
            values = axis_choices[["value"]],
            selected = "selectivity"
          ),
          tags$label("y-axis", `for` = ns("y_var"), style = "width: 7em;") %>%
            margin(l = 2, b = 0),
          selectInput(
            ns("y_var"),
            choices = axis_choices[["label"]],
            values = axis_choices[["value"]],
            selected = "Kd_Q1"
          ),
          style = "display: flex; align-items: center;"
        ),
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

  include_non_human <- reactive({
    !is.null(input$include_genes)
  })

  # update query gene select ----
  observeEvent(selection_genes(), {
    updateSelectInput(
      id = "query_gene",
      choices = selection_genes(),
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
      symbol == input$query_gene
    ][
      , is_filter_match := !is.na(Kd_Q1) &
        Kd_Q1 >= (10**input$affinity[1]) &
        Kd_Q1 <= (10**input$affinity[2]) &
        n_measurement_kd >= input$min_measurements
    ][
      , c("name", "plot_alpha", "selectivity_class") := .(
        lspci_id_name_map[lspci_id],
        if_else(is_filter_match, 0.8, 0.4),
        forcats::fct_rev(selectivity_class)
      )
    ][
      order(is_filter_match, selectivity_class, Kd_Q1)
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
    if (include_non_human()) { # all genes
      data_affinity_selectivity[["symbol"]] %>%
        unique() %>%
        sort()
    } else { # just human genes
      data_affinity_selectivity[
        gene_id %in% (data_gene_info[tax_id == 9606][["gene_id"]])
      ][["symbol"]] %>%
        unique() %>%
        sort()
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
    if (use_shared_data()) {
      matched_data <- c_shared_data
    } else {
      matched_data <- c_binding_data()
    }

    x_axis_vals <- axis_choice_map[[input$x_var]]
    y_axis_vals <- axis_choice_map[[input$y_var]]

    p <- matched_data %>%
      plot_ly(
        source = "mainplot"
      ) %>%
      add_markers(
        x = ~ get(input$x_var),
        y = ~ get(input$y_var),
        type = "scatter",
        mode = "markers",
        hoverinfo = "text",
        color = ~ selectivity_class,
        colors = rev(c("#225ea8", "#41b6c4", "#a1dab4", "#969696", "#cccccc")),
        opacity = ~ if_else(is_filter_match, 0.8, 0.3),
        text = ~ paste(
          sep = "",
          "Drug name: ", name, "\n",
          "Gene symbol: ", symbol,"\n",
          "x: ", round(get(input$x_var), digits = 2), "\n",
          "y: ", round(get(input$y_var), digits = 2)
        ),
        marker = list(
          size = 8
        )
      ) %>%
      layout(
        dragmode = "select",
        showlegend = TRUE,
        xaxis = list(
          title = x_axis_vals[["label"]],
          type = x_axis_vals[["type"]]
        ),
        yaxis = list(
          title = y_axis_vals[["label"]],
          type = y_axis_vals[["type"]]
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
    (if (use_shared_data() && !is.null(state$selected_rows)) {
      c_binding_data()[as.integer(state$selected_rows), ]
    } else {
      c_binding_data()
    })[
      order(-selectivity_class, ontarget_IC50_Q1)
    ] %>%
      select(name, symbol, selectivity_class, ontarget_IC50_Q1, offtarget_IC50_Q1, everything())
  })

  tbl_table <- reactive({

    .data <- tbl_data()

    download_name <- create_download_filename(
      c("compounds", "targeting", input$query_gene)
    )

    DT::datatable(
      .data,
      extensions = c("Buttons"),
      fillContainer = FALSE,
      rownames = FALSE,
      selection = "multiple",
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
              pattern = "^(name|symbol|selectivity_class|ontarget_IC50_Q1|offtarget_IC50_Q1)$",
              x = names(.data),
              invert = TRUE
            ) - 1,
            visible = FALSE
          )
        ),
        dom = 'lfrtipB',
        pagingType = "numbers",
        scrollX = FALSE,
        searchHighlight = TRUE
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

    tbl_data()$lspci_id[tbl_selection()]
  })

  o_chembl_tabs <- callModule(
    mod_server_chembl_tabs, "chembl_tabs_1", data_cmpd_info, r_selection_drugs, lspci_id_name_map
  )

  r_selection_title <- reactive({
    req(r_selection_drugs())

    hms_id <- tbl_data()$hms_id[tbl_selection()]

    paste0(r_selection_drugs(), "; ")
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
      lspci_id %in% drug &
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
