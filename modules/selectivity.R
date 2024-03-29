
axis_choices <- tribble(
  ~label, ~name, ~type, ~limits,
  "Affinity Q1 (nM)", "ontarget_ic50_q1", "log", c(0.1, 100000),
  "Selectivity", "selectivity", "linear", c(0, 1.2),
  "Tool score", "tool_score", "linear", c(0, 10),
  "Offtarget Affinity Q1 (nM)", "offtarget_ic50_q1", "log", c(0.1, 100000),
  "Affinity difference (log10 nM)", "ic50_difference", "linear", c(-5, 5),
  "Investigation bias", "investigation_bias", "linear", c(0, 1),
  "Strength", "strength", "linear", c(1, 10),
  "Wilcox p-value", "wilcox_pval", "linear", c(0, 1)
) %>%
  arrange(label)

axis_choice_map <- axis_choices %>%
  setDT() %>%
  melt("name") %>%
  group_by(name) %>%
  summarize(
    vals = set_names(
      value,
      variable
    ) %>%
      list()
  ) %>% {
    set_names(.[["vals"]], .[["name"]])
  }

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
            tags$h5("Target gene") %>%
              margin(b = 3),
            mod_ui_select_targets(
              ns("query_gene"),
              selectize_options = list(
                label = NULL,
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              )
            ) %>%
              tagAppendChild(
                checkboxInput(
                  id = ns("include_genes"),
                  choices = "Include non-human genes",
                  values = TRUE
                )
              ),
            tags$hr(),
            formGroup(
              label = "Minimum/maximum affinity (nM)",
              input = div(
                class = "logify-slider-10 active--pink",
                shiny::sliderInput(
                  inputId = ns("affinity"),
                  label = NULL,
                  min = -3,
                  max = 6,
                  step = 1,
                  value = c(-3, 6)
                )
              )
            ),
            formGroup(
              label = "Minimum number of affinity measurements",
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
              label = "Commercial availability",
              input = div(
                class = "active--pink",
                mod_ui_filter_commercial(ns(""))
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
      ) %>%
        margin(bottom = 3),
      mod_ui_chembl_tabs(ns(""))
    ),
    column(
      width = 8,
      card(
        # header = tagList(
        #   htmlOutput(ns("subtitle_plot"), container = p)
        # ),
        div(
          class = "d-flex align-items-center",
          tags$label("x-axis", `for` = ns("x_var"), style = "width: 5em;", class = "text-right") %>%
            margin(b = 0, r = 3),
          selectInput(
            ns("x_var"),
            choices = axis_choices[["label"]],
            values = axis_choices[["name"]],
            selected = "selectivity"
          ),
          tags$label("y-axis", `for` = ns("y_var"), style = "width: 5em;", class = "text-right") %>%
            margin(b = 0, r = 3),
          selectInput(
            ns("y_var"),
            choices = axis_choices[["label"]],
            values = axis_choices[["name"]],
            selected = "ontarget_ic50_q1"
          )
        ),
        div(
          plotly::plotlyOutput(
            outputId = ns("mainplot"),
            height = "400px"
          ) %>%
            shinycssloaders::withSpinner(color = "#303030")
        )
      ) %>%
        margin(bottom = 3),
      card(
        # header = tagList(
        #   h4("Target affinity and selectivity"),
        #   htmlOutput(ns("subtitle_data"), container = p)
        # ),
        div(
          dataTableOutput(
            outputId = ns("output_table")
          ),
          mod_ui_download_button(ns("output_table_csv_dl"), "Download CSV"),
          mod_ui_download_button(ns("output_table_xlsx_dl"), "Download Excel")
        )
      ) %>%
        margin(bottom = 3)
      # mod_ui_affinity_tables(ns("affinity_tables"))
    )
  ) %>%
    container()
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

  r_include_non_human <- reactive({
    req(!is.null(input$include_genes))
    input$include_genes
  })

  r_selection_genes <- reactive({
    req(!is.null(r_include_non_human()))
    if (r_include_non_human()) { # all genes
      data_selectivity[["lspci_target_id"]] %>%
        unique()
    } else { # just human genes
      data_target_map[
        tax_id == 9606 &
          lspci_target_id %in% data_selectivity[["lspci_target_id"]],
        .(lspci_target_id)
      ][["lspci_target_id"]]
    }
  })

  r_query_target <- callModule(
    mod_server_select_targets,
    "query_gene",
    data_target_map,
    default_choice = 489,
    r_eligible_targets = r_selection_genes,
    selectize_options = list(
      maxItems = 1L
    )
  )

  r_query_symbol <- reactive({
    req(r_query_target())
    # Show either symbol or, if NA, the gene ID
    target_id_to_name(r_query_target())
  })

  r_eligible_lspci_ids <- callModule(mod_server_filter_commercial, "")[["r_eligible_lspci_ids"]]

  r_binding_data <- reactive({
    req(
      r_query_target(),
      r_eligible_lspci_ids()
    )
    message("Calculating binding ", r_query_target())
    data_selectivity[
      lspci_target_id == r_query_target() & lspci_id %in% r_eligible_lspci_ids()
    ][
      data_compounds[
        ,
        .(lspci_id, name = pref_name, chembl_id)
      ],
      on = .(lspci_id),
      nomatch = NULL
    ][
      data_targets[
        ,
        .(lspci_target_id, symbol, gene_id)
      ],
      on = .(lspci_target_id), nomatch = NULL
    ][
      ,
      is_filter_match := !is.na(ontarget_ic50_q1) &
        ontarget_ic50_q1 >= (10**input$affinity[1]) &
        ontarget_ic50_q1 <= (10**input$affinity[2]) &
        ontarget_n >= input$min_measurements
    ][
      ,
      c("plot_alpha", "selectivity_class") := .(
        if_else(is_filter_match, 0.9, 0.2),
        forcats::fct_rev(selectivity_class)
      )
    ][
      order(is_filter_match, selectivity_class, ontarget_ic50_q1)
    ]
  })

  # titles ----
  # r_subtitle <- reactive({
  #   paste(
  #     "Showing compounds binding", r_query_symbol(), "that meet the filter criteria<br>
  #     Select compounds here for additional information"
  #   )
  # })

  # output$subtitle_plot <- renderText(r_subtitle())
  # output$subtitle_data <- renderText(r_subtitle())

  # mainplot ----
  output$mainplot <- renderPlotly({
    req(r_binding_data())
    message("Plotting binding data ", nrow(r_binding_data()))

    matched_data <- r_binding_data()

    x_axis_props <- axis_choice_map[[input$x_var]]
    y_axis_props <- axis_choice_map[[input$y_var]]

    x_limits <- c(
      min(x_axis_props[["limits"]][1], matched_data[[input$x_var]]),
      max(x_axis_props[["limits"]][2], matched_data[[input$x_var]])
    )
    if (x_axis_props[["type"]] == "log")
      x_limits <- log10(x_limits)

    y_limits <- c(
      min(y_axis_props[["limits"]][1], matched_data[[input$y_var]]),
      max(y_axis_props[["limits"]][2], matched_data[[input$y_var]])
    )
    if (y_axis_props[["type"]] == "log")
      y_limits <- log10(y_limits)

    x_limits <- expand_range(x_limits, mul = 0.05)
    y_limits <- expand_range(y_limits, mul = 0.05)

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
        colors = rev(SELECTIVITY_COLORS),
        customdata = ~ lspci_id,
        text = ~ paste(
          sep = "",
          "Drug name: ", name, "\n",
          "Gene symbol: ", symbol,"\n",
          "x: ", round(get(input$x_var), digits = 2), "\n",
          "y: ", round(get(input$y_var), digits = 2)
        ),
        marker = list(
          size = 8,
          opacity = ~ plot_alpha
        )
      ) %>%
      layout(
        dragmode = "select",
        showlegend = TRUE,
        xaxis = list(
          title = x_axis_props[["label"]],
          type = x_axis_props[["type"]],
          range = x_limits
        ),
        yaxis = list(
          title = y_axis_props[["label"]],
          type = y_axis_props[["type"]],
          range = y_limits
        )
      )
    # %>%
    #   highlight(
    #     on = "plotly_selected",
    #     off = "plotly_deselect",
    #     opacityDim = 0.3
    #   )
    p
  })

  # output_table ----

  r_selected_compounds_plot <- reactive({
    event_data("plotly_selected", "mainplot")$customdata
  })

  r_tbl_data <- reactive({
    req(r_binding_data())
    {
      if (
        !is.null(r_selected_compounds_plot()) &&
          length(r_selected_compounds_plot()) > 0
      )
        r_binding_data()[lspci_id %in% r_selected_compounds_plot()]
      else
        r_binding_data()
    }[
      is_filter_match == TRUE
    ][
      order(-selectivity_class, ontarget_ic50_q1)
    ] %>%
      select(
        name, chembl_id, symbol, gene_id, selectivity_class, ontarget_ic50_q1, offtarget_ic50_q1,
        everything(),
        -is_filter_match, -plot_alpha
      )
  })

  selectivity_reference_js <- callModule(
    mod_server_reference_modal, "selectivity"
  )

  r_table_dt <- reactive({
    req(r_tbl_data())

    .data <- r_tbl_data()

    datatable_tooltip(
      .data,
      extensions = c("Buttons"),
      style = "bootstrap4",
      rownames = FALSE,
      selection = "single",
      escape = setdiff(colnames(.data), "references"),
      column_specs = COLUMN_SPECS,
      options = list(
        dom = DT_DOM,
        buttons = list(
          list(
            extend = "colvis",
            text = "Additional columns",
            className = "btn-outline-black"
          )
        ),
        columnDefs = list(
          list(
            targets = grep(
              pattern = "^(name|chembl_id|selectivity_class|ontarget_ic50_q1|offtarget_ic50_q1|selectivity|references)$",
              x = names(.data),
              invert = TRUE
            ) - 1,
            visible = FALSE
          ),
          list(
            targets = match("references", names(.data)) - 1L,
            render = selectivity_reference_js[["render_js"]],
            createdCell = selectivity_reference_js[["created_cell_js"]]
          )
        ),
        pagingType = "numbers",
        scrollX = TRUE,
        searchHighlight = TRUE
      )
    ) %>%
      dt_style_selectivity()
  })

  output$output_table <- DT::renderDataTable({
      req(r_table_dt())
      r_table_dt()
    },
    server = TRUE
  )

  r_download_name <- reactive({
    create_download_filename(
      c("compounds", "targeting", r_query_symbol())
    )
  })

  callModule(mod_server_download_button, "output_table_xlsx_dl", r_tbl_data, "excel", r_download_name)
  callModule(mod_server_download_button, "output_table_csv_dl", r_tbl_data, "csv", r_download_name)

  r_selected_compounds_table <- reactive({
    if (is.null(input$output_table_rows_selected))
      NULL
    else
      isolate(r_tbl_data())[
        input$output_table_rows_selected
      ][["lspci_id"]]
  })

  callModule(mod_server_chembl_tabs, "", r_selected_compounds_table)

  #
  #
  # r_eligible_compounds_affinity_tables <- reactive("all")
  #
  # callModule(
  #   mod_server_affinity_tables, "affinity_tables",
  #   r_selected_compounds_table,
  #   data_selectivity, data_tas, data_targets, data_compounds,
  #   r_eligible_lspci_ids = r_eligible_compounds_affinity_tables
  # )

  setBookmarkExclude(
    table_inputs("output_table")
  )

}
