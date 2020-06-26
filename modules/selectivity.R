
axis_choices <- tribble(
  ~label, ~value, ~type,
  "Affinity Q1 (nM)", "affinity_Q1", "log",
  "Selectivity", "selectivity", "linear",
  "Tool score", "toolscore", "linear",
  "Offtarget Affinity Q1 (nM)", "offtarget_affinity_Q1", "log",
  "Affinity difference (nM)", "affinity_Q1_diff", "linear",
  "Investigation bias", "investigation_bias", "linear",
  "Strength", "strength", "linear",
  "Wilcox p-value", "wilcox_pval", "linear"
) %>%
  arrange(label)

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
              input = selectizeInput(
                ns("query_gene"),
                label = NULL,
                choices = NULL,
                multiple = FALSE
              ),
              help = "Search for a target gene"
            ),
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
      ) %>%
        margin(bottom = 3),
      mod_ui_chembl_tabs(ns("chembl_tabs_1"))
    ),
    column(
      width = 8,
      card(
        header = tagList(
          h4("Target affinity and selectivity plot"),
          htmlOutput(ns("subtitle_plot"), container = p)
        ),
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
            selected = "affinity_Q1"
          ),
          style = "display: flex; align-items: center;"
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
        header = tagList(
          h4("Target affinity and selectivity"),
          htmlOutput(ns("subtitle_data"), container = p)
        ),
        div(
          dataTableOutput(
            outputId = ns("output_table")
          ),
          mod_ui_download_button(ns("output_table_csv_dl"), "Download CSV"),
          mod_ui_download_button(ns("output_table_xlsx_dl"), "Download Excel")
        )
      ) %>%
        margin(bottom = 3),
      mod_ui_affinity_tables(ns("affinity_tables_1"))
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

  # update query gene select ----
  observeEvent(selection_genes(), {
    updateSelectizeInput(
      session,
      "query_gene",
      choices = c("BRAF", selection_genes()),
      selected = if(!is.null(input$query_gene) && str_length(input$query_gene) > 0)
        input$query_gene
      else
        "BRAF",
      server = TRUE
    )
  })

  r_eligible_lspci_ids <- callModule(mod_server_filtered_lspci_ids, "")

  r_binding_data <- reactive({
    req(input$query_gene)

    copy(data_affinity_selectivity)[
      symbol == input$query_gene & lspci_id %in% r_eligible_lspci_ids()
    ][
      ,
      is_filter_match := !is.na(affinity_Q1) &
        affinity_Q1 >= (10**input$affinity[1]) &
        affinity_Q1 <= (10**input$affinity[2]) &
        affinity_N >= input$min_measurements
    ][
      ,
      c("name", "plot_alpha", "selectivity_class") := .(
        lspci_id_name_map[lspci_id],
        if_else(is_filter_match, 0.9, 0.2),
        forcats::fct_rev(selectivity_class)
      )
    ][
      order(is_filter_match, selectivity_class, affinity_Q1)
    ]
  })

  # titles ----
  r_subtitle <- reactive({
    req(input$query_gene)
    paste(
      "Showing compounds binding", input$query_gene, "that meet the filter criteria<br>
      Select compounds here for additional information"
    )
  })

  output$subtitle_plot <- renderText(r_subtitle())
  output$subtitle_data <- renderText(r_subtitle())

  # mainplot ----
  output$mainplot <- renderPlotly({
    matched_data <- r_binding_data()

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
          title = x_axis_vals[["label"]],
          type = x_axis_vals[["type"]]
        ),
        yaxis = list(
          title = y_axis_vals[["label"]],
          type = y_axis_vals[["type"]]
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

  r_selected_compounds <- reactive({
    event_data("plotly_selected", "mainplot")$customdata
  })

  tbl_data <- reactive({
    (if (use_shared_data() && length(r_selected_compounds()) > 0) {
      r_binding_data()[lspci_id %in% r_selected_compounds()]
    } else {
      r_binding_data()
    })[
      is_filter_match == TRUE
    ][
      data_cmpd_info[, .(lspci_id, chembl_id)], on = "lspci_id", nomatch = NULL
    ][
      order(-selectivity_class, affinity_Q1)
    ] %>%
      select(name, chembl_id, symbol, selectivity_class, affinity_Q1, offtarget_affinity_Q1, everything())
  })

  tbl_data_formatted <- callModule(mod_server_reference_modal, "selectivity", tbl_data)

  tbl_table <- reactive({
    .data <- tbl_data_formatted()

    DT::datatable(
      .data,
      extensions = c("Buttons"),
      style = "bootstrap4",
      rownames = FALSE,
      selection = "multiple",
      escape = setdiff(colnames(.data), "references"),
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
              pattern = "^(name|chembl_id|selectivity_class|affinity_Q1|offtarget_affinity_Q1|selectivity|references)$",
              x = names(.data),
              invert = TRUE
            ) - 1,
            visible = FALSE
          )
        ) %>%
          c(column_title_defs(names(.data))),
        pagingType = "numbers",
        scrollX = TRUE,
        searchHighlight = TRUE
      )
    ) %>%
      DT::formatStyle(
        "selectivity_class",
        backgroundColor = DT::styleEqual(
          names(SELECTIVITY_COLORS), SELECTIVITY_COLORS
        ),
        color = DT::styleEqual(
          c("Semi-selective", "Most selective", "Unknown"),
          rep_len("white", 3),
          default = "black"
        )
      )
  })

  output$output_table <- DT::renderDataTable(
    tbl_table(),
    server = FALSE
  )

  r_download_name <- reactive({
    create_download_filename(
      c("compounds", "targeting", input$query_gene)
    )
  })

  callModule(mod_server_download_button, "output_table_xlsx_dl", tbl_data, "excel", r_download_name)
  callModule(mod_server_download_button, "output_table_csv_dl", tbl_data, "csv", r_download_name)

  # table row selection ----
  tbl_selection <- reactive({
    sort(input$output_table_rows_selected)
  })

  r_selection_drugs <- reactive({
    if (is.null(tbl_selection())) {
      return(integer())
    }

    tbl_data()$lspci_id[tbl_selection()]
  })

  o_chembl_tabs <- callModule(
    mod_server_chembl_tabs, "chembl_tabs_1", data_cmpd_info, r_selection_drugs, lspci_id_name_map
  )

  callModule(
    mod_server_affinity_tables, "affinity_tables_1",
    r_selection_drugs,
    data_affinity_selectivity, data_tas, data_gene_info, lspci_id_name_map
  )

}
