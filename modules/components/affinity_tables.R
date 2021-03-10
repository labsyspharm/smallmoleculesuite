subset_dt <- function(dt, selectors) {
  sel <- if (length(selectors) == 0) {
    FALSE
  } else if (!is.list(selectors)) {
    # assume it's drug ids
    dt[["lspci_id"]] %in% selectors
  } else {
    # List with selectors
    selectors %>%
      map(~if(is.null(.x) || length(.x) == 0) TRUE else .x) %>%
      imap(~dt[[.y]] %in% .x) %>%
      reduce(magrittr::and)
  }
  dt[sel]
}

#' Server module to display a tabset of tables with affinities and TAS values
#'
#' If r_selection_drugs is NULL, nothing will be filtered, everything will be
#' displayed. If r_selection is an empty list or vector, everything will be filtered
#' and nothing will be displayed
#'
#' @param r_selection_drugs Reactive containing lspci_ids of selected compounds
#' @param data_affinity_selectivity Data table with affinities and selectivities
#' @param data_tas Data table with TAS vectors
#' @param data_gene_info Data table with gene information
#' @param lspci_id_name_map Named vector mapping from lspci_id to compound name
#' @param selection Forwarded to DT::datatable
mod_server_affinity_tables <- function(
  input, output, session,
  r_selection_drugs,
  data_affinity_selectivity, data_tas, data_gene_info, lspci_id_name_map, selection = "none",
  r_eligible_lspci_ids = NULL
) {
  ns <- session$ns

  observeEvent(input$selectivity_nav, {
    showNavPane(ns(paste0("selectivity_nav_", input$selectivity_nav)))
  })

  r_selectivity_data_selected <- reactive({
    subset_dt(data_selectivity, r_selection_drugs())[
      if (is.null(r_eligible_lspci_ids)) TRUE else lspci_id %in% r_eligible_lspci_ids()
    ][
      data_compounds[, .(lspci_id, chembl_id, name = pref_name)], on = "lspci_id", nomatch = NULL
    ][
      data_targets[, .(lspci_target_id, symbol, gene_id)], on = "lspci_target_id", nomatch = NULL
    ][
      order(selectivity_class, ontarget_ic50_q1)
    ][
      , selectivity := round(selectivity, digits = 2)
    ] %>%
      select(
        name,
        chembl_id,
        symbol,
        selectivity_class,
        ontarget_ic50_q1, offtarget_ic50_q1, selectivity,
        everything()
      )
  })

  r_tas_data_selected <- reactive({
    subset_dt(data_tas, r_selection_drugs())[
      if (is.null(r_eligible_lspci_ids)) TRUE else lspci_id %in% r_eligible_lspci_ids()
    ][
      data_compounds[, .(lspci_id, chembl_id, name = pref_name)], on = "lspci_id", nomatch = NULL
    ][
      data_targets[, .(lspci_target_id, symbol, gene_id)], on = "lspci_target_id", nomatch = NULL
    ][
      order(tas, measurement)
    ] %>%
      select(name, chembl_id, symbol, everything())
  })

  r_selection_titles <- reactive({
    if(is.list(r_selection_drugs()))
      return("")
    if (length(r_selection_drugs()) < 1)
      return("Select compound above")
    paste0(
      "Selected: ", paste(lspci_id_name_map[r_selection_drugs()], collapse = "; ")
    )
  })

  r_download_name <- reactive({
    if(is.list(r_selection_drugs()))
      return(create_download_filename(c("affinity", "spectrum")))
    create_download_filename(
      c("affinity", "spectrum", data_compounds[lspci_id %in%r_selection_drugs()][["pref_name"]])
    )
  })

  output$subtitle_selection <- renderText(r_selection_titles())

  selectivity_reference_js <- callModule(mod_server_reference_modal, "selectivity")

  r_table_selected_selectivity<- reactive({
    .data <- r_selectivity_data_selected()

    datatable_tooltip(
      data = .data, # input$compound_selection),
      rownames = FALSE,
      escape = grep("^references$", names(.data), invert = TRUE, value = TRUE),
      selection = selection,
      style = "bootstrap4",
      extensions = "Buttons",
      column_specs = COLUMN_SPECS,
      options = list(
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
              x = names(.data),
              pattern = "^(name|symbol|selectivity_class|ontarget_ic50_q1|offtarget_ic50_q1|references)$",
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
        dom = DT_DOM,
        language = list(
          emptyTable = if (length(r_selection_drugs()) < 1) {
            "Please select row(s) from the data above."
          } else {
            "No data available"
          }
        ),
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

  output$table_selectivity <- DT::renderDataTable(
    r_table_selected_selectivity(),
    server = TRUE
  )

  callModule(mod_server_download_button, "selectivity_xlsx_dl", r_selectivity_data_selected, "excel", r_download_name)
  callModule(mod_server_download_button, "selectivity_csv_dl", r_selectivity_data_selected, "csv", r_download_name)

  selectivity_reference_js <- callModule(mod_server_reference_modal, "tas")

  r_table_selected_tas <- reactive({
    .data = r_tas_data_selected()

    datatable_tooltip(
      data = .data,
      rownames = FALSE,
      escape = grep("^references$", names(.data), invert = TRUE, value = TRUE),
      selection = selection,
      extensions = c("Buttons"),
      style = "bootstrap4",
      column_specs = COLUMN_SPECS,
      options = list(
        buttons = list(
          list(
            extend = "colvis",
            text = "Additional columns",
            className = "btn-outline-black"
          )
        ),
        dom = DT_DOM,
        language = list(
          emptyTable = if (length(r_selection_drugs()) < 1) {
            "Please select row(s) from the data above."
          } else {
            "No data available"
          }
        ),
        pagingType = "numbers",
        scrollX = TRUE,
        searchHighlight = TRUE,
        columnDefs = list(
          list(
            targets = grep(
              x = names(.data),
              pattern = "^(name|symbol|tas|derived_from|measurement|unit|references)$",
              invert = TRUE
            ) - 1,
            visible = FALSE
          ),
          list(
            targets = match("references", names(.data)) - 1L,
            render = selectivity_reference_js[["render_js"]],
            createdCell = selectivity_reference_js[["created_cell_js"]]
          )
        )
      )
    ) %>%
      DT::formatStyle(
        "tas",
        backgroundColor = DT::styleInterval(
          c(1, 2, 3), TAS_COLORS
        ),
        color = DT::styleInterval(1, c("white", "inherit"))
      )
  })

  output$table_tas <- DT::renderDataTable(
    r_table_selected_tas(),
    server = TRUE
  )

  callModule(mod_server_download_button, "tas_xlsx_dl", r_tas_data_selected, "excel", r_download_name)
  callModule(mod_server_download_button, "tas_csv_dl", r_tas_data_selected, "csv", r_download_name)

  r_either_selected <- reactiveVal()

  observeEvent(input$table_tas_rows_selected, {
    r_either_selected(
      r_tas_data_selected()[["lspci_id"]][sort(input$table_tas_rows_selected)]
    )
  })

  observeEvent(input$table_selectivity_rows_selected, {
    r_either_selected(
      r_selectivity_data_selected()[["lspci_id"]][sort(input$table_selectivity_rows_selected)]
    )
  })

  setBookmarkExclude(
    table_inputs(
      c(
        "table_selectivity",
        "table_tas"
      )
    )
  )

  list(
    session = session,
    r_selected_lspci_ids = r_either_selected
  )
}

#' UI module to display a tabset of compound affinites and TAS values
#'
#' @return UI element to display two tabs with tables
mod_ui_affinity_tables <- function(
  id,
  headers = list(
    h4("Compound affinity and selectivity"),
    p("Showing all available data for selected compounds, ignoring filters") %>%
      margin(b = 1)
  )
) {
  ns <- NS(id)
  card(
    header = exec(
      tagList,
      !!!headers,
      textOutput(ns("subtitle_selection"), p),
      navInput(
        appearance = "tabs",
        id = ns("selectivity_nav"),
        choices = c("Selectivity", "Target Affinity Spectrum"),
        values = c("selectivity", "tas"),
        selected = "selectivity",
        class = "card-header-tabs"
      )
    ),
    navContent(
      navPane(
        id = ns("selectivity_nav_selectivity"),
        tagList(
          dataTableOutput(
            outputId = ns("table_selectivity")
          ) %>%
            shinycssloaders::withSpinner(color = "#303030"),
          mod_ui_download_button(ns("selectivity_xlsx_dl"), "Download Excel"),
          mod_ui_download_button(ns("selectivity_csv_dl"), "Download CSV")
        )
      ),
      navPane(
        id = ns("selectivity_nav_tas"),
        tagList(
          p(
            "Target Affinity Spectrum (TAS) values are binding affinity assertions that aggregate",
            "compound binding data from heterogeneous sources, such as full",
            "dose-response affinity measurements, single dose binding assays",
            "and binding assertions from the literature."
          ),
          p(
            "The binding assertions are 1, 2 and 3, with 1 representing the strongest",
            "and 3 the weakest binding. 10 represents confirmed non-binding."
          ),
          p(
            "See", a("the publication", href = "https://doi.org/10.1016/j.chembiol.2019.02.018", target = "_blank"),
            "for details."
          ),
          dataTableOutput(
            outputId = ns("table_tas")
          ) %>%
            shinycssloaders::withSpinner(color = "#303030"),
          mod_ui_download_button(ns("tas_xlsx_dl"), "Download Excel"),
          mod_ui_download_button(ns("tas_csv_dl"), "Download CSV")
        )
      )
    )
  ) %>%
    tagList(
      mod_ui_reference_modal(ns("selectivity")),
      mod_ui_reference_modal(ns("tas"))
    )
}
