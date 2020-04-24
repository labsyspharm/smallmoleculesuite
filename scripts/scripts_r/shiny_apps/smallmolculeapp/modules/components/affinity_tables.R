#' Server module to display a tabset of tables with affinities and TAS values
#'
#' @param r_selection_drugs Reactive containing lspci_ids of selected compounds
#' @param data_affinity_selectivity Data table with affinities and selectivities
#' @param data_tas Data table with TAS vectors
#' @param data_gene_info Data table with gene information
#' @param lspci_id_name_map Named vector mapping from lspci_id to compound name
mod_server_affinity_tables <- function(
  input, output, session,
  r_selection_drugs,
  data_affinity_selectivity, data_tas, data_gene_info, lspci_id_name_map
) {
  ns <- session$ns

  observeEvent(input$selectivity_nav, {
    showNavPane(ns(paste0("selectivity_nav_", input$selectivity_nav)))
  })

  r_selectivity_data_selected <- reactive({
    drug_id <- r_selection_drugs()

    if (length(drug_id) < 1 || is.na(drug_id))
      return(data_affinity_selectivity[FALSE])

    data_affinity_selectivity[
      lspci_id %in% drug_id
    ][
      data_gene_info[, .(gene_id, symbol)], on = "gene_id", nomatch = NULL
    ][
      order(selectivity_class, affinity_Q1)
    ][
      , c("name", "selectivity") := .(
        lspci_id_name_map[lspci_id],
        round(selectivity, digits = 2)
      )
    ] %>%
      select(
        name,
        symbol,
        selectivity_class,
        affinity_Q1, offtarget_affinity_Q1, selectivity,
        -lspci_id,
        everything()
      )
  })

  r_tas_data_selected <- reactive({
    drug_id <- r_selection_drugs()

    if (length(drug_id) < 1 || is.na(drug_id))
      return(data_tas[FALSE])

    data_tas[
      lspci_id %in% drug_id
    ][
      data_gene_info[, .(gene_id, symbol)], on = "gene_id", nomatch = NULL
    ][
      order(tas)
    ][
      , name := lspci_id_name_map[lspci_id]
    ] %>%
      select(name, symbol, everything(), -lspci_id)
  })

  r_selection_titles <- reactive({
    if (length(r_selection_drugs()) < 1)
      return("Select compound above")
    paste0(
      "Selected: ", paste(lspci_id_name_map[r_selection_drugs()], collapse = "; ")
    )
  })

  output$subtitle_selection <- renderText(r_selection_titles())

  r_selectivity_tbl_data <- callModule(mod_server_reference_modal, "selectivity", r_selectivity_data_selected)

  r_table_selected_selectivity<- reactive({
    download_name <- create_download_filename(
      c("affinity", "spectrum", lspci_id_name_map[r_selection_drugs()])
    )
    .data = r_selectivity_tbl_data()

    DT::datatable(
      data = .data, # input$compound_selection),
      rownames = FALSE,
      escape = grep("^references$", names(.data), invert = TRUE, value = TRUE),
      # fillContainer = TRUE,
      options = list(
        # autoWidth = TRUE,
        extensions = "Buttons",
        buttons = list(
          list(extend = "copy")
        ),
        dom = "tpB",
        language = list(
          emptyTable = if (length(r_selection_drugs()) < 1) {
            "Please select row(s) from the data above."
          } else {
            "No data available"
          }
        ),
        pagingType = "numbers",
        selection = "none",
        scrollX = TRUE
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

  output$table_selectivity <- DT::renderDataTable(r_table_selected_selectivity())

  r_tas_tbl_data <- callModule(mod_server_reference_modal, "tas", r_tas_data_selected)

  r_table_selected_tas <- reactive({
    .data = r_tas_tbl_data()
    download_name <- create_download_filename(
      c("affinity", "spectrum", lspci_id_name_map[r_selection_drugs()])
    )

    DT::datatable(
      data = .data,
      rownames = FALSE,
      escape = grep("^references$", names(.data), invert = TRUE, value = TRUE),
      options = list(
        extensions = "Buttons",
        buttons = list(
          list(extend = "copy")
        ),
        dom = "tpB",
        language = list(
          emptyTable = if (length(r_selection_drugs()) < 1) {
            "Please select row(s) from the data above."
          } else {
            "No data available"
          }
        ),
        selection = "none",
        pagingType = "numbers",
        scrollX = TRUE
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

  output$table_tas <- DT::renderDataTable(r_table_selected_tas())

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

  list(
    r_selected_lspci_ids = r_either_selected
  )
}

#' UI module to display a tabset of compound affinites and TAS values
#'
#' @return UI element to display two tabs with tables
mod_ui_affinity_tables <- function(id) {
  ns <- NS(id)
  card(
    header = tagList(
      h4("Compound affinity and selectivity"),
      p("Showing all available data for selected compounds, ignoring filters") %>%
        margin(b = 1),
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
        div(
          dataTableOutput(
            outputId = ns("table_selectivity"),
            height = "500px"
          )
        )
      ),
      navPane(
        id = ns("selectivity_nav_tas"),
        div(
          dataTableOutput(
            outputId = ns("table_tas"),
            height = "500px"
          )
        )
      )
    )
  )
}
