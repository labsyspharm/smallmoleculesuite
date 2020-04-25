bindingDataUI <- function(id) {
  ns <- NS(id)

  columns(
    column(
      width = 6,
      mod_ui_affinity_tables(
        ns("table_by_compound"),
        headers = list(
          h4("Affinity and selectivity by compound"),
          formGroup(
            label = "Select compound",
            input = selectizeInput(
              ns("select_compound"),
              label = NULL,
              choices = NULL,
              multiple = FALSE
            ),
            help = "Search for a compound"
          )
        )
      )
    ),
    column(
      width = 6,
      mod_ui_affinity_tables(
        ns("table_by_target"),
        headers = list(
          h4("Affinity and selectivity by target"),
          formGroup(
            label = "Select target",
            selectizeInput(
              ns("select_target"),
              label = NULL,
              choices = NULL,
              multiple = FALSE
            ),
            help = "Search for a target"
          )
        )
      )
    )
  )
}

bindingDataServer <- function(input, output, session) {

  updateSelectizeInput(
    session,
    "select_compound",
    choices = c(set_names(75376L, "Roscovitine"), name_lspci_id_map),
    selected = "75376",
    server = TRUE,
    options = list(
      placeholder = "Compound name",
      searchField = "label"
    ),
    callback = fast_search
  )

  updateSelectizeInput(
    session,
    "select_target",
    choices = c(
      "ABL1",
      data_affinity_selectivity[["symbol"]] %>%
        unique() %>%
        sort()
    ),
    selected = "ABL1",
    server = TRUE,
    options = list(
      placeholder = "Target symbol"
    )
  )

  r_selection_compound <- reactive({
    list(
      lspci_id = as.integer(input$select_compound)
    )
  })

  callModule(
    mod_server_affinity_tables,
    "table_by_compound",
    r_selection_compound,
    data_affinity_selectivity, data_tas, data_gene_info, lspci_id_name_map
  )

  r_selection_target <- reactive({
    list(
      symbol = input$select_target
    )
  })

  callModule(
    mod_server_affinity_tables,
    "table_by_target",
    r_selection_target,
    data_affinity_selectivity, data_tas, data_gene_info, lspci_id_name_map
  )
}
