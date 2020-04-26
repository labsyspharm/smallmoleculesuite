bindingDataUI <- function(id) {
  ns <- NS(id)

  column(
    width = 8,
    style = "margin: auto;",
    formRow(
      formGroup(
        label = "Select compounds",
        input = selectizeInput(
          ns("select_compound"),
          label = NULL,
          choices = NULL,
          multiple = TRUE
        ),
        help = "Search for one or more compounds",
        width = "equal"
      ),
      actionButton(
        ns("reset_select_compound"),
        "",
        icon = icon("redo"),
        onclick = glue("$('#{ns('select_compound')}').selectize()[0].selectize.clear();")
      ) %>%
        margin(right = 2),
      formGroup(
        label = "Select targets",
        selectizeInput(
          ns("select_target"),
          label = NULL,
          choices = NULL,
          multiple = TRUE
        ),
        help = "Search for one or more targets",
        width = "equal"
      ) %>%
        margin(left = 2),
      actionButton(
        ns("reset_select_target"),
        "",
        icon = icon("redo"),
        onclick = glue("$('#{ns('select_target')}').selectize()[0].selectize.clear();")
      ),
    ),
    mod_ui_affinity_tables(
      ns("table"),
      headers = list(
        h4("Affinity and selectivity"),
        p("Filter by compound and target") %>%
          margin(b = 1)
      )
    )
  ) %>%
    columns()
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
      searchField = "label",
      closeAfterSelect = TRUE
    ),
    callback = fast_search
  )

  updateSelectizeInput(
    session,
    "select_target",
    choices = data_affinity_selectivity[["symbol"]] %>%
      unique() %>%
      sort(),
    selected = NULL,
    server = TRUE,
    options = list(
      placeholder = "Target symbol",
      closeAfterSelect = TRUE
    )
  )

  r_selection <- reactive({
    selection <- list()
    if(!is.null(input$select_compound))
      selection[["lspci_id"]] <- as.integer(input$select_compound)
    if(!is.null(input$select_target))
      selection[["symbol"]] <- input$select_target
    if(length(selection) > 0)
      selection
    else
      NULL
  })

  callModule(
    mod_server_affinity_tables,
    "table",
    r_selection,
    data_affinity_selectivity, data_tas, data_gene_info, lspci_id_name_map
  )
}
