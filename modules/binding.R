bindingDataUI <- function(id) {
  ns <- NS(id)

  column(
    width = 8,
    columns(
      column(
        columns(
          column(
            width = 11,
            mod_ui_select_compounds(
              ns("query"),
              selectize_options = list(label = "Select compounds", choices = NULL, multiple = TRUE, width = "100%")
            ) %>%
              margin(b = 0) %>%
              htmltools::tagAppendChild(
                tags$small(
                  class = "text-muted",
                  "Search for one or more compounds"
                )
              )
          ),
          column(
            width = 1,
            class = "m-auto p-0",
            tags$a(
              class = "btn p-0",
              onclick = glue("$('#{NS(ns('query'))('select_compound')}').selectize()[0].selectize.clear();"),
              icon("times-circle")
            )
          )
        )
      ),
      column(
        columns(
          column(
            width = 11,
            selectizeInput(
              ns("select_target"),
              label = "Select targets",
              choices = NULL,
              multiple = TRUE,
              width = "100%"
            ) %>%
              margin(b = 0) %>%
              htmltools::tagAppendChild(
                tags$small(
                  class = "form-text text-muted",
                  "Search for one or more targets"
                )
              )
          ),
          column(
            width = 1,
            class = "m-auto p-0",
            tags$a(
              class = "btn p-0",
              onclick = glue("$('#{ns('select_target')}').selectize()[0].selectize.clear();"),
              icon("times-circle")
            )
          )
        )
      )
    ) %>%
      margin(b = 2),
    columns(
      column(
        formGroup(
          label = "Commercial availability",
          input = mod_ui_filter_commercial(ns(""))
        )
      )
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
    margin("auto") %>%
    columns()
}

bindingDataServer <- function(input, output, session) {
  ns <- session$ns

  r_eligible_lspci_ids <- callModule(mod_server_filter_commercial, "", compounds = data_cmpd_info)

  r_selected_lspci_ids <- callModule(
    mod_server_select_compounds,
    "query",
    data_names,
    default_choice = 66153L,
    r_eligible_ids = r_eligible_lspci_ids,
    selectize_options = list(
      maxItems = 10
    )
  )

  r_default_target <- reactiveVal()

  onRestore(function(state) {
    if (is.null(state$input$select_target))
      return()
    else if (state$input$select_target[1] == "")
      r_default_target(NULL)
    else
      r_default_target(state$input$select_target)
  })

  observe({
    updateSelectizeInput(
      session,
      "select_target",
      choices = data_affinity_selectivity[["symbol"]] %>%
        unique() %>%
        sort(),
      selected = r_default_target(),
      server = FALSE,
      options = list(
        placeholder = "Target symbol",
        closeAfterSelect = TRUE
      )
    )
  })

  r_selection <- reactive({
    selection <- list()
    if(!is.null(r_selected_lspci_ids()))
      selection[["lspci_id"]] <- as.integer(r_selected_lspci_ids())
    if(!is.null(input$select_target))
      selection[["symbol"]] <- input$select_target
    if(length(selection) > 0)
      selection
    else
      integer()
  })

  affinity_tables <- callModule(
    mod_server_affinity_tables,
    "table",
    r_selection,
    data_affinity_selectivity, data_tas, data_gene_info, lspci_id_name_map,
    r_eligible_lspci_ids = r_eligible_lspci_ids
  )
}
