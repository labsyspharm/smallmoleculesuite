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
            mod_ui_select_targets(
              ns("target"),
              selectize_options = list(label = "Select target genes", choices = NULL, multiple = TRUE, width = "100%")
            ) %>%
              margin(b = 0) %>%
              htmltools::tagAppendChild(
                tags$small(
                  class = "text-muted",
                  "Search for one or more target genes"
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

  r_eligible_lspci_ids <- callModule(mod_server_filter_commercial, "", compounds = data_compounds)

  r_selected_lspci_ids <- callModule(
    mod_server_select_compounds,
    "query",
    data_compound_names,
    default_choice = 66153L,
    r_eligible_ids = r_eligible_lspci_ids,
    selectize_options = list(
      maxItems = 10
    )
  )

  r_eligible_targets <- reactive({"all"})

  r_selected_targets <- callModule(
    mod_server_select_targets,
    "target",
    data_target_map,
    default_choice = NULL,
    r_eligible_targets = r_eligible_targets
  )

  r_selection <- reactive({
    selection <- list()
    if(!is.null(r_selected_lspci_ids()))
      selection[["lspci_id"]] <- r_selected_lspci_ids()
    if(!is.null(r_selected_targets()))
      selection[["lspci_target_id"]] <- r_selected_targets()
    selection <- if(length(selection) > 0)
      selection
    else
      integer()
    selection
  })

  affinity_tables <- callModule(
    mod_server_affinity_tables,
    "table",
    r_selection,
    data_affinity_selectivity, data_tas, data_gene_info, lspci_id_name_map,
    r_eligible_lspci_ids = r_eligible_lspci_ids
  )
}
