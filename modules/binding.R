bindingDataUI <- function(id) {
  ns <- NS(id)

  column(
    width = 8,
    columns(
      column(
        {
          input <- mod_ui_select_compounds(
            ns("query"),
            selectize_options = list(label = "Filter by compound", choices = NULL, multiple = TRUE)
          )
          reset_button <- div(
            class = "input-group-append",
            tags$button(
              class = "btn btn-outline-secondary",
              type = "button",
              onclick = glue("$('#{NS(ns('query'))('select_compound')}').selectize()[0].selectize.clear();"),
              icon("times-circle")
            )
          )
          input[["children"]][[2]][["attribs"]][["class"]] <- "input-group"
          input[["children"]][[2]] <- htmltools::tagAppendChild(
            input[["children"]][[2]],
            reset_button
          )
          input
        }
      ),
      column(
        {
          input <- mod_ui_select_targets(
            ns("target"),
            selectize_options = list(label = "Filter by target", choices = NULL, multiple = TRUE)
          )
          reset_button <- div(
            class = "input-group-append",
            tags$button(
              class = "btn btn-outline-secondary",
              type = "button",
              onclick = glue("$('#{NS(ns('target'))('select_target')}').selectize()[0].selectize.clear();"),
              icon("times-circle")
            )
          )
          input[["children"]][[2]][["attribs"]][["class"]] <- "input-group"
          input[["children"]][[2]] <- htmltools::tagAppendChild(
            input[["children"]][[2]],
            reset_button
          )
          input
        }
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
      headers = NULL
    )
  ) %>%
    margin("auto") %>%
    columns() %>%
    container()
}

bindingDataServer <- function(input, output, session) {
  ns <- session$ns

  l_filter_commercial <- callModule(mod_server_filter_commercial, "")
  r_eligible_lspci_ids <- l_filter_commercial[["r_eligible_lspci_ids"]]
  r_only_commercial <- l_filter_commercial[["r_only_commercial"]]

  r_selected_lspci_ids <- callModule(
    mod_server_select_compounds,
    "query",
    r_only_commercial,
    default_choice = 66153L,
    selectize_options = list(
      maxItems = 10
    )
  )

  r_eligible_targets <- reactive("all")

  r_selected_targets <- callModule(
    mod_server_select_targets,
    "target",
    data_target_map,
    default_choice = integer(),
    r_eligible_targets = r_eligible_targets
  )

  r_selection <- reactive({
    req(
      !is.null(r_selected_lspci_ids()),
      !is.null(r_selected_targets())
    )
    selection <- list()
    if(length(r_selected_lspci_ids()) > 0)
      selection[["lspci_id"]] <- r_selected_lspci_ids()
    if(length(r_selected_targets()) > 0)
      selection[["lspci_target_id"]] <- r_selected_targets()
    if(length(selection) == 0)
      selection <- integer()
    selection
  })

  affinity_tables <- callModule(
    mod_server_affinity_tables,
    "table",
    r_selection,
    data_selectivity, data_tas, data_targets, data_compounds,
    r_eligible_lspci_ids = function() "all"
  )
}
