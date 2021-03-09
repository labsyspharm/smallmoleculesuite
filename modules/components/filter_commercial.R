#' Server module to filter commercially available compounds
#'
#' @return Reactive returning lspci_ids according to current selection
mod_server_filter_commercial <- function(
  input, output, session, compounds
) {
  reactive({
    if (is.null(input$filter_commercial))
      compounds[["lspci_id"]] %>%
        unique()
    else
      compounds[commercially_available == TRUE][["lspci_id"]] %>%
        unique()
  }) %>%
    bindCache(input$filter_commercial)
}

#' UI module to display a switch for commercial availability
#'
#' @param label Label for button
mod_ui_filter_commercial <- function(id) {
  ns <- NS(id)
  switchInput(
    id = ns("filter_commercial"),
    choices = "Include only commercially available compounds. Vendors available in the cross-references at ZINC or eMolecules.",
    values = "only_commercial",
    selected = "only_commercial"
  )
}
