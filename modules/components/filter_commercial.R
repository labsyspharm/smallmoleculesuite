filter_commercial <- function(filter) {
  if (filter)
    data_compounds[commercially_available == TRUE][["lspci_id"]] %>%
      unique()
  else
    data_compounds[["lspci_id"]] %>%
      unique()
}

mem_filter_commercial <- memoise(filter_commercial)

#' Server module to filter commercially available compounds
#'
#' @return Reactive returning lspci_ids according to current selection
mod_server_filter_commercial <- function(
  input, output, session, compounds
) {
  r_filter_commercial <- reactive({
    req(!is.null(input$filter_commercial))
    if (input$filter_commercial != FALSE)
      TRUE
    else
      FALSE
  }, label = "r_filter_commercial")

  r_eligible_lspci_ids <- reactive({
    mem_filter_commercial(r_filter_commercial())
  }, label = "r_eligible_lspci_ids")

  list(
    r_filter_commercial = r_filter_commercial,
    r_eligible_lspci_ids = r_eligible_lspci_ids
  )
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
