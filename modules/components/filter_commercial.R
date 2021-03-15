filter_commercial <- function(only_commercial) {
  if (only_commercial)
    data_compounds[commercially_available == TRUE][["lspci_id"]] %>%
      unique()
  else
    data_compounds[["lspci_id"]] %>%
      unique()
}
filter_commercial <- memoise(filter_commercial)

#' Server module to filter commercially available compounds
#'
#' @return Reactive returning lspci_ids according to current selection
mod_server_filter_commercial <- function(
  input, output, session
) {
  r_only_commercial <- reactive({
    req(!is.null(input$filter_commercial))
    if (input$filter_commercial != FALSE)
      TRUE
    else
      FALSE
  }, label = "r_only_commercial")

  r_eligible_lspci_ids <- reactive({
    filter_commercial(r_only_commercial())
  }, label = "r_eligible_lspci_ids")

  list(
    r_only_commercial = r_only_commercial,
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
