#' Server module to filter commercially available compounds
#'
#' @param data Dataframe for filtering, should contain lspci_id
#' @return Filtered dataframe
mod_server_filter_commercial <- function(
  input, output, session,
  data
) {
  if (is.null(input$filter_commercial))
    return(data)
  data[lspci_id %in% commercially_available]
}

#' UI module to display a switch for commercial availability
#'
#' @param label Label for button
mod_ui_filter_commercial <- function(id, label = "Download") {
  ns <- NS(id)
  switchInput(
    id = ns("filter_commercial"),
    choices = "Include only commercially available compounds. Check cross-references for ZINC.",
    values = "only_commercial",
    selected = "only_commercial"
  )
}
