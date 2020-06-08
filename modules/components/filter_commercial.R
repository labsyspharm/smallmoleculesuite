#' Server module to filter commercially available compounds
#'
#' @return Reactive returning lspci_ids according to current selection
mod_server_filtered_lspci_ids <- function(
  input, output, session
) {
  reactive({
    if (is.null(input$filter_commercial))
      data_cmpd_info$lspci_id
    else
      data_cmpd_info[commercially_available == TRUE][["lspci_id"]]
  })
}

#' UI module to display a switch for commercial availability
#'
#' @param label Label for button
mod_ui_filter_commercial <- function(id) {
  ns <- NS(id)
  switchInput(
    id = ns("filter_commercial"),
    choices = "Include only commercially available compounds. Check cross-references for ZINC.",
    values = "only_commercial",
    selected = "only_commercial"
  )
}
