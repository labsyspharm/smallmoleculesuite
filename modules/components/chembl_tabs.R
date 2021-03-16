REPORT_TYPES <- c(
  "Name and structure" = "name_and_classification",
  "Cross-references" = "unichem_cross_refs"
)

make_report_object <- function(type, chembl_id, name) {
  if (is.na(chembl_id))
    tags$p(paste0("No information on ChEMBL for compound ", name, "."))
  else {
    url <- paste0(
      "https://www.ebi.ac.uk/chembl/embed/#compound_report_card/", chembl_id, "/", type
    )
    tags$object(data = url, width = "100%", height = "500")
  }
}

mod_server_chembl_tabs <- function(input, output, session, r_selected_compound) {
  ns <- session$ns

  r_selected_compound_info <- reactive({
    if (is.null(r_selected_compound()))
      NULL
    else
      data_compounds[
        lspci_id == r_selected_compound(),
        .(chembl_id, pref_name)
      ]
  })

  output$chembl_report <- renderUI({
    if (is.null(r_selected_compound_info()))
      tags$p(class = "text-center text-muted", "Select compound for more information.")
    else
      make_report_object(
        type = input$chembl_report_nav,
        chembl_id = r_selected_compound_info()[["chembl_id"]],
        name = r_selected_compound_info()[["pref_name"]]
      )
  })
}

#' UI module to display a tabset of ChEMBL compound information cards
#'
#' @return UI element to display ChEMBL compound
mod_ui_chembl_tabs <- function(id) {
  ns <- NS(id)
  card(
    header = navInput(
      appearance = "tabs",
      id = ns("chembl_report_nav"),
      choices = names(REPORT_TYPES),
      values = REPORT_TYPES,
      class = "card-header-tabs"
    ),
    navContent(
      uiOutput(ns("chembl_report"))
    )
  )
}
