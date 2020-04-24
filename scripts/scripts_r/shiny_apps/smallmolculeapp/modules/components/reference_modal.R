REFERENCE_URLS <- c(
  pubmed = "https://pubmed.ncbi.nlm.nih.gov/",
  chembl = "https://www.ebi.ac.uk/chembl/document_report_card/",
  patent = "https://patents.google.com/?q=",
  synapse = "https://www.synapse.org/#!Synapse:",
  doi = "https://dx.doi.org/",
  hmsl = "http://lincs.hms.harvard.edu/db/datasets/"
)

format_references <- function(references) {
  ref_split <- stringr::str_split(references, stringr::fixed("|"))[[1]]
  links <- map(
    ref_split,
    function(r) {
      s <- stringr::str_split(r, stringr::fixed(":"))[[1]]
      tags$p(tags$a(r, href = paste0(REFERENCE_URLS[s[1]], s[2]), target = "_blank"))
    }
  )
  exec(div, !!!links)
}

mod_server_reference_modal <- function(
  input, output, session,
  r_data, reference_col = "references"
) {
  ns <- session$ns

  r_references <- reactive({
    r_data()[[reference_col]]
  })

  r_clicked_references <- reactive({
    req(input$clicked_reference)
    idx <- input$clicked_reference %>%
      stringr::str_match("reference_link_([0-9]+)$") %>%
      {.[[1, 2]]} %>%
      as.integer()
    r_references()[idx]
  })

  o_reference_change <- observeEvent(r_clicked_references(), {
    req(r_clicked_references())
    shiny::showModal(
      modalDialog(
        format_references(r_clicked_references()),
        easyClose = TRUE
      )
    )
  })

  r_ref_links <- reactive({
    map_chr(
      seq_along(r_references()),
      ~actionLink(
        ns("clicked_reference"),
        "References",
        icon = icon("book-open"),
        onclick = paste0("Shiny.setInputValue('", ns("clicked_reference"), "', this.id, {priority: 'event'});"),
        onmousedown = "event.stopPropagation();",
        id = paste0("reference_link_", .x)
      ) %>%
        as.character()
    )
  })

  reactive({
    if(nrow(r_data()) > 0)
      set(r_data(), j = reference_col, value = r_ref_links())
    else
      r_data()
  })
}
