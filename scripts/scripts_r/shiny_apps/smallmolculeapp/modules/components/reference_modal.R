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
  df, reference_col = "references"
) {
  ns <- session$ns

  if(!nrow(df) > 0)
    return(df)

  references <- df[[reference_col]]

  r_references <- reactive({
    req(input$clicked_reference)
    idx <- input$clicked_reference %>%
      stringr::str_match("reference_link_([0-9]+)$") %>%
      {.[[1, 2]]} %>%
      as.integer()
    references[idx]
  })

  o_reference_change <- observeEvent(r_references(), {
    shiny::showModal(
      modalDialog(
        format_references(r_references()),
        easyClose = TRUE
      )
    )
  })

  ref_links <- map_chr(
    seq_along(references),
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

  df[, references := ref_links]
}
