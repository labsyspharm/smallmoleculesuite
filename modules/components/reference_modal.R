REFERENCE_URLS <- c(
  pubmed_id = "https://pubmed.ncbi.nlm.nih.gov/",
  chembl_id = "https://www.ebi.ac.uk/chembl/document_report_card/",
  patent_id = "https://patents.google.com/?q=",
  synapse_id = "https://www.synapse.org/#!Synapse:",
  doi = "https://dx.doi.org/",
  hms_lincs = "http://lincs.hms.harvard.edu/db/datasets/",
  pubmed = "https://pubmed.ncbi.nlm.nih.gov/",
  chembl = "https://www.ebi.ac.uk/chembl/document_report_card/",
  patent = "https://patents.google.com/?q=",
  synapse = "https://www.synapse.org/#!Synapse:"
)

format_references <- function(references) {
  ref_split <- str_split(references, fixed("|"))[[1]]
  links <- map(
    ref_split,
    function(r) {
      s <- str_split(r, fixed(":"))[[1]]
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

  # References before assignment of links to reference column
  r_raw_references <- reactive({
    r_data()[[reference_col]]
  })

  r_clicked_reference_idx <- reactive({
    req(input$clicked_reference, cancelOutput = TRUE)
    input$clicked_reference %>%
      str_match("reference_link_([0-9]+)$") %>%
      {.[[1, 2]]} %>%
      as.integer()
  })

  o_reference_change <- observeEvent(r_clicked_reference_idx(), {
    req(r_clicked_reference_idx())
    # Need to isolate here, because otherwise if r_data changes
    # the modal is shown again. We only want to show modal if input$clicked_references
    # changes
    clicked_row <- isolate(r_data())[r_clicked_reference_idx(), ]
    shiny::showModal(
      modalDialog(
        format_references(isolate(r_raw_references()[r_clicked_reference_idx()])),
        title = paste(
          "References for", clicked_row[["name"]], "binding to", clicked_row[["symbol"]]
        ),
        easyClose = TRUE
      )
    )
  })

  r_ref_links <- reactive({
    map_chr(
      seq_along(r_raw_references()),
      ~actionLink(
        ns("clicked_reference"),
        "References",
        icon = icon("book-open"),
        onclick = paste0("Shiny.setInputValue('", ns("clicked_reference"), "', this.id, {priority: 'event'});"),
        # Stop selection event in column with references
        onmousedown = "event.stopPropagation();",
        id = paste0("reference_link_", .x)
      ) %>%
        as.character()
    )
  })

  r_data_with_ref <- reactive({
    if(nrow(r_data()) > 0)
      set(r_data(), j = reference_col, value = r_ref_links())
    else
      r_data()
  })
}
