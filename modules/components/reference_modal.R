library(jsonlite)

REFERENCE_URLS <- list(
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

REFERENCE_RENDER_JS <- r"--(
function(data, type, row, meta) {
  if (type !== "display")
    return(data);
  const reference_urls = `reference_urls`;
  const refs = data.split("|");
  const ref_links = "References;" + refs.map(function(r) {
    const ref_info = r.split(":");
    return("<a href=\"" + reference_urls[ref_info[0]] + ref_info[1] + "\" target=\"blank\">" + r + "</a>");
  }).join("<br>");
  return(ref_links);
}
)--" %>%
  glue(
    reference_urls = toJSON(REFERENCE_URLS, auto_unbox = TRUE),
    .open = "`", .close = "`"
  ) %>%
  JS()

mod_ui_reference_modal <- function(id) {
  ns <- NS(id)

  mod_ui_modal_column(ns(""))
}

mod_server_reference_modal <- function(
  input, output, session
) {
  ns <- session$ns

  list(
    created_cell_js = callModule(
      mod_server_modal_column,
      "",
      button_text = tagList(
        icon("link"),
        " References"
      ) %>%
        as.character()
    ),
    render_js = REFERENCE_RENDER_JS
  )
}
