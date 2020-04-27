REPORT_TYPES <- c(
  "Name and structure" = "name_and_classification",
  "Cross-references" = "unichem_cross_refs"
)

#' Server module to display a tabset of ChEMBL compound information cards
#'
#' @param dt_cmpd_info Data table with lspci_id and chembl_id
#' @param r_selected_ids Reactive which gives selected lspci_ids
#' @param name_map Mapping from lspci_id to compound
#'
#' @return List of observers that automatically updates the ChEMBL tabs if selection
#'   changes
mod_server_chembl_tabs <- function(input, output, session, dt_cmpd_info, r_selected_ids, name_map) {
  ns <- session$ns
  user_data <- session$userData
  tabs_open_accession <- ns("chembl_tabs")

  o_choices <- observe({
    selected_lspci_ids <- r_selected_ids()
    all_chembl_tabs <- user_data[[tabs_open_accession]]

    ids <- dt_cmpd_info[
      lspci_id %in% selected_lspci_ids,
      .(
        chembl_id,
        name = name_map[lspci_id]
      )
    ][
      order(name)
    ]

    new_ids <- setdiff(ids[["chembl_id"]], all_chembl_tabs)
    stale_ids <- setdiff(all_chembl_tabs, ids[["chembl_id"]])

    ids[, new_id := chembl_id %in% new_ids]

    # Remove any tabs that are no longer selected
    walk(
      stale_ids,
      function (chembl_id) {
        walk(
          REPORT_TYPES,
          ~removeUI(paste0("#", ns(paste0("chembl_tab_", chembl_id, "_", .x))))
        )
      }
    )

    make_report_object <- function(type, chembl_id, name) {
      obj <- if (is.na(chembl_id))
        tags$p(paste0("No information on ChEMBL for compound ", name, "."))
      else {
        url <- paste0(
          "https://www.ebi.ac.uk/chembl/embed/#compound_report_card/", chembl_id, "/", type
        )
        tags$object(data = url, width = "100%", height = "500")
      }
      navPane(
        id = ns(paste0("chembl_tab_", chembl_id, "_", type)),
        obj
      )
    }

    chembl_tabs <- pmap(
      ids[new_id == TRUE],
      function(chembl_id, name, ...) {
        map(
          REPORT_TYPES,
          make_report_object, chembl_id, name
        )
      }
    )

    insertUI(
      selector = paste0("#", ns("chembl_tab_parent")),
      where = "beforeEnd",
      ui = exec(tagList, !!!unlist(chembl_tabs, recursive = FALSE, use.names = FALSE))
    )

    updateNavInput(
      id = "chembl_nav",
      choices = ids[["name"]],
      values = ids[["chembl_id"]],
      selected = if (nrow(ids) > 0)
        ids[order(-new_id, name)][[1, "chembl_id"]]
      else
        NULL
    )

    if (length(selected_lspci_ids) < 1)
      showNavPane(ns("chembl_tab_empty"))

    user_data[[tabs_open_accession]] <- ids[["chembl_id"]]
  })

  o_switch <- observeEvent({
      input$chembl_nav
      input$chembl_report_nav
    }, {
    if (is.null(input$chembl_nav) || is.null(input$chembl_report_nav))
      return()
    showNavPane(ns(paste0("chembl_tab_", input$chembl_nav, "_", input$chembl_report_nav)))
    selected_chembl_tab <- input$chembl_nav
  })
}

#' UI module to display a tabset of ChEMBL compound information cards
#'
#' @return UI element to display ChEMBL compound
mod_ui_chembl_tabs <- function(id) {
  ns <- NS(id)
  card(
    header = tagList(
      h4("ChEMBL compound report"),
      navInput(
        appearance = "pills",
        id = ns("chembl_nav"),
        choices = NULL,
        values = NULL
      ) %>%
        margin(b = 3),
      navInput(
        appearance = "tabs",
        id = ns("chembl_report_nav"),
        choices = names(REPORT_TYPES),
        values = REPORT_TYPES,
        class = "card-header-tabs"
      )
    ),
    navContent(
      id = ns("chembl_tab_parent"),
      navPane(
        id = ns("chembl_tab_empty"),
        p(
          "No compound selected."
        )
      ),

    )
  )
}
