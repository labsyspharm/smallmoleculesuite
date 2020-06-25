bindingDataUI <- function(id) {
  ns <- NS(id)

  column(
    width = 8,
    style = "margin: auto;",
    formRow(
      id = ns("reference"),
      formGroup(
        label = "Select compounds",
        input = mod_ui_select_compounds(ns("query")),
        help = "Search for one or more compounds",
        width = "equal"
      ),
      actionButton(
        ns("reset_select_compound"),
        "",
        icon = icon("redo"),
        onclick = glue("$('#{NS(ns('query'))('select_compound')}').selectize()[0].selectize.clear();")
      ) %>%
        margin(right = 2),
      formGroup(
        label = "Select targets",
        selectizeInput(
          ns("select_target"),
          label = NULL,
          choices = NULL,
          multiple = TRUE
        ),
        help = "Search for one or more targets",
        width = "equal"
      ) %>%
        margin(left = 2),
      actionButton(
        ns("reset_select_target"),
        "",
        icon = icon("redo"),
        onclick = glue("$('#{ns('select_target')}').selectize()[0].selectize.clear();")
      )
    ),
    formRow(
      formGroup(
        label = "Commercial availability",
        input = mod_ui_filter_commercial(ns(""))
      )
    ),
    mod_ui_affinity_tables(
      ns("table"),
      headers = list(
        h4("Affinity and selectivity"),
        p("Filter by compound and target") %>%
          margin(b = 1)
      )
    ),
    tags$head(
      tags$script(
        I("
          Shiny.addCustomMessageHandler('scrollCallback',
            function(msg) {
              let target = $('#' + msg);
              let offset = target.offset();
              $('html, body').animate({scrollTop: offset.top}, 'slow');
            }
          );
        ")
      )
    )
  ) %>%
    columns()
}

bindingDataServer <- function(input, output, session) {
  ns <- session$ns

  query <- isolate(
    parseQueryString(session$clientData$url_search)
  )

  use_query <- !is.null(query[["tool"]]) && query[["tool"]] == "reference"

  r_eligible_lspci_ids <- callModule(
    mod_server_filtered_lspci_ids,
    ""
  )

  r_selected_lspci_ids <- callModule(
    mod_server_select_compounds,
    "query",
    data_names,
    default_choice = if (use_query) query[["lspci_id"]] else 66153L,
    r_eligible_ids = r_eligible_lspci_ids,
    selectize_options = list(
      maxItems = 10
    )
  )

  updateSelectizeInput(
    session,
    "select_target",
    choices = data_affinity_selectivity[["symbol"]] %>%
      unique() %>%
      sort(),
    selected = if (use_query) query[["symbol"]],
    server = TRUE,
    options = list(
      placeholder = "Target symbol",
      closeAfterSelect = TRUE
    )
  )

  query_show <- use_query

  r_selection <- reactive({
    # Prevent input updates from firing one at a time after
    # updateSelectizeInput call when a query has been sent by the user
    if (
      query_show &&
      (
        !(is.null(query[["lspci_id"]]) || isTRUE(query[["lspci_id"]] == r_selected_lspci_ids())) ||
        !(is.null(query[["symbol"]]) || isTRUE(query[["symbol"]] == input$select_target))
      )
    )
      return(integer())
    query_show <<- FALSE
    selection <- list()
    if(!is.null(r_selected_lspci_ids()))
      selection[["lspci_id"]] <- as.integer(r_selected_lspci_ids())
    if(!is.null(input$select_target))
      selection[["symbol"]] <- input$select_target
    if(length(selection) > 0)
      selection
    else
      integer()
  })

  affinity_tables <- callModule(
    mod_server_affinity_tables,
    "table",
    r_selection,
    data_affinity_selectivity, data_tas, data_gene_info, lspci_id_name_map
  )

  if (use_query) {
    updateNavInput(
      id = "selectivity_nav",
      selected = query[["tab"]],
      session = affinity_tables[["session"]]
    )
    session$sendCustomMessage(type = "scrollCallback", ns("reference"))
  }

}

mod_server_scroll_binding <- function(
  input, output, session
) {
  ns <- session$ns
  session$sendCustomMessage(type = "scrollCallback", ns("reference"))
}
