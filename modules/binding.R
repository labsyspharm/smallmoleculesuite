bindingDataUI <- function(id) {
  ns <- NS(id)

  column(
    width = 8,
    style = "margin: auto;",
    formRow(
      id = ns("reference"),
      formGroup(
        label = "Select compounds",
        input = selectizeInput(
          ns("select_compound"),
          label = NULL,
          choices = NULL,
          multiple = TRUE
        ),
        help = "Search for one or more compounds",
        width = "equal"
      ),
      actionButton(
        ns("reset_select_compound"),
        "",
        icon = icon("redo"),
        onclick = glue("$('#{ns('select_compound')}').selectize()[0].selectize.clear();")
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

  r_name_lspci_id_map_filtered <- callModule(
    mod_server_filter_commercial_name_lspci_id_map,
    ""
  )

  first_run <- TRUE

  observeEvent(r_name_lspci_id_map_filtered(), {
    req(
      r_name_lspci_id_map_filtered()
    )
    if (first_run) {
      first_run <<- FALSE
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
      choices <- c(
        if (use_query)
          set_names(query[["lspci_id"]], lspci_id_name_map[[query[["lspci_id"]]]])
        else
          set_names(75376L, "Roscovitine"),
        r_name_lspci_id_map_filtered()
      )
      options <- list(
        selected = if (use_query) query[["lspci_id"]] else "75376",
        server = TRUE,
        options = list(
          placeholder = "Compound name",
          searchField = "label",
          closeAfterSelect = TRUE
        )
      )
    } else {
      choices <- r_name_lspci_id_map_filtered()
      options <- list(selected = NULL)
    }
    exec(
      updateSelectizeInput,
      session,
      "select_compound",
      choices = choices,
      callback = fast_search,
      !!!options
    )
  }, ignoreInit = FALSE, ignoreNULL = TRUE)

  query_show <- use_query

  r_selection <- reactive({
    # Prevent input updates from firing one at a time after
    # updateSelectizeInput call when a query has been sent by the user
    if (
      query_show &&
      !isTRUE(query[["lspci_id"]] == input$select_compound) &&
      !isTRUE(query[["target"]] != input$select_target)
    )
      return(integer())
    query_show <<- FALSE
    selection <- list()
    if(!is.null(input$select_compound))
      selection[["lspci_id"]] <- as.integer(input$select_compound)
    if(!is.null(input$select_target))
      selection[["symbol"]] <- input$select_target
    message(selection)
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
