SELECT_COMPOUND_RENDER_JS <- I(
  '{
    option: function(item, escape) {
      const type_map = {
        "emolecules": "<span class=\\"compound-source vendor-source\\">emolecules name</span>",
        "chembl": "<span class=\\"compound-source chembl-source\\"><img alt=\\"ChEMBL logo\\" src=\\"sms/assets/img/chembl_logo.png\\" class=\\"source-logo\\"> ChEMBL</span>",
        "hmsl": "<span class=\\"compound-source hmsl-source\\"><img alt=\\"LINCS logo\\" src=\\"sms/assets/img/lincs_logo.png\\" class=\\"source-logo\\"> HMS LINCS</span>",
      };
      return "<div class=\\"compound-result\\"><span><strong>" + escape(item.name) + "</strong></span>" +
        type_map[escape(item.source)] + "</div>"
    }
  }'
)

SELECT_COMPOUNDS_OPTIONS <- list(
  maxItems = 10,
  maxOptions = 10,
  placeholder = "Compound name",
  loadThrottle = 500,
  searchField = "name",
  valueField = "name_id",
  labelField = "name",
  closeAfterSelect = TRUE,
  render = SELECT_COMPOUND_RENDER_JS
)

#' Server module to select compounds
#'
#' @param compounds Dataframe of compounds, should contain lspci_id
#' @param r_eligible_ids Reactive with vector of eligible lspci_ids
#' @return Filtered dataframe
mod_server_select_compounds <- function(
  input, output, session,
  compounds,
  default_choice = NULL,
  r_eligible_ids = NULL,
  selectize_options = NULL
) {

  r_default_choice <- reactiveVal(default_choice)

  onRestore(function(state) {
    # Have to remove -x suffix
    if (is.null(state$input$select_compound))
      return()
    if (state$input$select_compound[1] == "")
      r_default_choice(NULL)
    else
      r_default_choice(
        str_split_fixed(
          state$input$select_compound,
          fixed("-"),
          n = 2
        )[, 1]
      )
  })

  selectize_options_ <- SELECT_COMPOUNDS_OPTIONS

  for (i in seq_along(selectize_options))
    selectize_options_[[names(selectize_options)[[i]]]] <- selectize_options[[i]]

  r_eligible_compounds <- reactive({
    req(r_eligible_ids())
    data_compound_names[
      lspci_id %in% r_eligible_ids()
    ][
      , .(name, name_id, source)
    ]
  })

  observe({
    req(r_eligible_compounds())
    # Don't react to r_default_choice() because we are only interested in the initial value
    default_choice <- isolate(r_default_choice())
    # Append already selected compounds on update, but don't react to them
    selected <- c(
      if (!is.null(default_choice)) paste0(default_choice, "-1"),
      isolate(input$select_compound)
    )
    updateSelectizeInput(
      session,
      inputId = "select_compound",
      choices = r_eligible_compounds(),
      selected = selected,
      server = TRUE,
      options = selectize_options_,
      callback = fast_search
    )
    # Only use default once upon loading
    r_default_choice(NULL)
  })

  reactive({
    if (is.null(input$select_compound))
      NULL
    else
      unique(as.integer(
        str_split_fixed(input$select_compound, fixed("-"), 2)[, 1]
      ))
  })
}

#' UI module to display a search bar for
#'
#' @param selectize_options List of options passed to selectizeInput
mod_ui_select_compounds <- function(
  id,
  selectize_options = list(label = NULL, choices = NULL, multiple = TRUE, width = "100%")
) {
  ns <- NS(id)
  exec(
    selectizeInput,
    ns("select_compound"),
    !!!selectize_options,
    options = SELECT_COMPOUNDS_OPTIONS
  )
}
