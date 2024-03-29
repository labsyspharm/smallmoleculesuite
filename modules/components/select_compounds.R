SELECT_COMPOUND_RENDER_JS <- I(
  r'--({
    option: function(item, escape) {
      const type_map = {
        "emolecules": "<span class=\"compound-source vendor-source\"> Emolecules</span>",
        "chembl": "<span class=\"compound-source chembl-source\"><img alt=\"ChEMBL logo\" src=\"sms/assets/img/chembl_logo.png\" class=\"source-logo\"> ChEMBL</span>",
        "hmsl": "<span class=\"compound-source hmsl-source\"><img alt=\"LINCS logo\" src=\"sms/assets/img/lincs_logo.png\" class=\"source-logo\"> HMS LINCS</span>",
        "emolecules_id": "<span class=\"compound-source vendor-source\"> Emolecules ID</span>",
        "chembl_id": "<span class=\"compound-source chembl-source\"><img alt=\"ChEMBL logo\" src=\"sms/assets/img/chembl_logo.png\" class=\"source-logo\"> ChEMBL ID</span>",
        "hmsl_id": "<span class=\"compound-source hmsl-source\"><img alt=\"LINCS logo\" src=\"sms/assets/img/lincs_logo.png\" class=\"source-logo\"> HMS LINCS ID</span>",
        "lspci_id": "<span class=\"compound-source\"> SMS ID</span>",
      };
      return `<div class="compound-result"><span><strong>${escape(item.name)}</strong></span>
        ${type_map[escape(item.source)]}</div>`.replace(
          /\{\{([0-9]+)\}\}/g, " <span class=\"text-muted\">(#$1)</span>"
        );
    },
    item: function(item, escape) {
      return `<div class="item">${item.name}</div>`.replace(
        /\{\{([0-9]+)\}\}/g, " <span class=\"text-muted\">(#$1)</span>"
      );
    }
  })--'
)

SELECT_COMPOUNDS_OPTIONS <- list(
  maxItems = 10,
  maxOptions = 10,
  placeholder = "Select compounds",
  loadThrottle = 500,
  searchField = "name",
  valueField = "name_id",
  labelField = "name",
  closeAfterSelect = TRUE,
  render = SELECT_COMPOUND_RENDER_JS
)

strip_compound_suffix <- function(x) {
  unique(as.integer(
    str_split_fixed(x, fixed("-"), 2)[, 1]
  ))
}

eligible_compounds <- function(only_commercial) {
  if (only_commercial)
    data_compound_names[
      lspci_id %in% filter_commercial(TRUE)
    ][
      , .(name, name_id, source)
    ]
  else
    data_compound_names[
      , .(name, name_id, source)
    ]
}
eligible_compounds <- memoise(eligible_compounds)

#' Server module to select compounds
#'
#' @param compounds Dataframe of compounds, should contain lspci_id
#' @param r_eligible_ids Reactive with vector of eligible lspci_ids
#' @return Filtered dataframe
mod_server_select_compounds <- function(
  input, output, session,
  r_commercial_only,
  default_choice = integer(),
  selectize_options = NULL
) {

  r_default_choice <- reactiveVal(default_choice)

  onRestore(function(state) {
    # Have to remove -x suffix
    val <- state$input$select_compound
    if (is.null(val))
      NULL
    else if (val[1] == "")
      r_default_choice(integer())
    else
      r_default_choice(strip_target_suffix(val))
  })

  selectize_options_ <- SELECT_COMPOUNDS_OPTIONS

  for (i in seq_along(selectize_options))
    selectize_options_[[names(selectize_options)[[i]]]] <- selectize_options[[i]]

  r_eligible_compounds <- reactive({
    req(!is.null(r_commercial_only()))
    eligible_compounds(r_commercial_only())
  })

  observeEvent(r_eligible_compounds(), {
    req(r_eligible_compounds(), !is.null(r_default_choice()))
    # Append already selected compounds on update, but don't react to them
    selected <- c(
      if (length(r_default_choice()) > 0) paste0(r_default_choice(), "-1"),
      if (!is.null(input$select_compound) && input$select_compound != "") input$select_compound
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
    r_default_choice(integer())
  })

  reactive({
    if (is.null(input$select_compound) || input$select_compound[1] == "")
      integer()
    else
      strip_compound_suffix(input$select_compound)
  }, label = "r_selected_compounds")
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
