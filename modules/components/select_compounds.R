SELECT_COMPOUND_RENDER_JS <- I(
  '{
    option: function(item, escape) {
      const type_map = {
        "vendor": "<span class=\\"compound_source vendor_source\\">vendor name</span>",
        "chembl_id": "<span class=\\"compound_source chembl_source\\"><img alt=\\"ChEMBL logo\\" src=\\"sms/assets/img/chembl_logo.png\\" class=\\"source_logo\\"> ChEMBL ID</span>",
        "chembl_pref": "<span class=\\"compound_source chembl_source\\"><img alt=\\"ChEMBL logo\\" src=\\"sms/assets/img/chembl_logo.png\\" class=\\"source_logo\\"> ChEMBL primary</span>",
        "chembl_alt": "<span class=\\"compound_source chembl_source\\"><img alt=\\"ChEMBL logo\\" src=\\"sms/assets/img/chembl_logo.png\\" class=\\"source_logo\\"> ChEMBL alternate</span>",
        "hmsl_id": "<span class=\\"compound_source hmsl_source\\"><img alt=\\"LINCS logo\\" src=\\"sms/assets/img/lincs_logo.png\\" class=\\"source_logo\\"> HMS LINCS ID</span>",
        "hmsl_pref": "<span class=\\"compound_source hmsl_source\\"><img alt=\\"LINCS logo\\" src=\\"sms/assets/img/lincs_logo.png\\" class=\\"source_logo\\"> HMS LINCS primary</span>",
        "hmsl_alt": "<span class=\\"compound_source hmsl_source\\"><img alt=\\"LINCS logo\\" src=\\"sms/assets/img/lincs_logo.png\\" class=\\"source_logo\\"> HMS LINCS alternate</span>"
      };
      return "<div class=\\"compound_result\\"><span><strong>" + escape(item.label) + "</strong></span>" +
        type_map[escape(item.source)] + "</div>"
    }
  }'
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

  if (is.null(r_eligible_ids))
    r_eligible_ids <- function() compounds[commercially_available == TRUE][["lspci_id"]]

  selectize_options_ <- list(
    maxItems = 1,
    # maxOptions = 10,
    placeholder = "Compound name",
    loadThrottle = 500,
    searchField = "label",
    closeAfterSelect = TRUE,
    render = SELECT_COMPOUND_RENDER_JS
  )

  onRestore(function(state) {
    # Have to remove -x suffix
    if (is.null(state$input$select_compound))
      return()
    default_choice <- str_split_fixed(
      state$input$select_compound,
      fixed("-"),
      n = 2
    )[, 1]
  })

  for (i in seq_along(selectize_options))
    selectize_options_[[names(selectize_options)[[i]]]] <- selectize_options[[i]]

  r_eligible_compounds <- reactive({
    req(r_eligible_ids())
    compounds[
      lspci_id %chin% r_eligible_ids()
    ][
      , .(label = name, value = lspci_id_unique, lspci_id, source)
    ]
  })

  observe({
    req(r_eligible_compounds())
    # browser()
    updateSelectizeInput(
      session,
      inputId = "select_compound",
      choices = r_eligible_compounds(),
      # choices = compounds[
      #   lspci_id %in% isolate(r_eligible_ids()),
      #   .(label = name, value = lspci_id_unique, lspci_id, source)
      # ],
      # selected = paste0(r_default_choice(), "-1"),
      selected = paste0(default_choice, "-1"),
      server = TRUE,
      options = selectize_options_,
      callback = fast_search
    )
  })

  reactive({
    req(input$select_compound)
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
    !!!selectize_options
  )
}
