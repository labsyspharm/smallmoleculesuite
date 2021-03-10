SELECT_TARGET_RENDER_JS <- I(
  r'--({
    option: function(item, escape) {
      const type_map = {
        "symbol": "<span class=\"target-source target-source-symbol\">Symbol",
        "gene_id": "<span class=\"target-source target-source-gene-id\">Gene ID",
      };
      const species_map = {
        9606: "Human",
        10116: "Rat",
        10090: "Mouse"
      };
      return `<div class=\"target-result\"><span><strong>${escape(item.name)}</strong></span>` +
        `${type_map[escape(item.type)]} (${species_map[escape(item.tax_id)]})</span></div>`;
    }
  })--'
)

SELECT_TARGET_OPTIONS <- list(
  maxItems = 10,
  maxOptions = 10,
  placeholder = "Select targets",
  loadThrottle = 500,
  searchField = "name",
  closeAfterSelect = TRUE,
  labelField = "name",
  valueField = "lspci_target_id_unique",
  render = SELECT_TARGET_RENDER_JS
)

mod_server_select_targets <- function(
  input, output, session,
  target_map,
  default_choice = NULL,
  r_eligible_targets = NULL,
  selectize_options = NULL
) {

  r_default_choice <- reactiveVal(default_choice)

  onRestore(function(state) {
    # Have to remove -x suffix
    if (is.null(state$input$select_target))
      return()
    if (state$input$select_target[1] == "")
      r_default_choice(NULL)
    else
      r_default_choice(select_target)
  })

  selectize_options_ <- SELECT_TARGET_OPTIONS

  for (i in seq_along(selectize_options))
    selectize_options_[[names(selectize_options)[[i]]]] <- selectize_options[[i]]

  r_target_map_eligible <- reactive({
    req(r_eligible_targets())
    if (r_eligible_targets() == "all")
      target_map
    else
      target_map[
        lspci_target_id %in% r_eligible_targets()
      ]
  })

  observe({
    req(r_target_map_eligible())
    # Don't react to r_default_choice() because we are only interested in the initial value
    default_choice <- isolate(r_default_choice())
    # Append already selected compounds on update, but don't react to them
    selected <- c(
      if (!is.null(default_choice)) paste0(default_choice, "-1"),
      isolate(input$select_target)
    )
    updateSelectizeInput(
      session,
      inputId = "select_target",
      choices = r_target_map_eligible(),
      selected = selected,
      server = TRUE,
      options = selectize_options_,
      callback = fast_search
    )
    r_default_choice(NULL)
  })

  reactive({
    if (is.null(input$select_target))
      NULL
    else
      unique(as.integer(
        str_split_fixed(input$select_target, fixed("-"), 2)[, 1]
      ))
  })
}

mod_ui_select_targets <- function(
  id,
  selectize_options = list(label = NULL, choices = NULL, multiple = TRUE, width = "100%")
) {
  ns <- NS(id)
  exec(
    selectizeInput,
    ns("select_target"),
    !!!selectize_options
  )
}
