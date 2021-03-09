SELECT_TARGET_RENDER_JS <- I(
  '{
    option: function(item, escape) {
      const type_map = {
        "symbol": "<span class=\\"gene-source gene-source-symbol\\">Symbol</span>",
        "gene_id": "<span class=\\"gene-source gene-source-gene-id\\">Gene ID</span>",
      };
      const species_map = {
        9606: "Human",
        10116: "Rat",
        10090: "Mouse"
      };
      return "<div class=\\"gene-result\\"><span><strong>" + escape(item.name) + "</strong></span>" +
        type_map[escape(item.type)] + "</div>"
    }
  }'
)

SELECTIZE_OPTIONS <- list(
  maxItems = 10,
  # maxOptions = 10,
  placeholder = "Target gene",
  loadThrottle = 500,
  searchField = "name",
  closeAfterSelect = TRUE,
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

  selectize_options_ <- SELECTIZE_OPTIONS

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
    updateSelectizeInput(
      session,
      inputId = "select_target",
      choices = r_target_map_eligible(),
      # choices = compounds[
      #   lspci_id %in% isolate(r_eligible_ids()),
      #   .(label = name, value = lspci_id_unique, lspci_id, source)
      # ],
      selected =  paste0(r_default_choice(), "-1"),
      server = TRUE,
      options = selectize_options_,
      callback = fast_search
    )
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
