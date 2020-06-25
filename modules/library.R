libraryUI <- function(id) {
  ns <- NS(id)

  columns(
    column(
      width = 4,
      card(
        header = navInput(
          appearance = "tabs",
          id = ns("nav"),
          choices = c("Filters", "Instructions"),
          values = c("filters", "instructions"),
          selected = "filters"
        ),
        navContent(
          navPane(
            id = ns("pane_filters"),
            p(
              "Type or paste gene symbols in the text box below to generate a downloadable table of drugs targetting those genes.",
              "One gene per line."
            ),
            formInput(
              id = ns("gene_form"),
              formGroup(
                label = "Find ligands for gene symbols",
                input = shiny::textAreaInput(
                  inputId = ns("gene_list"),
                  label = NULL,
                  rows = 5
                ),
                help = div(
                  "This tool uses HUGO symbols Please see",
                  tags$a(
                    target = "_blank", href = "https://genenames.org",
                    "genenames.org"
                  ),
                  "for help."
                )
              ),
              formGroup(
                label = "Example gene lists",
                input = {
                  sel <- selectInput(
                    id = ns("gene_example"),
                    choices = names(data_genes), # data/load.R
                    selected = "Dark_Kinome"
                  )
                  sel$children[[1]]$attribs$placeholder <- "Dark_Kinome"
                  sel
                },
                help = "Selecting a choice will populate the input above with an example list of genes."
              ),
              formSubmit(
                ns("submit"),
                label = "Submit"
              ) %>%
                background("orange"),
              actionButton(
                ns("reset_gene_list"),
                "Clear list",
                icon = icon("redo"),
                onclick = glue("$('#{ns('gene_list')}')[0].value = null;")
              ) %>%
                background("orange")
            ) %>%
              margin(bottom = 3),
            navContent(
              navPane(
                id = ns("pane_results"),
                p(
                  shiny::textOutput(
                    outputId = ns("gene_targets"),
                    inline = TRUE
                  ),
                  linkInput(
                    id = ns("gene_unknowns"),
                    label = shiny::icon("exclamation-circle")
                  ) %>%
                    font(color = "orange")
                ) %>%
                  display("flex") %>%
                  flex(justify = "between", align = "center") %>%
                  margin(top = 4, bottom = 4) %>%
                  font(size = "lg"),
                formGroup(
                  label = "Commercial availability",
                  input = div(
                    class = "active--orange",
                    mod_ui_filter_commercial(ns(""))
                  )
                ),
                formGroup(
                  label = tags$h6("Selectivity levels") %>% margin(b = 0),
                  input = checkboxInput(
                    inline = TRUE,
                    id = ns("filter_probes"),
                    choices = c("Most selective", "Semi-selective", "Poly-selective", "Unknown"),
                    # values = c("most_selective", "semi_selective", "poly_selective", "unknown_selective"),
                    selected = "Most selective"
                  ) %>%
                    active("orange"),
                  help = "Choose the selectivity levels for which you want chemical probes to be included in the library."
                ),
                formGroup(
                  label = tags$h6("Clinical phases") %>% margin(b = 0),
                  input = checkboxInput(
                    inline = TRUE,
                    id = ns("filter_phase"),
                    choices = c("Approved", "Phase III", "Phase II", "Phase I"),
                    values = c(4, 3, 2, 1),
                    selected = 4
                  ) %>%
                    active("orange"),
                  help = "Select compounds in clinical development to be added to the library."
                ),
                formGroup(
                  label = tags$h6("Expert opinion compounds") %>% margin(b = 0),
                  input = checkboxInput(
                    inline = TRUE,
                    id = ns("filter_expert"),
                    choices = "chemicalprobes.org 4.0 star rating",
                    values = "chem_probe"
                  ) %>%
                    active("orange"),
                  help = "Select compounds that are endorsed by other users to be added to the library."
                ),
                formGroup(
                  label = tags$h6("Output table") %>% margin(b = 0),
                  input = radiobarInput(
                    id = ns("table_display"),
                    choices = c("Display per entry", "Display per compound"),
                    values = c("entry", "compound"),
                    selected = "entry"
                  ) %>%
                    active("orange")
                ),
                formGroup(
                  label = tags$h6("Minimum affinity for query target (nM)") %>% margin(b = 0),
                  div(
                    class = "logify-slider active--orange",
                    shiny::sliderInput(
                      inputId = ns("filter_affinity"),
                      label = NULL,
                      min = 0,
                      max = 14,
                      step = 1,
                      value = 8
                    )
                  )
                ),
                formGroup(
                  label = tags$h6("Minimum number of affinity measurements") %>% margin(b = 0),
                  div(
                    class = "active--orange",
                    shiny::sliderInput(
                      inputId = ns("filter_measurement"),
                      label = NULL,
                      min = 1,
                      max = 40,
                      value = 2
                    )
                  )
                )
              )
            )
          ),
          navPane(
            id = ns("pane_instructions"),
            p("The Library app helps you build custom small molecule libraries"),
            p("To use the Library app:"),
            tags$ol(
              class = "pl-4",
              tags$li(
                "Submit a list of targets that you want to build the library for (in HUGO nomenclature), or select one of the pre-selected gene lists."
              ),
              tags$li(
                "Select up to which selectivity level you want to be included."
              ),
              tags$li(
                "Select which approval phases you want to include for clinical compounds."
              ),
              tags$li(
                "Select whether to include the compounds from chemicalprobes.org (4.0 star rating only)."
              ),
              tags$li(
                "Choose whether to view the table per target or per compound"
              ),
              tags$li(
                "Download the library."
              )
            )
          )
        )
      )
    ),
    column(
      width = 8,
      card(
        h3("Output table"),
        div(
          DT::dataTableOutput(
            outputId = ns("table_results"),
            height = "625px"
          ),
          mod_ui_download_button(ns("output_table_csv_dl"), "Download CSV"),
          mod_ui_download_button(ns("output_table_xlsx_dl"), "Download Excel")
        )
      ) %>%
        margin(b = 3),
      mod_ui_chembl_tabs(ns("chembl_tabs_1"))
    )
  )
}

mod_ui_set_library_vals_button <- function(id, label, button_icon = icon("braille")) {
  ns <- NS(id)
  actionButton(
    ns("button"),
    label,
    icon = button_icon
  )
}

mod_server_set_library_vals_button <- function(
  input, output, session, library_session, vals, finish_callback = NULL
) {
  observeEvent(input$button, {
    for (field in names(vals)) {
      switch(
        field,
        gene_example = updateSelectInput(
          id = "gene_example", selected = vals[[field]],
          session = library_session
        )
      )
    }
    if (!is.null(finish_callback))
      finish_callback()
  })
}

libraryServer <- function(input, output, session, load_example) {
  ns <- session$ns

  # Define genes found in our data
  liganded_genes <- unique(
    data_optimal_compounds$symbol,
    data_chemical_probes$symbol
  )

  # nav ----
  observeEvent(input$nav, {
    req(input$nav)
    switch(
      input$nav,
      filters = showNavPane(ns("pane_filters")),
      instructions = showNavPane(ns("pane_instructions"))
    )
  })

  # Load an example gene list
  observeEvent(input$gene_example, {
    browser()
    shiny::updateTextAreaInput(
      session = session,
      inputId = "gene_list",
      value = paste0(data_genes[[input$gene_example]], collapse = "\n")
    )
  })

  observeEvent(load_example(), once = TRUE, {
    updateFormInput(ns("submit"), submit = TRUE)
  })

  observeEvent(input$gene_form, {
    showNavPane(ns("pane_results"))
  })

  r_gene_list <- reactive({
    if (is.null(input$gene_list)) {
      return(NULL)
    }
    strsplit(input$gene_list, "\n")[[1]]
  })

  r_gene_unknown <- reactive({
    dplyr::setdiff(r_gene_list(), data_gene_info$symbol)
  })

  r_gene_known <- reactive({
    dplyr::intersect(liganded_genes, r_gene_list())
  })

  observeEvent(input$gene_unknowns, {
    req(length(r_gene_unknown()) > 0)

    showModal(
      modal(
        id = NULL,
        header = h5("Unqualified targets"),
        p(paste0("The following ", length(r_gene_unknown()), " targets do not have any annotated ligands: ")),
        p(paste(r_gene_unknown(), collapse = ", "))
      )
    )
  })

  r_eligible_lspci_ids <- callModule(mod_server_filtered_lspci_ids, "")

  output$gene_targets <- renderText({
    if (is.null(r_gene_list()) || length(r_gene_list()) < 1) {
      "No genes upload yet"
    } else {
      paste(length(r_gene_known()), "target(s) with at least one ligand")
    }
  })

  r_selection_selectivity <- reactive({
    req(r_gene_known())

    # browser()
    data_optimal_compounds[
      symbol %in% r_gene_known() &
        selectivity_class %in% input$filter_probes &
        reason_included == "selectivity"
    ]
  })

  r_selection_clinical <- reactive({
    req(r_gene_known())

    data_optimal_compounds[
      symbol %in% r_gene_known() &
        max_phase %in% input$filter_phase &
        reason_included == "clinical"
    ]
  })

  r_selection_chemprobes <- reactive({
    req(r_gene_known())

    data_chemical_probes[
      symbol %in% r_gene_known() &
        avg_rating == 4
    ][
      , reason_included := "expert_opinion"
    ]
  })

  r_selection_table <- reactive({
    req(
      r_gene_known(),
      r_selection_selectivity(),
      r_selection_clinical()
    )

    rbindlist(
      list(
        r_selection_selectivity(),
        r_selection_clinical()
      ),
      fill = TRUE
    )[
      affinity_Q1 <= 2**input$filter_affinity &
        affinity_N >= input$filter_measurement &
        lspci_id %in% r_eligible_lspci_ids()
    ] %>%
      {
        if (isTRUE("chem_probe" %in% input$filter_expert))
          rbindlist(list(., r_selection_chemprobes()), fill = TRUE)
        else .
      }
  })

  r_table_entry <- reactive({
    r_selection_table() %>%
      dplyr::inner_join(
        data_cmpd_info %>%
          dplyr::select(lspci_id, chembl_id, pref_name),
        by = "lspci_id"
      ) %>%
      dplyr::distinct() %>%
      dplyr::select(
        symbol, chembl_id,
        pref_name, selectivity_class, max_phase, affinity_Q1, affinity_N,
        gene_id, reason_included, lspci_id
      ) %>%
      dplyr::mutate_at(           # rounds mean and SD to closest 0.1 if greater than 1.
        vars(affinity_Q1),    # if less than one, rounds to two significant digits.
        ~ ifelse(. > 1, round(., 1), signif(., 2))
      )
  })

  r_table_compound <- reactive({
    r_selection_table() %>%
      dplyr::inner_join(
        data_cmpd_info %>%
          dplyr::select(
            lspci_id, chembl_id, pref_name
          ),
        by = "lspci_id"
      ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(
        lspci_id, chembl_id, pref_name, max_phase
      ) %>%
      dplyr::summarise(
        reason_included = paste(symbol, ": ", reason_included, collapse = "; ")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(
        vars(max_phase),
        as.integer
      )
  })

  r_tbl_data <- reactive({
    req(input$table_display)

    .data <- if (input$table_display == "entry") {
      r_table_entry()
    } else if (input$table_display == "compound") {
      r_table_compound()
    }
  })

  r_tbl <- reactive({
    .data <- r_tbl_data()

    DT::datatable(
      data = .data,
      extensions = c("Buttons"),
      fillContainer = FALSE,
      filter = "top",
      selection = "multiple",
      rownames = FALSE,
      options = list(
        autoWidth = TRUE,
        buttons = list(
          list(
            extend = "colvis",
            text = "Additional columns"
          )
        ),
        columnDefs = list(
          list(
            targets = grep(
              pattern = "^(lspci_id|gene_id)$",
              x = names(.data),
              invert = FALSE
            ) - 1,
            visible = FALSE
          )
        ) %>%
          c(column_title_defs(names(.data))),
        dom = "lfrtipB",
        fixedHeader = list(
          header = TRUE
        ),
        pagingType = "numbers",
        searchHighlight = TRUE,
        scrollX = TRUE
      )
    )
  })

  output$table_results <- DT::renderDataTable(
    r_tbl(),
    server = FALSE
  )

  r_download_name <- reactive({
    create_download_filename(
      c("compound", "library")
    )
  })

  callModule(mod_server_download_button, "output_table_xlsx_dl", r_tbl_data, "excel", r_download_name)
  callModule(mod_server_download_button, "output_table_csv_dl", r_tbl_data, "csv", r_download_name)

  # table row selection ----
  r_tbl_selection <- reactive({
    sort(input$table_results_rows_selected)
  })

  r_selection_drugs <- reactive({
    if (is.null(r_tbl_selection())) {
      return(integer())
    }

    r_tbl_data()$lspci_id[r_tbl_selection()]
  })

  o_chembl_tabs <- callModule(
    mod_server_chembl_tabs, "chembl_tabs_1", data_cmpd_info, r_selection_drugs, lspci_id_name_map
  )

  session
}
