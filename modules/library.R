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
            formGroup(
              label = NULL,
              input = shiny::textAreaInput(
                inputId = ns("gene_list"),
                label = NULL,
                rows = 5
              ),
              help = tagList(
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
              input = shiny::selectInput(
                inputId = ns("gene_example"),
                label = NULL,
                choices = data_gene_lists[["gene_list"]] %>%
                  unique(),
                selected = "Dark Kinome",
                multiple = FALSE
              ),
              help = "Selecting a choice will populate the input above with an example list of genes."
            ),
            actionButton(
              inputId = ns("submit"),
              class = "bg-orange",
              label = "Submit"
            ),
            actionButton(
              ns("reset_gene_list"),
              "Clear list",
              icon = icon("redo"),
              onclick = glue("$('#{ns('gene_list')}')[0].value = null;")
            ),
            p(
              shiny::textOutput(
                outputId = ns("gene_targets"),
                inline = TRUE
              ),
              uiOutput(
                outputId = ns("gene_unknowns_ui"),
                inline = TRUE
              )
            ) %>%
              margin(top = 4, bottom = 4),
            tags$hr(),
            formGroup(
              label = "Commercial availability",
              class = "active--orange",
              input = div(
                mod_ui_filter_commercial(ns(""))
              )
            ),
            formGroup(
              label = tags$h6("Selectivity levels") %>% margin(b = 0),
              class = "active--orange",
              input = checkboxInput(
                inline = TRUE,
                id = ns("filter_probes"),
                choices = c("Most selective", "Semi-selective", "Poly-selective", "Unknown"),
                selected = "Most selective"
              ),
              help = "Add compounds with the given selectivity to the library."
            ),
            formGroup(
              label = tags$h6("Clinical phases") %>% margin(b = 0),
              class = "active--orange",
              input = checkboxInput(
                inline = TRUE,
                id = ns("filter_phase"),
                choices = c("Approved", "Phase III", "Phase II", "Phase I"),
                values = c(4, 3, 2, 1),
                selected = 4
              ),
              help = "Add compounds that are approved or in clinical development to the library."
            ),
            formGroup(
              label = tags$h6("Expert opinion compounds") %>% margin(b = 0),
              class = "active--orange",
              input = switchInput(
                inline = TRUE,
                id = ns("filter_expert"),
                choices = "chemicalprobes.org 4-star rating",
                values = "chem_probe",
                selected = "chem_probe"
              ),
              help = "Add compounds endorsed by experts to the library."
            ),
            formGroup(
              label = tags$h6("Output table") %>% margin(b = 0),
              class = "active--orange",
              input = radiobarInput(
                id = ns("table_display"),
                class = "btn-group-secondary",
                choices = c("Display per entry", "Display per compound"),
                values = c("entry", "compound"),
                selected = "entry"
              )
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
        div(
          DT::dataTableOutput(
            outputId = ns("table_results")
          ),
          mod_ui_download_button(ns("output_table_csv_dl"), "Download CSV"),
          mod_ui_download_button(ns("output_table_xlsx_dl"), "Download Excel")
        )
      ) %>%
        margin(b = 3)
    )
  )
}

libraryServer <- function(input, output, session) {
  ns <- session$ns

  # Define genes found in our data
  liganded_genes <- union(
    data_library$lspci_target_id,
    data_chemical_probes$lspci_target_id
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

  r_example_gene_symbols <- reactive({
    req(input$gene_example)
    paste0(
      data_gene_lists[gene_list == input$gene_example][["symbol"]],
      collapse = "\n"
    )
  })

  # Load an example gene list
  observeEvent(r_example_gene_symbols(), {
    req(r_example_gene_symbols)
    shiny::updateTextAreaInput(
      session = session,
      inputId = "gene_list",
      value = r_example_gene_symbols()
    )
  })

  r_symbol_list <- reactiveVal()

  observeEvent(input$submit, {
    gene_list <- input$gene_list
    req(gene_list)
    r_symbol_list(
      str_split(gene_list, fixed("\n"))[[1]]
    )
  })

  observeEvent(r_example_gene_symbols(), {
    req(r_example_gene_symbols())
    r_symbol_list(
      str_split(r_example_gene_symbols(), fixed("\n"))[[1]]
    )
  })

  r_target_id_list <- reactive({
    req(r_symbol_list())
    data_targets[
      symbol %in% r_symbol_list()
    ][["lspci_target_id"]]
  })

  r_gene_unknown <- reactive({
    dplyr::setdiff(r_target_id_list(), liganded_genes)
  })

  r_gene_known <- reactive({
    dplyr::intersect(liganded_genes, r_target_id_list())
  })

  observeEvent(input$gene_unknowns, {
    req(length(r_gene_unknown()) > 0)

    showModal(
      modal(
        id = NULL,
        header = h5("Unqualified targets"),
        p(paste0("The following ", length(r_gene_unknown()), " targets do not have any annotated ligands: ")),
        p(paste(
          target_id_to_name(r_gene_unknown()),
          collapse = ", "
        ))
      )
    )
  })

  r_eligible_lspci_ids <- callModule(mod_server_filter_commercial, "")[["r_eligible_lspci_ids"]]

  output$gene_targets <- renderText({
    if (is.null(r_target_id_list()) || (length(r_target_id_list()) < 1)) {
      "No genes upload yet"
    } else {
      paste(
        length(r_gene_known()),
        "target(s) with at least one ligand."
      )
    }
  })


  output$gene_unknowns_ui <- renderUI({
    if (is.null(r_gene_unknown()) || (length(r_gene_unknown()) < 1))
      return(NULL)
    linkInput(
      id = ns("gene_unknowns"),
      label = span(
        shiny::icon("bars"),
        paste(length(r_gene_unknown()), "without ligands.")
      )
    )
  })

  r_selection_selectivity <- reactive({
    req(r_gene_known())
    data_library[
      lspci_target_id %in% r_gene_known() &
        selectivity_class %in% input$filter_probes &
        reason_included == "selectivity"
    ]
  })

  r_selection_clinical <- reactive({
    req(r_gene_known())
    data_library[
      lspci_target_id %in% r_gene_known() &
        max_phase %in% input$filter_phase &
        reason_included == "clinical"
    ]
  })

  r_selection_chemprobes <- reactive({
    req(r_gene_known())
    data_chemical_probes[
      lspci_target_id %in% r_gene_known() &
        max_rating == 4
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
      ontarget_ic50_q1 <= 2**input$filter_affinity &
        ontarget_n >= input$filter_measurement &
        lspci_id %in% r_eligible_lspci_ids()
    ] %>%
      {
        if (isTRUE("chem_probe" %in% input$filter_expert))
          rbindlist(list(., r_selection_chemprobes()), fill = TRUE)
        else .
      }
  })

  r_table_entry <- reactive({
    r_selection_table()[
      data_compounds[
        ,
        .(lspci_id, chembl_id, name = pref_name)
      ],
      on = .(lspci_id), nomatch = NULL
    ][
      data_targets[
        ,
        .(lspci_target_id, symbol, gene_id)
      ],
      on = .(lspci_target_id), nomatch = NULL
    ][
      ,
      .(symbol, chembl_id,
        name, selectivity_class, max_phase, ontarget_ic50_q1, ontarget_n,
        gene_id, reason_included, lspci_id)
    ] %>%
      unique()
  })

  r_table_compound <- reactive({
    r_selection_table()[
      ,
      max_phase := as.integer(max_phase)
    ][
      data_compounds[
        ,
        .(lspci_id, chembl_id, name = pref_name)

      ], on = .(lspci_id), nomatch = NULL
    ][
      data_targets[
        ,
        .(lspci_target_id, symbol, gene_id)
      ], on = .(lspci_target_id), nomatch = NULL
    ][
      ,
      .(reason_included = paste(symbol, ": ", reason_included, collapse = "; ", sep = "")),
      keyby = .(lspci_id, chembl_id, name, max_phase)
    ] %>%
      unique()
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

    dt <- datatable_tooltip(
      data = .data,
      extensions = c("Buttons"),
      style = "bootstrap4",
      selection = "multiple",
      rownames = FALSE,
      column_specs = COLUMN_SPECS,
      options = list(
        dom = DT_DOM,
        buttons = list(
          list(
            extend = "colvis",
            text = "Additional columns",
            className = "btn-outline-black"
          )
        ),
        columnDefs = list(
          list(
            targets = grep(
              pattern = "^(lspci_id|gene_id)$",
              x = names(.data),
              invert = FALSE
            ) - 1L,
            visible = FALSE
          )
        ),
        pagingType = "numbers",
        searchHighlight = TRUE,
        scrollX = TRUE
      )
    )

    if (isolate(input$table_display) == "entry")
      dt <- dt_style_selectivity(dt)

    dt
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
  # r_tbl_selection <- reactive({
  #   sort(input$table_results_rows_selected)
  # })
  #
  # r_selection_drugs <- reactive({
  #   if (is.null(r_tbl_selection())) {
  #     return(integer())
  #   }
  #
  #   r_tbl_data()$lspci_id[r_tbl_selection()]
  # })

  # o_chembl_tabs <- callModule(
  #   mod_server_chembl_tabs, "chembl_tabs_1", data_compounds, r_selection_drugs, lspci_id_name_map
  # )

  setBookmarkExclude(
    table_inputs("table_results")
  )
}
