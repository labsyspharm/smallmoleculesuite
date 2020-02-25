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
                label = "Gene name of target drug",
                input = shiny::textAreaInput(
                  inputId = ns("gene_list"),
                  label = NULL,
                  rows = 5
                ) %>%
                  margin(top = -3),
                help = div(
                  "This tool uses HUGO names. Please see",
                  tags$a(
                    target = "_blank", href = "https://genenames.org",
                    "genenames.org"
                  ),
                  "for help."
                )
              ),
              formSubmit(
                label = "Pre-defined gene sets"
              ) %>%
                background("orange")
            ) %>%
              margin(bottom = 3),
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
                  label = tags$h6("Probes") %>% margin(b = 0),
                  input = checkboxInput(
                    inline = TRUE,
                    id = ns("filter_probes"),
                    choices = c("Most selective", "Semi-selective", "Poly-selective", "Unknown"),
                    # values = c("best", "second", "non", "un"),
                    selected = "Most selective"
                  ) %>%
                    active("orange"),
                  help = "Choose the selectivity levels for which you want chemical probes to be included in the library."
                ),
                formGroup(
                  label = tags$h6("Maximum clinical phase") %>% margin(b = 0),
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
                  label = tags$h6("Maximum Kd for query target (nM)") %>% margin(b = 0),
                  div(
                    class = "logify-slider active--orange",
                    shiny::sliderInput(
                      inputId = ns("filter_affinity"),
                      label = NULL,
                      min = 0,
                      max = 5,
                      step = 0.25,
                      value = 3
                    )
                  )
                ),
                formGroup(
                  label = tags$h6("Minimum number of measurements") %>% margin(b = 0),
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
          )
        )
      ) %>%
        margin(b = 3),
      card(
        h3("Reference cards"),
        htmlOutput(
          outputId = ns("chembl_list")
        )
      ) %>%
        margin(b = 2)
    )
  )
}

libraryServer <- function(input, output, session, load_example) {
  ns <- session$ns

  # Define genes found in our data
  data_all_genes <- data_selection_selectivity$symbol

  # nav ----
  observeEvent(input$nav, {
    switch(
      input$nav,
      filters = showNavPane(ns("pane_filters")),
      instructions = showNavPane(ns("pane_instructions"))
    )
  })

  # Load an example gene list
  observeEvent(input$gene_example, {
    shiny::updateTextAreaInput(
      session = session,
      inputId = "gene_list",
      value = paste0(data_genes[[input$gene_example]], collapse = "\n")
    )
  })

  observeEvent(load_example(), once = TRUE, {
    session$sendCustomMessage("click.library.sm", list())
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
    dplyr::setdiff(r_gene_list(), data_all_genes)
  })

  r_gene_known <- reactive({
    dplyr::intersect(data_all_genes, r_gene_list())
  })

  observeEvent(input$gene_unknowns, {
    req(length(r_gene_unknown()) > 0)

    showModal(
      modal(
        id = NULL,
        header = h5("Unqualified targets"),
        p("The following targets do not have a known qualifying ligand, please check HUGO name: "),
        p(paste(r_gene_unknown(), collapse = ", "))
      )
    )
  })

  output$gene_targets <- renderText({
    if (is.null(r_gene_list()) || length(r_gene_list()) < 1) {
      "No genes upload yet"
    } else {
      paste(length(r_gene_known()), "target(s) with at least one ligand")
    }
  })

  r_selection_selectivity <- reactive({
    req(r_gene_known())

    gene_select <- data_selection_selectivity %>%
      dplyr::filter(symbol %in% r_gene_known())

    if (is.null(input$filter_probes)) {
      return(dplyr::slice(gene_select, 0))
    }

    gene_select %>%
      dplyr::filter(selectivity_class %in% input$filter_probes)
  })

  r_selection_clinical <- reactive({
    req(r_gene_known())

    gene_clinical <- data_selection_selectivity %>%
      dplyr::filter(symbol %in% r_gene_known())

    if (is.null(input$filter_phase)) {
      return(dplyr::slice(gene_clinical, 0))
    }

    phase_clinical <- gene_clinical %>%
      dplyr::filter(max_phase %in% input$filter_phase)

    # sliders "sd" and "affinity" are in log10 scale
    phase_clinical %>%
      dplyr::filter(Kd_Q1 <= 10 ^ input$filter_affinity) %>%
      dplyr::filter(n_measurement_kd >= input$filter_measurement)
  })

  r_selection_chemprobes <- reactive({
    req(r_gene_known())

    data_selection_chemprobes %>%
      dplyr::mutate(Kd_Q1 = NA, n_measurement_kd = NA, selectivity_class = "probe") %>%
      dplyr::filter(symbol %in% r_gene_known())
  })

  r_selection_table <- reactive({
    req(
      r_gene_known(),
      r_selection_selectivity(),
      r_selection_clinical()
    )

    target_cols <- c(
      "gene_id", "lspci_id", "KD_Q1", "SD_aff", "n_measurement_kd", "selectivity_class"
    )

    this <- dplyr::bind_rows(
      r_selection_selectivity() %>% dplyr::select(!!target_cols),
      r_selection_clinical() %>% dplyr::select(!!target_cols)
    )

    if (isTRUE("chem_probe" %in% input$filter_expert)) {
      req(r_selection_chemprobes())

      this <- this %>%
        dplyr::bind_rows(
          r_selection_chemprobes() %>% dplyr::select(!!target_cols)
        )
    }

    this
  })

  r_table_entry <- reactive({
    r_selection_table() %>%
      dplyr::inner_join(
        data_cmpd_info %>%
          dplyr::select(lspci_id, chembl_id, pref_name, max_phase),
        by = "lspci_id"
      ) %>%
      dplyr::inner_join(
        data_gene_info,
        by = "gene_id"
      ) %>%
      dplyr::distinct() %>%
      dplyr::select(
        symbol, chembl_id,
        pref_name, selectivity_class, max_phase, Kd_Q1, n_measurement_kd,
        gene_id, tax_id
      ) %>%
      dplyr::mutate_at(
        vars(symbol, chembl_id, pref_name, source, gene_id, tax_id),
        factor
      ) %>%
      dplyr::mutate_at(
        vars(max_phase),
        as.integer
      ) %>%
      dplyr::mutate_at(           # rounds mean and SD to closest 0.1 if greater than 1.
        vars(Kd_Q1),    # if less than one, rounds to two significant digits.
        ~ ifelse(. > 1, round(., 1), signif(., 2))
      ) %>%
      dplyr::rename(
        `Kd_Q1_(nM)` = Kd_Q1,
        reason_included = selectivity_class
      )
  })

  r_table_compound <- reactive({
    r_selection_table() %>%
      dplyr::inner_join(
        data_cmpd_info %>%
          dplyr::select(
            lspci_id, chembl_id, pref_name, max_phase,
            alt_names
          ),
        by = "lspci_id"
      ) %>%
      dplyr::inner_join(
        data_gene_info,
        by = "gene_id"
      ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(
        lspci_id, chembl_id, pref_name, alt_names, max_phase
      ) %>%
      dplyr::summarise(
        sources = paste(symbol, "; ", selectivity_class, collapse = ", ")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(
        vars(lspci_id, chembl_id, pref_name),
        factor
      ) %>%
      dplyr::mutate_at(
        vars(max_phase),
        as.integer
      ) %>%
      dplyr::rename(
        reason_included = sources
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
    view_type <- if (input$table_display == "entry") "target" else "compound"
    download_name <- create_download_filename(
      c("library", "small", "molecule", "suite"), c(view_type, "view")
    )

    .data <- r_tbl_data() %>%
      dplyr::mutate(
        ` ` = NA_character_,
        chembl_id = lapply(
          glue(
            "<a target='_blank'
                href='https://www.ebi.ac.uk/chembl/compound_report_card/{ chembl_id }'>
                { chembl_id }<sup class='ml-1'><i class='fa fa-external-link'></i></sup>
             </a>"
          ),
          HTML
        ),
      ) %>%
      dplyr::select(` `, everything())

    DT::datatable(
      data = .data,
      extensions = c("Buttons", "Select"),
      fillContainer = FALSE,
      filter = "top",
      rownames = FALSE,
      options = list(
        autoWidth = TRUE,
        buttons = list(
          list(extend = "copy"),
          list(
            extend = "csv",
            title = download_name
          ),
          list(
            extend = "excel",
            title = download_name
          ),
          list(
            extend = "colvis",
            columns = ":not(.select-checkbox)"
          )
        ),
        columnDefs = list(
          list(
            targets = 0,
            className = "select-checkbox",
            orderable = FALSE
          ),
          list(
            targets = 1,
            visible = input$table_display != "compound"
          )
        ),
        dom = "lfrtipB",
        fixedHeader = list(
          header = TRUE
        ),
        pagingType = "numbers",
        searchHighlight = TRUE,
        select = list(
          style = "os",
          selector = "td.select-checkbox"
        ),
        scrollX = FALSE
      )
    )
  })

  output$table_results <- DT::renderDataTable(
    r_tbl(),
    server = FALSE
  )

  output$chembl_list <- renderUI({
    req(input$table_results_rows_selected)

    tbl_selection <- r_tbl_data()[input$table_results_rows_selected, ]
    chembl_ids <- sort(unique(as.character(tbl_selection$chembl_id)))

    chembl_first <- vector("logical", length(chembl_ids))
    chembl_first[1] <- TRUE

    nav_link <- function(x, active) {
      active <- if (active) " active" else ""
      glue("<a class='text-black nav-link{ active }' data-toggle='pill'
             data-target='#{ x }' role='tab'>{ x }</a>")
    }

    tab_pane <- function(x, active) {
      active <- if (active) " show active" else ""
      glue("<div class='tab-pane { active }' id='{ x }'
             style='height: 750.5px'
             role='tabpanel'>
             <object data='https://www.ebi.ac.uk/chembl/embed/#compound_report_card/{ x }/name_and_classification'
              width='100%' height='100%'>
             </object>
           </div>")
    }

    chembl_panes <- c(
      "<div class='row'>",
      "<div class='col-3'>",
      "<div class='nav flex-colum nav-pills active-orange' role='tablist'>",
      purrr::map2_chr(chembl_ids, chembl_first, nav_link),
      "</div>",
      "</div>",
      "<div class='col-9'>",
      "<div class='tab-content'>",
      purrr::map2_chr(chembl_ids, chembl_first, tab_pane),
      "</div>",
      "</div>",
      "</div>"
    )

    HTML(glue_collapse(chembl_panes, "\n"))
  })

}
