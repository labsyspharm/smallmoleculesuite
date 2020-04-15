tas_weighted_jaccard <- function(query_id, min_n = 6) {
  query_tas <- data_tas[lspci_id == query_id, .(entrez_gene_id, tas)]
  data_tas[
    query_tas,
    on = "entrez_gene_id",
    nomatch = NULL
  ][
    ,
    mask := tas < 10 | i.tas < 10
  ][
    ,
    if (sum(mask) >= min_n) .(
      "tas_similarity" = sum(pmin(tas[mask], i.tas[mask])) / sum(pmax(tas[mask], i.tas[mask])),
      "n" = sum(mask),
      "n_prior" = .N
    ) else .(
      "tas_similarity" = numeric(),
      "n" = integer(),
      "n_prior" = integer()
    ),
    by = "lspci_id"
  ]
}

pfp_correlation <- function(query_id, min_n = 6) {
  query_pfps <- data_pfp[lspci_id == query_id]
  merge(
    query_pfps,
    data_pfp,
    by = "assay_id",
    all = FALSE,
    sort = FALSE,
    suffixes = c("_1", "_2")
  )[
    ,
    mask := abs(rscore_tr_1) >= 2.5 | abs(rscore_tr_2) >= 2.5
  ][
    ,
    if(sum(mask) >= min_n) .(
      "pfp_correlation" = cor(rscore_tr_1, rscore_tr_2),
      "n_prior" = .N,
      "n" = sum(mask)
    ) else .(
      "pfp_correlation" = numeric(),
      "n" = integer(),
      "n_prior" = integer()
    ),
    by = lspci_id_2
  ] %>%
    setnames("lspci_id_2", "lspci_id")
}

chemical_similarity <- function(query_id) {
  fps <- data_fingerprints$tanimoto_all(query_id)
  setDT(fps, key = "id")
  colnames(fps) <- c("lspci_id", "structural_similarity")
  fps
}

calculate_similarities <- function(query_id, min_n_pfp = 6, min_n_tas = 6) {
  tas_sim <- tas_weighted_jaccard(query_id, min_n_tas)[
    ,
    .(lspci_id, tas_similarity, n_tas_similarity = n)
  ]
  pfp_sim <- pfp_correlation(query_id, min_n_pfp)[
    ,
    .(lspci_id, pfp_correlation, n_pfp_correlation = n)
  ]
  chem_sim <- chemical_similarity(query_id)
  merge(
    chem_sim,
    merge(tas_sim, pfp_sim, by = "lspci_id", all = TRUE),
    all.y = TRUE,
    by = "lspci_id"
  )
}



similarityUI <- function(id) {
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
            formGroup(
              label = "Select reference compound",
              input = selectizeInput(
                ns("query_compound"),
                label = NULL,
                choices = NULL,
                multiple = TRUE
              ),
              help = "Search for a compound"
            ),
            formGroup(
              label = "Minimum number of affinity assays in common with reference compound",
              input = div(
                class = "logify-slider active--green",
                shiny::sliderInput(
                  inputId = ns("n_common"),
                  label = NULL,
                  min = 1,
                  max = 8,
                  step = 1,
                  value = 2
                )
              )
            ),
            formGroup(
              label = "Minimum number of phenotypic assays in common with reference compound",
              input = div(
                class = "logify-slider active--green",
                shiny::sliderInput(
                  inputId = ns("n_pheno"),
                  label = NULL,
                  min = 1,
                  max = 8,
                  step = 1,
                  value = 2
                )
              )
            ),
            formGroup(
              label = "Use linked data",
              input = div(
                class = "active--green",
                switchInput(
                  id = ns("use_shared"),
                  choices = "Using linked data may slow down graph load times, but will allow interaction between the graphs and tables.",
                  values = "use",
                  selected = "use"
                )
              )
            )
          ),
          navPane(
            id = ns("pane_instructions"),
            p("The Similarity app helps you find compounds similar to your compound of interest."),
            p("To use the Similarity app:"),
            tags$ol(
              class = "pl-4",
              tags$li(
                "Select a reference compound and set filters as desired. Three plots show up under 'Compound similarity plots'. These plots describe the similarity to the reference compound in phenotype (PFP), targets (TAS), and chemical structure (structural similarity) -- calculated using Morgan2 fingerprints in RDkit."
              ),
              tags$li(
                "Select an area of the compound similarity plots you are interested in. They will show up in table format under 'Compound similarity data'."
              ),
              tags$li(
                "In the 'Compound similarity data' select a compound so see Its Target Afinity Spectrum in the 'Compound similarity selections' that shows up below (you may have to scroll down)."
              )
            )
          )
        )
      ),
      card(
        h3("Compound information"),
        uiOutput(
          outputId = ns("compound_info")
        )
      )
    ),
    column(
      width = 8,
      card(
        h3("Compound similarity plots"),
        p("Select an area of similarity you are interested in. Hover over points for more information. Double-click on plot to un-select region."),
        div(
          columns(
            column(
              width = 4,
              plotly::plotlyOutput(
                outputId = ns("plot_pheno_struct") # mainplot1
              )
            ),
            column(
              width = 4,
              plotly::plotlyOutput(
                outputId = ns("plot_target_struct") # mainplot2
              )
            ),
            column(
              width = 4,
              plotly::plotlyOutput(
                outputId = ns("plot_pheno_target") # mainplot3
              )
            )
          )
        )
      ) %>%
        margin(bottom = 3),
      card(
        h3("Compound similarity data"),
        p("Select up to three similar compounds for which target affinity information will be displayed"),
        div(
          dataTableOutput(
            # output_table
            outputId = ns("table_sim_compound"),
            height = "575px"
          )
        )
      ) %>%
        margin(bottom = 3),
      card(
        h3("Selectivity of selected compound"),
        textOutput(ns("subtitle_selection"), h6),
        div(
          dataTableOutput(
            outputId = ns("table_selection"),
            height = "500px"
          )
        )
      )
    )
  )
}

similarityServer <- function(input, output, session) {
  ns <- session$ns

  observeEvent(input$nav, {
    switch(
      input$nav,
      filters = showNavPane(ns("pane_filters")),
      instructions = showNavPane(ns("pane_instructions"))
    )
  })

  observe({
    updateSelectizeInput(
      session,
      inputId = "query_compound",
      choices = name_lspci_id_map,
      selected = "Nilotinib",
      server = TRUE,
      options = list(
        maxItems = 1,
        maxOptions = 10,
        placeholder = "Compound name",
        loadThrottle = 500,
        createFilter = ".{3,}",
        searchField = "label"
      ),
      callback = fast_search
    )
  })

  c_binding_msg <- reactive({
    if (NROW(r_ref_data()) > 0) {
      paste0(input$query_compound, "; ", lspci_id_name_map[[input$query_compound]])
    } else {
      paste("No gene target binding data available for", lspci_id_name_map[[input$query_compound]])
    }
  })

  output$binding_drug <- renderText({
    c_binding_msg()
  })

  output$reference_drug <- renderText({
    req(input$query_compound)

    paste(
      "Compound similarities for", lspci_id_name_map[[input$query_compound]],
      "from Small Molecule Suite compound library"
    )
  })

  r_sim_data <- reactive({
    req(
      input$query_compound,
      input$n_common,
      input$n_pheno
    )
    calculate_similarities(
      as.integer(input$query_compound),
      min_n_pfp = 2**input$n_pheno,
      min_n_tas = 2**input$n_common
    )[
      , name := lspci_id_name_map[lspci_id]
    ][
      , c("pfp_correlation", "tas_similarity", "structural_similarity") :=
        lapply(.SD, round, digits = 2),
      .SDcols = c("pfp_correlation", "tas_similarity", "structural_similarity")
    ][
      , c("pfp_correlation_plot", "tas_similarity_plot", "structural_similarity_plot") :=
        .(pfp_correlation, tas_similarity, structural_similarity)
    ] %>%
      setnafill(cols = "pfp_correlation_plot", fill = -1.1) %>%
      setnafill(cols = c("tas_similarity_plot", "structural_similarity_plot"), fill = -0.1)
  })

  use_shared_data <- reactive({
    !is.null(input$use_shared)
  })

  r_ref_data <- reactive({
    req(input$query_compound)

    data_selectivity[
      lspci_id == input$query_compound,
      .(
        symbol, selectivity_class, Kd_Q1
      )
    ][
      order(selectivity_class, Kd_Q1)
    ] %>%
      unique()
  })

  r_sim_selection <- reactive({
    sort(input$table_sim_compound_rows_selected)
  })

  r_selection_drugs <- reactive({
    if (is.null(r_sim_selection()))
      return(NULL)

    r_tbl_sim_data()[["lspci_id"]][r_sim_selection()]
  })

  r_selection_titles <- reactive({
    paste(r_tbl_sim_data()[["name"]][r_sim_selection()], collapse = "; ")
  })

  # plots ----
  x_shared_data <- crosstalk::SharedData$new(r_sim_data, ~ lspci_id)

  r_plot_data_shared <- reactive({
    if (use_shared_data()) {
      x_shared_data
    } else {
      r_sim_data()
    }
  })

  make_plot <- function(id, x, y, x_axis, y_axis) {
    text_formula <-paste0(
      '~ paste(',
      '"Drug 1: ", lspci_id_name_map[[input$query_compound]], "\\n",',
      '"Drug 2: ", name, "\\n",',
      '"x: ", ', x, ', "\\n",',
      '"y: ", ', y, ',',
      'sep = "")'
    )
    renderPlotly({
      r_plot_data_shared() %>%
        plot_ly(
          source = id,
          x = reformulate(x),
          y = reformulate(y),
          type = "scatter",
          mode = "markers",
          color = I("black"),
          text = as.formula(text_formula)
        ) %>%
        layout(
          showlegend = FALSE,
          dragmode = "select",
          shapes = list(
            list(type='line', x0= -0.1, x1= -0.1, y0=-1.2, y1=1.2,
                 line=list(dash='dot', width=2, color = "red")),
            list(type='line', x0= -0.15, x1= 1.15, y0=-1.1, y1=-1.1,
                 line=list(dash='dot', width=2, color = "red"))
          ),
          xaxis = x_axis,
          yaxis = y_axis
        ) %>%
        highlight(
          on = "plotly_selected",
          off = "plotly_deselect",
          color = I("#00ac9f")
        )
    })
  }

  # mainplot1
  output$plot_pheno_struct <- make_plot(
    "pheno_struct",
    "structural_similarity_plot", "pfp_correlation_plot",
    x_axis = list(
      range = c(-0.15, 1.15),
      title = "Structural similarity",
      tickmode = "array",
      tickvals = c(-0.1, seq(0,1,.25)),
      ticktext = c("NA", as.character(seq(0,1,.25)))
    ),
    y_axis = list(
      range = c(-1.2, 1.2),
      title = "Phenotypic Correlation",
      tickmode = "array",
      tickvals = c(-1.1, seq(-1,1,.5)),
      ticktext = c("NA", as.character(seq(-1,1,.5)))
    )
  )

  # mainplot2
  output$plot_target_struct <- make_plot(
    "target_struct",
    "structural_similarity_plot", "tas_similarity_plot",
    x_axis = list(
      range = c(-0.15, 1.15),
      title = "Structural similarity",
      tickmode = "array",
      tickvals = c(-0.15, seq(0,1,.25)),
      ticktext = c("NA", as.character(seq(0,1,.25)))
    ),
    y_axis = list(
      range = c(-0.15, 1.15),
      title = "Target Similarity",
      tickmode = "array",
      tickvals = c(-0.15, seq(0,1,.2)),
      ticktext = c("NA", as.character(seq(0,1,.2)))
    )
  )

  # mainplot3
  output$plot_pheno_target <- make_plot(
    "pheno_target",
    "structural_similarity_plot", "pfp_correlation_plot",
    x_axis = list(
      range = c(-0.15, 1.15),
      title = "Target Similarity",
      tickmode = "array",
      tickvals = c(-0.15, seq(0,1,.25)),
      ticktext = c("NA", as.character(seq(0,1,.25)))
    ),
    y_axis = list(
      range = c(-1.2, 1.2),
      title = "Phenotypic Correlation",
      tickmode = "array",
      tickvals = c(-1.2, seq(-1,1,.5)),
      ticktext = c("NA", as.character(seq(-1,1,.5)))
    )
  )

  compounds_selected <- reactiveVal()

  walk(
    c("pheno_struct", "target_struct", "pheno_target"),
    ~observe({
      compounds_selected(event_data("plotly_selected", .x)$key)
    })
  )

  observeEvent(input$query_compound, {
    compounds_selected(NULL)
  })

  r_cmpd_info_data <- reactive({
    selected_ids <- r_selection_drugs()
    if (length(selected_ids) < 1)
      return(tags$p("No compound selected"))
    else if (length(selected_ids) > 1)
      return(tags$p("More than one compound selected"))
    chembl_id <- data_cmpd_info[lspci_id == as.integer(selected_ids)][["chembl_id"]]
    if (is.na(chembl_id))
      return(tags$p("No information on ChEMBL for compound"))
    chembl_url <- paste0(
      "https://www.ebi.ac.uk/chembl/embed/#compound_report_card/", chembl_id, "/name_and_classification"
    )
    tags$object(data = chembl_url, width = "100%", height = "500")
  })

  output$compound_info <- renderUI(r_cmpd_info_data())

  r_tbl_sim_data <- reactive({
    selected_ids <- compounds_selected()
    if (use_shared_data() && length(selected_ids) > 0) {
      r_sim_data()[lspci_id %in% selected_ids]
    } else {
      r_sim_data()
    } %>%
      select(name, everything(), -ends_with("_plot"))
  })

  r_tbl_sim_compound <- reactive({
    .data <- r_tbl_sim_data()

    col_types <- unname(vapply(.data, class, character(1)))

    download_name <- create_download_filename(
      c("similarity", "table", input$query_compound)
    )

    DT::datatable(
      .data,
      extensions = c("Buttons"),
      rownames = FALSE,
      options = list(
        # autoWidth = TRUE,
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
            extend = "colvis"
          )
        ),
        columnDefs = list(
          list(
            targets = grep(
              x = names(.data),
              pattern = "^(name|structural_similarity|pfp_correlation|tas_similarity)$",
              invert = TRUE
            ) - 1,
            visible = FALSE
          ),
          list(
            targets = grep(
              x = names(.data),
              pattern = "^(structural_similarity|pfp_correlation|tas_similarity)$"
            ) - 1,
            defaultContent = "NA"
          )
        ),
        selection = list(mode = "multiple", target = "column"),
        dom = "lfrtipB",
        pagingType = "numbers",
        scrollCollapse = TRUE,
        scrollX = FALSE,
        searchHighlight = TRUE,
        stateSave = TRUE
      )
    )
  })

  output$table_sim_compound = DT::renderDataTable(
    r_tbl_sim_compound(),
    server = TRUE
  )

  get_compound_selection <- function(drug_id) {
    if (is.null(drug_id) || is.na(drug_id) || length(drug_id) < 1) {
      return(
        data_affinity_selectivity[FALSE]
      )
    }

    data_affinity_selectivity[
      lspci_id %in% drug_id
    ][
      order(selectivity_class, Kd_Q1)
    ]
  }

  output$subtitle_selection <- renderText({
    r_selection_titles()
  })

  output$table_selection <- DT::renderDataTable({
    download_name <- create_download_filename(
      c("affinity", "spectrum", input$query_compound)
    )

    DT::datatable(
      data = get_compound_selection(r_selection_drugs()), # input$compound_selection),
      rownames = FALSE,
      # fillContainer = TRUE,
      options = list(
        # autoWidth = TRUE,
        extensions = "Buttons",
        buttons = list(
          list(extend = "copy"),
          list(
            extend = "csv",
            title = download_name
          ),
          list(
            extend = "excel",
            title = download_name
          )
        ),
        dom = "tpB",
        language = list(
          emptyTable = if (is.null(r_sim_selection())) {
            "Please select row(s) from the data above."
          } else {
            "No data available"
          }
        ),
        scrollX = FALSE,
        pagingType = "numbers"
      )
    )
  })
}
