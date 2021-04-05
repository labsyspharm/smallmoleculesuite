tas_weighted_jaccard <- function(query_id, min_n = 6) {
  query_tas <- data_tas[lspci_id == query_id, .(lspci_target_id, tas)]
  data_tas[
    ,
    .(lspci_id, lspci_target_id, tas)
  ][
    query_tas,
    on = "lspci_target_id",
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
      tas_similarity = double(),
      n = integer(),
      n_prior = integer()
    ),
    by = "lspci_id"
  ]
}
tas_weighted_jaccard <- memoise(tas_weighted_jaccard)

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
      "n" = sum(mask),
      "n_prior" = .N
    ) else .(
      pfp_correlation = double(),
      n = integer(),
      n_prior = integer()
    ),
    by = lspci_id_2
  ] %>%
    setnames("lspci_id_2", "lspci_id")
}
pfp_correlation <- memoise(pfp_correlation)

chemical_similarity <- function(query_id) {
  fps <- data_fingerprints$tanimoto_all(query_id)
  setDT(fps, key = "id")
  colnames(fps) <- c("lspci_id", "structural_similarity")
  fps
}
chemical_similarity <- memoise(chemical_similarity)

#
# all_similarities <- function(
#   query_compound,
#   tas_n_common,
#   pfp_n_common,
#   eligible_lspci_ids
# ) {
#
#   tas_sim <- tas_weighted_jaccard(
#     as.integer(query_compound),
#     2**tas_n_common
#   )[
#     ,
#     .(lspci_id, tas_similarity, n_tas_similarity = n)
#   ]
#
#   pfp_sim <- pfp_correlation(
#     as.integer(query_compound),
#     2**pfp_n_common
#   )[
#     ,
#     .(lspci_id, pfp_correlation, n_pfp_correlation = n)
#   ]
#
#   chem_sim <- chemical_similarity(as.integer(query_compound))
#
#   merge(
#     chem_sim,
#     merge(tas_sim, pfp_sim, by = "lspci_id", all = TRUE),
#     all.y = TRUE,
#     by = "lspci_id"
#   ) %>%
#     {
#       .[
#         lspci_id %in% eligible_lspci_ids
#       ][
#         data_compounds[
#           ,
#           .(lspci_id, name = pref_name, chembl_id)
#         ],
#         on = .(lspci_id), nomatch = NULL
#       ][
#         , c("pfp_correlation", "tas_similarity", "structural_similarity") :=
#           lapply(.SD, round, digits = 2),
#         .SDcols = c("pfp_correlation", "tas_similarity", "structural_similarity")
#       ][
#         , c("pfp_correlation_plot", "tas_similarity_plot", "structural_similarity_plot") :=
#           .(pfp_correlation, tas_similarity, structural_similarity)
#       ] %>%
#         setnafill(cols = "pfp_correlation_plot", fill = -1.1) %>%
#         setnafill(cols = c("tas_similarity_plot", "structural_similarity_plot"), fill = -0.1)
#     }
# }

all_similarities_table <- function(
  query_compound,
  sim_data,
  tas_n_common,
  pfp_n_common,
  only_commercial,
  selected_compounds = NULL
) {
  (
    if (is.null(selected_compounds))
      sim_data
    else
      sim_data[lspci_id %in% selected_compounds]
  )[
    order(-pfp_correlation, -tas_similarity, -structural_similarity)
  ] %>% {
    # Add chemical similarity for compounds that don't have any other similarity
    if (is.null(selected_compounds))
      rbindlist(list(
        .,
        chemical_similarity(query_compound)[
          !lspci_id %in% .[["lspci_id"]] &
            if (only_commercial) lspci_id %in% filter_commercial(TRUE) else TRUE
        ][
          order(-structural_similarity)
        ][,
          structural_similarity := round(structural_similarity, digits = 2)
        ][
          data_compounds[, .(lspci_id, name = pref_name, chembl_id)],
          on = "lspci_id", nomatch = NULL
        ]
      ), fill = TRUE)
    else
      .
  } %>%
    select(
      name, chembl_id, everything(),
      -ends_with("_plot"), -starts_with("i.")
    )
}
all_similarities_table <- memoise(all_similarities_table, omit_args = c("sim_data"))

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
            tags$h5("Reference compound") %>%
              margin(b = 3),
            mod_ui_select_compounds(ns("query")) %>%
              htmltools::tagAppendChild(
                tags$small(
                  class = "form-text text-muted",
                  "Compare selected compound to all other compounds in the database."
                )
              ),
            tags$hr(),
            formGroup(
              label = "Minimum number of affinity assays in common with reference compound",
              input = div(
                class = "logify-slider active--green",
                shiny::sliderInput(
                  inputId = ns("n_common"),
                  label = NULL,
                  min = 1,
                  max = 7,
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
                  max = 7,
                  step = 1,
                  value = 2
                )
              )
            ),
            formGroup(
              label = "Commercial availability",
              input = div(
                class = "active--green",
                mod_ui_filter_commercial(ns(""))
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
      ) %>%
        margin(bottom = 3),
      mod_ui_chembl_tabs(ns(""))
    ),
    column(
      width = 8,
      card(
        # header = h6("Drag to filter."),
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
              ) %>%
                shinycssloaders::withSpinner(color = "#303030")
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
        # header = h6("Select compounds for additional information"),
        div(
          dataTableOutput(
            outputId = ns("table_sim_compound")
          ),
          mod_ui_download_button(ns("output_table_csv_dl"), "Download CSV"),
          mod_ui_download_button(ns("output_table_xlsx_dl"), "Download Excel")
        )
      ) %>%
        margin(bottom = 3)
      # mod_ui_affinity_tables(ns("affinity_tables_1"))
    )
  ) %>%
    container()
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

  l_filter_commercial <- callModule(mod_server_filter_commercial, "")
  r_eligible_lspci_ids <- l_filter_commercial[["r_eligible_lspci_ids"]]
  r_only_commercial <- l_filter_commercial[["r_only_commercial"]]


  r_query_compound <- callModule(
    mod_server_select_compounds,
    "query",
    r_only_commercial,
    default_choice = 100755L,
    selectize_options = list(
      maxItems = 1L
    )
  )

  r_query_compound_name <- reactive({
    req(r_query_compound())
    compound_id_to_name(r_query_compound())
  })

  output$binding_drug <- renderText({
    c_binding_msg()
  })

  r_tas_sim <- reactive({
    req(r_query_compound())
    tas_weighted_jaccard(
      as.integer(r_query_compound()),
      2**input$n_common
    )[
      ,
      .(lspci_id, tas_similarity, n_tas_similarity = n)
    ]
  })

  r_pfp_sim <- reactive({
    req(r_query_compound())
    pfp_correlation(
      as.integer(r_query_compound()),
      2**input$n_pheno
    )[
      ,
      .(lspci_id, pfp_correlation, n_pfp_correlation = n)
    ]
  })

  r_chem_sim <- reactive({
    req(r_query_compound())
    chemical_similarity(as.integer(r_query_compound()))
  })

  r_sim_data <- reactive({
    req(
      r_chem_sim(),
      r_tas_sim(),
      r_pfp_sim(),
      r_eligible_lspci_ids()
    )
    merge(
      r_chem_sim(),
      merge(r_tas_sim(), r_pfp_sim(), by = "lspci_id", all = TRUE),
      all.y = TRUE,
      by = "lspci_id"
    ) %>%
      {
        .[
          lspci_id %in% r_eligible_lspci_ids()
        ][
          data_compounds[
            ,
            .(lspci_id, name = pref_name, chembl_id)
          ],
          on = .(lspci_id), nomatch = NULL
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
      }
  })

  r_sim_selection <- reactive({
    sort(input$table_sim_compound_rows_selected)
  })

  r_selection_drugs <- reactive({
    if (is.null(r_sim_selection()))
      return(integer())

    r_tbl_sim_data()[["lspci_id"]][r_sim_selection()]
  })

  # plots ----
  x_shared_data <- crosstalk::SharedData$new(r_sim_data, ~ lspci_id)

  make_plot <- function(id, x, y, x_axis, y_axis) {
    text_formula <- paste0(
      '~ paste(',
      '"Drug 1: ", r_query_compound_name(), "\\n",',
      '"Drug 2: ", name, "\\n",',
      '"x: ", ', x, ', "\\n",',
      '"y: ", ', y, ',',
      'sep = "")'
    )
    x_line <- c(x_axis[["tickvals"]][[1]], y_axis[["range"]])
    y_line <- c(y_axis[["tickvals"]][[1]], x_axis[["range"]])
    renderPlotly({
      x_shared_data %>%
        plot_ly(
          source = id,
          x = reformulate(x),
          y = reformulate(y)
        ) %>%
        add_trace(
          type = "scatter",
          mode = "markers",
          color = I("black"),
          text = as.formula(text_formula),
          opacity = 0.7,
          customdata = ~ lspci_id
        ) %>%
        layout(
          showlegend = FALSE,
          dragmode = "select",
          shapes = list(
            list(type='line', x0= x_line[[1]], x1= x_line[[1]], y0=x_line[[2]], y1=x_line[[3]],
                 line=list(dash='dot', width=2, color = "red")),
            list(type='line', x0= y_line[[2]], x1= y_line[[3]], y0=y_line[[1]], y1=y_line[[1]],
                 line=list(dash='dot', width=2, color = "red"))
          ),
          xaxis = x_axis,
          yaxis = y_axis
        ) %>%
        highlight(
          on = "plotly_selected",
          off = "plotly_deselect",
          color = I("#00ac9f")
        ) %>%
        toWebGL()
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
      tickvals = c(-0.1, seq(0,1,.25)),
      ticktext = c("NA", as.character(seq(0,1,.25)))
    ),
    y_axis = list(
      range = c(-0.15, 1.15),
      title = "Target Similarity",
      tickmode = "array",
      tickvals = c(-0.1, seq(0,1,.2)),
      ticktext = c("NA", as.character(seq(0,1,.2)))
    )
  )

  # mainplot3
  output$plot_pheno_target <- make_plot(
    "pheno_target",
    "tas_similarity_plot", "pfp_correlation_plot",
    x_axis = list(
      range = c(-0.15, 1.15),
      title = "Target Similarity",
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

  r_compounds_selected_plot <- reactiveVal()

  walk(
    c("pheno_struct", "target_struct", "pheno_target"),
    ~observe({
      r_compounds_selected_plot(event_data("plotly_selected", .x)$customdata)
    })
  )

  observeEvent(r_query_compound(), {
    r_compounds_selected_plot(NULL)
  })

  r_tbl_sim_data <- reactive({
    req(r_sim_data())
    all_similarities_table(
      isolate(r_query_compound()),
      r_sim_data(),
      # Passing these only to make sure memoization works
      isolate(input$n_common),
      isolate(input$n_pheno),
      isolate(r_only_commercial()),
      r_compounds_selected_plot()
    )
  })

  r_dt_sim_data <- reactive({
    .data <- r_tbl_sim_data()
    col_types <- unname(vapply(.data, class, character(1)))

    datatable_tooltip(
      .data,
      extensions = c("Buttons"),
      style = "bootstrap4",
      rownames = FALSE,
      selection = "single",
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
              x = names(.data),
              pattern = "^(name|chembl_id|structural_similarity|pfp_correlation|tas_similarity)$",
              invert = TRUE
            ) - 1,
            visible = FALSE
          ),
          list(
            targets = (1:ncol(.data)) - 1,
            defaultContent = "NA"
          )
        ),
        pagingType = "numbers",
        scrollCollapse = TRUE,
        searchHighlight = TRUE
      )
    )
  })

  output$table_sim_compound = DT::renderDataTable(
    r_dt_sim_data(),
    server = TRUE
  )

  r_download_name <- reactive({
    create_download_filename(
      c("similarity", "table", r_query_compound_name())
    )
  })

  setBookmarkExclude(
    table_inputs("table_sim_compound")
  )

  callModule(mod_server_download_button, "output_table_xlsx_dl", r_tbl_sim_data, "excel", r_download_name)
  callModule(mod_server_download_button, "output_table_csv_dl", r_tbl_sim_data, "csv", r_download_name)

  r_selected_compounds_table <- reactive({
    if (is.null(input$table_sim_compound_rows_selected))
      NULL
    else
      isolate(r_tbl_sim_data())[
        input$table_sim_compound_rows_selected
      ][["lspci_id"]]
  })

  callModule(mod_server_chembl_tabs, "", r_selected_compounds_table)

  # callModule(
  #   mod_server_affinity_tables,
  #   "affinity_tables_1",
  #   r_selection_drugs,
  #   data_selectivity, data_tas, data_targets, data_compounds,
  #   r_eligible_lspci_ids = reactive("all")
  # )
}
