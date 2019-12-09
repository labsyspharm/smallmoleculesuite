create_bookmark_id <- function() {
  date_time <- format(Sys.time(), "%Y%m%d-%H%M%S")
  id <- substr(as.character(runif(1)), 3, 6)
  paste0(date_time, "-", id)
}

write_zip <- function(contents, path) {
  stopifnot(class(contents) == "list")
  
  contents_paths <- Map(df = contents, file = names(contents), function(df, file) {
    temp_file <- fs::path_temp(file)
    readr::write_csv(df, temp_file)
    temp_file
  })
  
  contents_paths <- vapply(contents_paths, as.character, character(1))
  
  on.exit({
    fs::file_delete(contents_paths)
  })
  
  zip(path, contents_paths, flags = "-j")
}

makeQueryString <- function(lst) {
  paste0("?", paste0(names(lst), "=", unlist(lst), collapse = "&"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

dataTableOutput <- function(outputId, width = "100%", height = "auto") {
  # list(
  #   DT::datatable(data.frame())$dependencies,
  #   htmltools::singleton(
  #     tags$head(
  #       tags$script(src = "https://cdn.datatables.net/1.10.19/js/jquery.dataTables.min.js"),
  #       tags$script(src = "https://cdn.datatables.net/1.10.19/js/dataTables.bootstrap4.min.js"),
  #       tags$link(rel = "stylesheet", href = "https://cdn.datatables.net/1.10.19/css/dataTables.bootstrap4.min.css")
  #     )
  #   ),
  #   htmltools::suppressDependencies(
  #     "datatables-css", "dt-core", "dt-core-bootstrap"
  #   ),
  #   DT::dataTableOutput(
  #     outputId = outputId,
  #     width = width,
  #     height = height
  #   )
  # )
  
  DT::dataTableOutput(
    outputId = outputId,
    width = width,
    height = height
  )
}

restore_input <- function(id, value, session) {
  fun <- switch(
    id,
    `lib-table_display` = updateRadiobarInput,
    # `goto_library_1` = 
    # `goto_similarity_1` = 
    # `goto_similarity_2` = 
    `select-include_genes` = updateCheckboxInput,
    `lib-gene_list` = updateTextAreaInput,
    # `about` = 
    `lib-filter_measurement` = updateSliderInput,
    `lib-filter_sd` = updateSliderInput,
    # `sim-compound_selection` = updateListGroupInput,
    # `lib-nav` = 
    # `goto_selectivity_1` = 
    `lib-filter_affinity` = updateSliderInput,
    `select-sd` = updateSliderInput,
    `lib-filter_expert` = updateSliderInput,
    # `select-select_gene` = updateListGroupInput
    `lib-filter_phase` = updateCheckboxGroupInput,
    # `sim-nav` = 
    `sim-n_common` = updateSliderInput,
    `sim-n_pheno` = updateSliderInput,
    `select-min_measurements` = updateSliderInput,
    `lib-filter_probes` = updateCheckboxInput,
    `sim-query_compound` = updateSelectInput,
    `lib-gene_example` = updateSelectInput,
    `select-query_gene` = updateSelectInput,
    `select-affinity` = updateSliderInput
    # `lib-gene_form` = 
  )
  
  if (is.null(fun)) {
    return()
  }
  
  if ("inputId" %in% names(as.list(args(fun)))) {
    if (is.null(value)) {
      return()
    }
    
    fun(inputId = id, value = value, session = session)
  } else {
    fun(id = id, selected = value, session = session)
  }
}
