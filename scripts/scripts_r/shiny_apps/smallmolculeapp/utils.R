
create_download_filename <- function(prefix, suffix = NULL) {
  full <- c(prefix, "%s", suffix)
  formatted <- stringr::str_replace_all(tolower(full), "\\s+", "-")
  collapse <- paste0(formatted, collapse = "-")

  sprintf(collapse, Sys.Date())
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

fast_search <- function(data, req) {
  query <- shiny::parseQueryString(req$QUERY_STRING)

  # extract the query variables, conjunction (and/or), search string, maximum options
  var <- c(shiny:::safeFromJSON(query$field))

  # all keywords in lower-case, for case-insensitive matching
  key <- unique(strsplit(tolower(query$query), '\\s+')[[1]])

  data_lower <- attr(data, "lower", exact = TRUE)
  if (is.null(data_lower)) {
    data_lower <- data %>%
      mutate_if(is.character, stri_trans_tolower)
    setattr(data, "lower", data_lower)
  }

  if (identical(key, '')) key <- character(0)
  mop <- as.numeric(query$maxop)
  vfd <- query$value  # the value field name
  sel <- attr(data, 'selected_value', exact = TRUE)

  # start searching for keywords in all specified columns
  idx <- logical(nrow(data))
  if (length(key)) {
    for (v in var) {
      matches <- do.call(
        cbind,
        lapply(key, function(k) {
          stri_detect_fixed(data_lower[[v]], k, negate = FALSE)
        })
      )
      # merge column matches using OR, and match multiple keywords in one column
      # using the conjunction setting (AND or OR)
      matches <- rowSums(matches)
      if (query$conju == 'and')
        idx <- idx | (matches == length(key))
      else
        idx <- idx | matches
    }
  }
  # only return the first n rows (n = maximum options in configuration)
  idx <- utils::head(if (length(key)) which(idx) else seq_along(idx), mop)
  # make sure the selected value is in the data
  # if (length(sel)) {
  #   i <- stats::na.omit(match(sel, data[, vfd]))
  #   if (length(i)) idx <- sort(utils::head(unique(c(i, idx)), mop))
  # }
  data <- data[idx, ]

  res <- shiny:::toJSON(shiny:::columnToRowData(data))
  shiny:::httpResponse(200, 'application/json', enc2utf8(res))
}

dt_add_download_button <- function(
  dt, id, output, r_data, fn_prefix, type = c("csv", "excel")
) {
  type <- match.arg(type)
  button_id <- paste0(id, "_button")
  wrapper_id <- paste0(id, "_wrapper")
  dl_button <- downloadButton(button_id, paste("Download", toupper(type)), class = "dt-button")
  dl_button_str <- dl_button %>%
    as.character() %>%
    stringr::str_replace_all(stringr::fixed("\n"), "")
  add_button_js <- DT::JS(paste0("$('#", wrapper_id, "').append($('", dl_button_str, "'));"))
  if (is.null(dt$x$options))
    dt$x$options <- list()
  if (is.null(dt$x$options$dom))
    dt$x$options$dom <- "lfrtipB"
  dt$x$options$dom <- paste0(dt$x$options$dom, '<"#', wrapper_id, '">')
  if (!is.null(dt$x$options$initComplete)) {
    func_content <- dt$x$options$initComplete %>%
      stringr::str_split_fixed(stringr::fixed("{"), 2) %>%
      magrittr::extract(1, 2) %>%
      stringr::str_split_fixed(stringr::fixed("}"), 2) %>%
      magrittr::extract(1, 1)
    dt$x$options$initComplete <- DT::JS(
      paste0("function(settings, json) {", func_content, add_button_js, "}")
    )
  } else {
    dt$x$options$initComplete <- DT::JS(
      paste0("function(settings, json) {", add_button_js, "}")
    )
  }
  dl_handler <- downloadHandler(
    filename = function() {
      paste0(fn_prefix, switch(type, csv = ".csv", excel = ".xlsx"))
    },
    content = function(file) {
      browser()
      switch(
        type,
        csv = write_csv(r_data(), file),
        excel = writexl::write_xlsx(r_data(), file)
      )
    }
  )
  output[[button_id]] <- dl_handler
  browser()
  dt
}
