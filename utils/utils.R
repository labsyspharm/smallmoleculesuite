
create_download_filename <- function(prefix, suffix = NULL) {
  full <- c(prefix, "%s", suffix)
  formatted <- str_replace_all(tolower(full), "\\s+", "-")
  collapse <- paste0(formatted, collapse = "-")

  sprintf(collapse, Sys.Date())
}

`%||%` <- function(a, b) if (is.null(a)) b else a

fast_search <- function(data, req) {
  setDT(data)

  query <- shiny::parseQueryString(req$QUERY_STRING)

  # extract the query variables, conjunction (and/or), search string, maximum options
  var <- c(shiny:::safeFromJSON(query$field))

  key <- unique(strsplit(query$query, '\\s+')[[1]])

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
          str_detect(data[[v]], fixed(k, ignore_case = TRUE), negate = FALSE)
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
  if (length(sel)) {
    i <- stats::na.omit(match(sel, data[[vfd]]))
    if (length(i)) idx <- sort(utils::head(unique(c(i, idx)), mop))
  }

  data_out <- data[
    idx,
  ][
    # Make sure only the first result for every lspci_id is returned
    , .SD[1, ], by = lspci_id
  ]

  res <- shiny:::toJSON(shiny:::columnToRowData(data_out))
  shiny:::httpResponse(200, 'application/json', enc2utf8(res))
}

table_inputs <- function(table_name) {
  suffixes <- c(
    "_rows_selected",
    "_rows_all",
    "_rows_current",
    "_columns_selected",
    "_search",
    "_cells_selected",
    "_cell_clicked",
    "_state"
  )
  inputs <- outer(table_name, suffixes, FUN = "paste0")
  dim(inputs) <- NULL
  inputs
}
