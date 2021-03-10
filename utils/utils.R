
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

  if (length(var) > 1)
    stop("More than one search column not supported: ", var)

  if (length(key) > 1)
    stop("More than one key not supported: ", key)

  # Make sure selection is also in results
  # sel_match <- chmatch(sel, data[[vfd]])
  # key_match <- str_which(data[[var]], fixed(key, ignore_case = TRUE))
  # data_out <- data[
  #   unique(
  #     c(
  #       sel_match,
  #       key_match
  #     )
  #   )
  # ][
  #   ,
  #   match_len := str_length(get(var))
  # [
  #   o
  # ] %>%
  #   head(n = mop)
  # Make sure selection is also in results
  sel_match <- if (length(sel)) data[[vfd]] == sel else logical(nrow(data))
  key_match <- if (length(key)) str_detect(data[[var]], fixed(key, ignore_case = TRUE)) else logical(nrow(data))
  both_match <- sel_match | key_match
  data_out <- data[
    both_match
  ][
    ,
    match_len := str_length(get(var))
  ][
    order(
      !sel_match[both_match],
      match_len
    )
  ][
    ,
    match_len := NULL
  ]
  # data_out <- data_out[
  #   order(
  #     !sel_match,
  #
  #   )
  # ] %>%
  #   head(n = mop)

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
