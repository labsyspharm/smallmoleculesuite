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
