zipped_csv <- function(df_list, zippedfile, filenames, stamp) {
  dir = tempdir()
  mkdir = paste0("mkdir ", dir, "/", stamp)
  system(mkdir)
  len = length(df_list)
  for (i in 1:len) {
    # filename in temp directory 
    assign(paste0("temp",i), paste0(dir, "/", stamp, "/", filenames[i], ".csv"))
    # write temp csv
    write_csv(df_list[[i]], path=get(paste0("temp",i)))
  }
  
  # zip temp csv
  print(dir)
  print(filenames)
  
  zip(zippedfile, paste0(dir,"/", stamp, "/", filenames, ".csv"), flags = "-j" )
  # delete temp csv
  for(i in 1:len) {
    unlink( paste0("temp",i) )
  }
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
