#' Server module to display a set of download buttons in datatable instance
#'
#' @param dt Datatable instance before rendering
#' @param r_data Reactive containing the data to downlaod
#' @param type Either "csv" or "excel" determining the download file type
#' @param file_prefix Character, function or reactive to generate the filename prefix
#' @return Modified datatable instance
mod_server_download_button <- function(
  input, output, session,
  dt, r_data, type = c("csv", "excel"), file_prefix = function() paste0("dl-", Sys.Date())
) {
  ns <- session$ns
  dl_button <- downloadButton(
    ns("dl_button"),
    paste("Download", switch(type, csv = "CSV", excel = "Excel")),
    class = "dt-button"
  )
  insertUI("body", ui = dl_button, immediate = TRUE)
  add_button_js <- DT::JS(
    paste0(
      "$('#", ns("dl_wrapper"), "').siblings('.dt-buttons').append($('#", ns("dl_button"), "'));",
      "$('#", ns("dl_wrapper"), "').remove();"
    )
  )
  if (is.null(dt$x$options))
    dt$x$options <- list()
  if (is.null(dt$x$options$dom))
    dt$x$options$dom <- "lfrtipB"
  dt$x$options$dom <- paste0(dt$x$options$dom, '<"#', ns("dl_wrapper"), '">')
  if (!is.null(dt$x$options$initComplete)) {
    func_content <- dt$x$options$initComplete %>%
      stringr::str_split_fixed(stringr::fixed("{"), 2) %>%
      magrittr::extract(1, 2) %>%
      stringr::str_split_fixed(stringr::fixed("Shiny.bindAll();}"), 2) %>%
      magrittr::extract(1, 1)
    dt$x$options$initComplete <- DT::JS(
      paste0("function(settings, json) {", func_content, add_button_js, "Shiny.bindAll();}")
    )
  } else {
    dt$x$options$initComplete <- DT::JS(
      paste0("function(settings, json) {", add_button_js, "Shiny.bindAll();}")
    )
  }
  output$dl_button <- downloadHandler(
    filename = function()
      paste0(
        if(is.function(file_prefix))
          file_prefix()
        else
          file_prefix,
        switch(type, csv = ".csv", excel = ".xlsx")
      ),
    content = function(file) {
      switch(
        type,
        csv = write_csv(r_data(), file),
        excel = writexl::write_xlsx(r_data(), file)
      )
    }
  )
  dt
}
