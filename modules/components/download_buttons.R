#' Server module to display a set of download buttons
#'
#' @param r_data Reactive containing the data to downlaod
#' @param type Either "csv" or "excel" determining which
#'   download button is displayed
#' @param file_prefix Character, function or reactive to generate the filename prefix
mod_server_download_button <- function(
  input, output, session,
  r_data, type = c("csv", "excel"), file_prefix = function() paste0("dl-", Sys.Date())
) {
  output$download <- downloadHandler(
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
}

#' UI module to display a download button
#'
#' @param label Label for button
mod_ui_download_button <- function(id, label = "Download") {
  ns <- NS(id)
  downloadButton(ns("download"), label, class = "btn-outline-black")
}
