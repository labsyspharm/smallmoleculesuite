function(input, output, session) {
  selectivity <- callModule(
    selectivityServer, "selectivity"
  )  
  
  onBookmarked(function(url) {
    # url_list <- shiny::parseQueryString(url)
    # 
    # date_time <- format(Sys.time(), "%Y%m%d-%H%M%S")
    # rando_id <- substr(as.character(runif(1)), 3, 6)
    # 
    # # `new_id`
    # state_id <- glue::glue("{ app_name }-{ date_time }-{ id }")
    # 
    # url_list[[session$ns("bookmark")]]
    # new_url = gsub("\\?_inputs_.*", paste0("?bookmark=",new_id), url)
    # session$sendCustomMessage("bookmark_url", message = new_url)
    # values$url = new_url
    # input_list = reactiveValuesToList(input, all.names = T)
    # print("input_list")
    # print(names(input_list))
    # input_list_save = input_list[c("query_gene", "include_genes", "filter_button",
    #                                "affinity", "sd", "min_measurements",
    #                                "output_table_rows_selected")]
    # input_list_save$points_selected = values$points_selected
    # s3saveRDS(input_list_save, bucket = aws_bucket, object = paste0("sms_bookmarks/", new_id, "/", "input.rds"), check_region = F)
    # updateQueryString(new_url)
  })
}
