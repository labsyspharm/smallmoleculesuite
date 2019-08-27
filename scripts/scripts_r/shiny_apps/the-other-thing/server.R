function(input, output, session) {
  observe({
    switch(
      input$nav,
      home = pushRoute("/home"),
      selectivity = pushRoute("/selectivity"),
      similarity = pushRoute("/similarity"),
      library = pushRoute("/library")
    )
  }) 

  .modal_body <- modal(
    id = NULL,
    size = "lg",
    header = h5("About"),
    HTML(htmltools::includeMarkdown("inst/about.md"))
  )
  observeEvent(input$about, {
    showModal(.modal_body)
  })
  
  observeRoute("/home", {
    showNavPane("page_home")
  })
  
  observeEvent(c(input$link_selectivity, input$goto_selectivity_1), {
    pushRoute("/selectivity")
  })
  
  observeRoute("/selectivity", {
    showNavPane("page_selectivity")
    updateNavInput(
      id = "nav",
      selected = "selectivity"
    )
  })
  
  observeEvent(c(input$link_similarity, input$goto_similarity_1, input$goto_similarity_2), {
    pushRoute("/similarity")
  })
  
  observeRoute("/similarity", {
    showNavPane("page_similarity")
    updateNavInput(
      id = "nav",
      selected = "similarity"
    )
  })
  
  observeEvent(c(input$link_library, input$goto_library_1), {
    pushRoute("/library")
  })
  
  observeRoute("/library", {
    showNavPane("page_library")
    updateNavInput(
      id = "nav",
      selected = "library"
    )
  })
  
  callModule(
    module = selectivityServer,
    id = "select"
  )
  
  callModule(
    module = similarityServer,
    id = "sim"
  )
  
  callModule(
    module = libraryServer,
    id = "lib"
  )
  
  .modal_bookmark <- modal(
    # id = "modal_bookmark",
    id = NULL,
    textInput(
      id = "bookmark_name",
      value = "my_save"
    ),
    buttonInput(id = "bookmark_done", "Done")
  )
  
  observeEvent(input$bookmark_begin, {
    showModal(.modal_bookmark)
  })
  
  observeEvent(input$bookmark_done, {
    closeModal()
    
    f_name <- paste0(paste(replicate(4, sample(1000, 1)), collapse = ""), ".rds")
    
    f_path <- fs::file_create(fs::path_home("Downloads", f_name))
    
    saveRDS(shiny::reactiveValuesToList(path), path)
    
    pushQuery(bookmark = )
  })
  
  onRestored(function(state) {
    qs <- getQueryString()
    
    save_name <- qs$q
    
    if (!is.null(.SAVE_STATE[[save_name]])) {
      print(.SAVE_STATE[[save_name]])
    }
  })

  onBookmarked(function(url) {
    save_name <- input$bookmark_name
    
    .SAVE_STATE[[save_name]] <- reactiveValuesToList(input)
    
    updateQueryString(paste0("?q=", save_name), "replace")
  })
}
