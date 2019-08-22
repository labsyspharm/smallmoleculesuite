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

  .modal_body <- HTML(htmltools::includeMarkdown("inst/about.md"))  
  observeEvent(input$about, {
    showModal(
      modal(
        id = NULL,
        size = "lg",
        title = "About",
        .modal_body
      )
    )
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
  
  # observeEvent(input$bookmark, {
  #   session$doBookmark()
  # })
  # 
  # onBookmarked(function(url) {
  #   updateQueryString(url)
  # })
}
