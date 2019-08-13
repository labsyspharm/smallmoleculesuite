function(input, output, session) {
  observe({
    switch(
      input$nav,
      home = showNavPane("page_home"),
      selectivity = showNavPane("page_selectivity"),
      similarity = showNavPane("page_similarity"),
      library = showNavPane("page_library")
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
  
  observeEvent(input$link_selectivity, {
    showNavPane("page_selectivity")
    updateNavInput(
      id = "nav",
      selected = "selectivity"
    )
  })
  
  observeEvent(input$link_similarity, {
    showNavPane("page_similarity")
    updateNavInput(
      id = "nav",
      selected = "similarity"
    )
  })
  
  observeEvent(input$link_library, {
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
}
