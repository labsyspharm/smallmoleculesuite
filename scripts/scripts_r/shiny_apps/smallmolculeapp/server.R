function(input, output, session) {
  navToPage <- function(name) {
    updateNavInput("nav", selected = name)
    showNavPane(paste0("page_", name))
  }

  observeEvent(input$nav, {
    navToPage(input$nav)
  })

  .modal_about <- modal(
    id = NULL,
    size = "lg",
    header = h5("About"),
    HTML(htmltools::includeMarkdown("inst/about.md"))
  )
  observeEvent(input$about, {
    showModal(.modal_about)
  })

  .modal_funding <- modal(
    id = NULL,
    size = "md",
    header = h5("Funding"),
    p("This open-access webtool is funded by NIH grants U54-HL127365, U24-DK116204 and U54-HL127624.")
  )
  observeEvent(c(input$funding, input$funding2), {
    showModal(.modal_funding)
  })

  observeEvent(c(input$link_selectivity, input$goto_selectivity_1), {
    navToPage("selectivity")
  })

  observeEvent(c(input$link_similarity, input$goto_similarity_1, input$goto_similarity_2), {
    navToPage("similarity")
  })

  observeEvent(c(input$link_library, input$goto_library_1), {
    navToPage("library")
  })

  callModule(
    module = selectivityServer,
    id = "select"
  )

  callModule(
    module = similarityServer,
    id = "sim"
  )

  # callModule(
  #   module = libraryServer,
  #   id = "lib",
  #   load_example = reactive({
  #     req(input$nav == "library")
  #   })
  # )

  callModule(
    module = bindingDataServer,
    id = "bd"
  )
}
