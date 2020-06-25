function(input, output, session) {
  navToPage <- function(name, session = NULL) {
    if (is.null(session))
      session <- getDefaultReactiveDomain()
    updateNavInput("nav", selected = name, session = session)
    showNavPane(paste0("page_", name), session = session)
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

  observeEvent(c(input$link_similarity, input$goto_similarity_1), {
    navToPage("similarity")
  })

  observeEvent(input$goto_binding, callModule(mod_server_scroll_binding, "bd"))

  observeEvent(c(input$goto_data_1, input$goto_data_2), {
    navToPage("download")
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

  library_session <- callModule(
    module = libraryServer,
    id = "lib",
    load_example = reactive({
      req(input$nav)
      input$nav == "library"
    })
  )

  callModule(
    module = bindingDataServer,
    id = "bd"
  )

  callModule(
    mod_server_set_library_vals_button, "kinase_lib",
    library_session = library_session, vals = c("gene_example" = "Kinome"),
    finish_callback = function() {
      updateFormInput("submit", submit = TRUE, session = library_session)
      navToPage("library", session = session)
    }
  )

  callModule(
    mod_server_set_library_vals_button, "moa_lib",
    library_session = library_session, vals = c("gene_example" = "Full_LigandedGenome"),
    finish_callback = function() {
      updateFormInput("submit", submit = TRUE, session = library_session)
      navToPage("library", session = session)
    }
  )
}
