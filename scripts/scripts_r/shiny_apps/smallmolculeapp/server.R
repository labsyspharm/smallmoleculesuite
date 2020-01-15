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
  
  callModule(
    module = libraryServer,
    id = "lib",
    load_example = reactive({
      req(input$nav == "library")
    })
  )
  
  callModule(
    module = bindingDataServer,
    id = "bd"
  )
  
  observeEvent(input$bookmark_begin, {
    closeModal()
    
    bookmark_id <- create_bookmark_id()
    bookmark_url <- paste0("?bookmark=", bookmark_id)

    input_list <- reactiveValuesToList(input, all.names = TRUE)
    
    # input_list_save = input_list[c("filter_button",
    #                                "probes", "clinical", "legacy",
    #                                "affinity", "meas", "sd"
    # )]
    
    aws.s3::s3saveRDS(
      x = input_list, 
      bucket = "small-molecule-suite", 
      object = paste0("sms_bookmarks/", bookmark_id, "/", "input.rds"), 
      check_region = FALSE
    )
    
    showModal(
      modal(
        id = NULL,
        header = "Session bookmarked",
        p("Your bookmark id is ", bookmark_id)
      )
    )
    
    updateQueryString(bookmark_url)
  })
  
  onRestored(function(state) {
    qs <- getQueryString()
    
    bookmark_id <- qs$bookmark

    s3_path <- paste0("sms_bookmarks/", bookmark_id, "/input.rds")
    
    if (!aws.s3::object_exists(s3_path, "small-molecule-suite", check_region = FALSE)) {
      showModal(
        modal(
          id = NULL,
          title = h3("Invalid bookmark id"),
          p("Sorry, the bookmark id does not appear to exist.")
        )
      )
      return()
    }
    
    stored_inputs <- aws.s3::s3readRDS(
      object = s3_path,
      bucket = "small-molecule-suite",
      check_region = FALSE
    )
    
    Map(
      id = names(stored_inputs), 
      value = stored_inputs, 
      session = list(session),
      f = restore_input
    )
    
    showModal(modal(id = NULL, header = h3("Session restored")))
  })
  
}
