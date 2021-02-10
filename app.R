source("global.R", local = TRUE)

router <- make_router(
  route("/", home_page),
  route(
    "selectivity",
    selectivityUI(
      id = "select"
    )
  ),
  route(
    "similarity",
    similarityUI(
      id = "sim"
    )
  ),
  route(
    "library",
    libraryUI(
      id = "lib"
    )
  ),
  route("download", download_page)
)

ui <- tagList(
  page_headers,
  webpage(
    nav = navbar_ui,
    router$ui
  )
)

server <- function(input, output, session) {

  router$server(input, output, session)

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

  callModule(
    module = bindingDataServer,
    id = "binding_data"
  )

  callModule(
    module = selectivityServer,
    id = "selectivity"
  )

  callModule(
    module = similarityServer,
    id = "similarity"
  )

  callModule(
    module = selectivityServer,
    id = "selectivity"
  )

  callModule(
    module = libraryServer,
    id = "library"
  )
}

shinyApp(ui, server)
