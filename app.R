source("global.R", local = TRUE)

router <- make_router(
  default = route("home", home_page),
  route(
    "binding",
    tagList(
      p(
        class = "lead",
        "Quick reference compound binding data."
      ) %>%
        font(align = "center", size = "sm") %>%
        margin(top = -1, b = 3),
      bindingDataUI(
        id = "binding"
      )
    )
  ),
  route(
    "selectivity",
    selectivityUI(
      id = "selectivity"
    )
  ),
  route(
    "similarity",
    similarityUI(
      id = "similarity"
    )
  ),
  route(
    "library",
    libraryUI(
      id = "library"
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
    id = "binding"
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
    module = libraryServer,
    id = "library"
  )
}

shinyApp(ui, server)
