
#
# navbar_server <- function(input, output, session) {
#   .modal_about <- modal(
#     id = NULL,
#     size = "lg",
#     header = h5("About"),
#     HTML(htmltools::includeMarkdown("inst/about.md"))
#   )
#   observeEvent(input$about, {
#     showModal(.modal_about)
#   })
#
#   .modal_funding <- modal(
#     id = NULL,
#     size = "md",
#     header = h5("Funding"),
#     p("This open-access webtool is funded by NIH grants U54-HL127365, U24-DK116204 and U54-HL127624.")
#   )
#   observeEvent(c(input$funding, input$funding2), {
#     showModal(.modal_funding)
#   })
# }
#
# home_server <- function(input, output, session) {
#   callModule(
#     module = bindingDataServer,
#     id = "binding_data"
#   )
# }
#
# selectivity_server <- function(input, output, session) {
#   callModule(
#     module = selectivityServer,
#     id = "selectivity"
#   )
# }
#
# similarity_server <- function(input, output, session) {
#   callModule(
#     module = similarityServer,
#     id = "similarity"
#   )
# }
#
# selectivity_server <- function(input, output, session) {
#   callModule(
#     module = selectivityServer,
#     id = "selectivity"
#   )
# }
#
# library_server <- function(input, output, session) {
#   callModule(
#     module = libraryServer,
#     id = "library"
#   )
# }

