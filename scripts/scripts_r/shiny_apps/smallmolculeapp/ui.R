function(req) {
  list(
    htmltools::htmlDependency(
      "font-awesome",
      "5.3.1", "www/shared/fontawesome", package = "shiny",
      stylesheet = c("css/all.min.css", "css/v4-shims.min.css")
    ),
    DT:::DTDependency("default"),
    DT:::extDependency("Buttons", "default", list()),
    tags$head(
      tags$link(href="https://fonts.googleapis.com/css?family=Lato:400,700&display=swap", rel="stylesheet"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/slider.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
      tags$script(src = "js/main.js")
    ),
    webpage(
      nav = navbar(
        tags$a(
          class = "navbar-brand",
          href="http://sorger.med.harvard.edu/",
          tags$img(class = "h-2", src = "../assets/img/logo.png")
        ),
        navInput(
          appearance = "pills",
          id = "nav",
          choices = list(
            list(icon("home"), "Home"),
            list(icon("circle", class = "selectivity--pink"), "Selectivity"),
            list(icon("circle", class = "similarity--green"), "Similarity"),
            list(icon("circle", class = "library--orange"), "Library")
          ),
          values = c(
            "home",
            "selectivity",
            "similarity",
            "library"
          )
        ) %>%
          margin(left = "auto"),
        buttonInput(
          id = "about",
          label = "About"
        ) %>%
          background("black") %>%
          font(color = "white"),
        buttonInput(
          id = "funding",
          label = "Funding"
        ) %>%
          background("black") %>%
          font("white"),
        buttonInput(
          id = "bookmark_begin",
          label = icon("link")
        ) %>%
          background("black") %>%
          font(color = "white"),
        tags$a(
          href = "https://github.com/labsyspharm/smallmoleculesuite",
          target = "_blank",
          icon("github", class = "fa-lg")
        ) %>%
          font(color = "white") %>%
          margin(l = 2)
      ) %>%
        active("red") %>%
        padding(0, r = 3, l = 3) %>%
        margin(b = 4) %>%
        background("black") %>%
        shadow(),
      container(
        navContent(
          # home ----
          navPane(
            id = "page_home",
            fade = FALSE,
            class = "active",
            columns(
              column(
                d3("The Small Molecule Suite") %>%
                  font(align = "center"),
                p("An open-access tool developed by the Harvard Program",
                  "in Therapeutic Sciences (HiTS) and",
                  linkInput(id = "funding2", label = "funded by the NIH")) %>%
                  font(align = "center") %>%
                  margin(b = 5),
                columns( # ├ use cases ----
                  column(
                    width = 12,
                    h1("Use cases") %>%
                      margin(bottom = 3) %>%
                      font(align = "center")
                  ),
                  column(
                    width = 12,
                    deck(
                      card(
                        h5("I want pre-calculated libraries—"),
                        p(
                          div(
                            class = "shadow-sm d-inline-block",
                            tags$button(
                              class = "btn btn-blue shadow-sm",
                              type = "button",
                              `data-toggle` = "modal",
                              `data-target` = "#modal_optimal_kinase",
                              icon("window-restore"),
                              "Kinases"
                            )
                          ),
                          optimalKinaseModal()
                        ),
                        p(
                          div(
                            class = "shadow-sm d-inline-block",
                            tags$button(
                              class = "btn btn-blue shadow-sm",
                              type = "button",
                              `data-toggle` = "modal",
                              `data-target` = "#modal_moa",
                              icon("window-restore"),
                              "MOA"
                            )
                          ),
                          moaModal()
                        )
                      ) %>%
                        shadow("small"),
                      card(
                        h5("I have a gene and—"),
                        p(
                          linkInput(
                            id = "goto_selectivity_1",
                            label = list(
                              icon("share"),
                              "I need a full list of small molecules that target a gene of interest."
                            )
                          )
                        ),
                        p(
                          linkInput(
                            id = "goto_library_1",
                            label = list(
                              icon("share"),
                              "I need two orthogonal small molecules for a set of targets"
                            )
                          )
                        )
                      ) %>%
                        shadow("small"),
                      card(
                        h5("I have a compound and—"),
                        p(
                          linkInput(
                            id = "goto_similarity_1",
                            label = list(
                              icon("share"),
                              "I would like to know which small molecules are similar to a small molecule of interest"
                            )
                          )
                        ),
                        p(
                          linkInput(
                            id = "goto_similarity_2",
                            label = list(
                              icon("share"),
                              "What are the targets?"
                            )
                          )
                        )
                      ) %>%
                        shadow("small")
                    )
                  )
                ) %>%
                  margin(bottom = 5),
                columns( # ├ applications ----
                  column(
                    width = 12,
                    h1("Applications") %>%
                      margin(bottom = 3) %>%
                      font(align = "center")
                  ),
                  column(
                    linkInput(
                      id = "link_selectivity",
                      label = list(
                        tags$img(src = "assets/img/helix.png") %>%
                          height(10),
                        # icon("circle", class = "fa-6x selectivity--pink"),
                        h4("Selectivity") %>%
                          font(color = "black") %>%
                          margin(top = 2)
                      )
                    ),
                    p("Selectivity shows the affinity and selectivity of compounds in the HMS-LINCS collection for a gene of interest.")
                  ) %>%
                    display("flex") %>%
                    flex(direction = "column") %>%
                    font(align = "center"),
                  column(
                    linkInput(
                      id = "link_similarity",
                      label = list(
                        tags$img(src = "assets/img/molecule.png") %>%
                          height(10),
                        # icon("circle", class = "fa-6x similarity--green"),
                        h4("Similarity") %>%
                          font(color = "black") %>%
                          margin(top = 2)
                      )
                    ),
                    p("Similarity shows the similarity of compounds in the HMS-LINCS collection to a reference compound.")
                  ) %>%
                    display("flex") %>%
                    flex(direction = "column") %>%
                    font(align = "center"),
                  column(
                    linkInput(
                      id = "link_library",
                      label = list(
                        tags$img(src = "assets/img/flask.png") %>%
                          height(10),
                        # icon("circle", class = "fa-6x text-orange"),
                        h4("Library") %>%
                          font(color = "black") %>%
                          margin(top = 2)
                      )
                    ),
                    p("Library composes custom chemical genetics libraries for gene-sets of interest.")
                  ) %>%
                    display("flex") %>%
                    flex(direction = "column") %>%
                    font(align = "center")
                ),
                # columns( # ├ binding data ----
                #   column(
                #     width = 12,
                #     h1("Binding data") %>%
                #       font(align = "center"),
                #     p(class = "lead",
                #       "Quick reference compound binding data.") %>%
                #       font(align = "center", size = "sm") %>%
                #       margin(top = -1, b = 3)
                #   ),
                #   column(
                #     width = 12,
                #     bindingDataUI("bd")
                #   )
                # ) %>%
                #   margin(top = 5, bottom = 5)
              )
            )
          ),
          # selectivity ----
          navPane(
            id = "page_selectivity",
            selectivityUI(
              id = "select"
            )
          ),
          # similarity ----
          navPane(
            id = "page_similarity",
            similarityUI(
              id = "sim"
            )
          ),
          # library ----
          # navPane(
          #   id = "page_library",
          #   libraryUI(
          #     id = "lib"
          #   )
          # )
        )
      )
    )
  )
}
