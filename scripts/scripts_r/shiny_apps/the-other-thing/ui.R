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
  container(
    navbar(
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
      margin(b = 4, r = -3, l = -3) %>% 
      background("black") %>% 
      shadow(),
    navContent(
      # home ----
      navPane(
        id = "page_home",
        columns(
          column(
            width = 2
          ),
          column(
            h3("The Small Molecule Suite") %>% 
              font(align = "center"),
            p("Developed by the Harvard Initiative in Therapeutic Sciences (HiTS)") %>% 
              font(align = "center") %>% 
              margin(b = 5),
            columns(
              column(
                linkInput(
                  id = "link_selectivity",
                  label = list(
                    icon("circle", class = "fa-6x selectivity--pink"),
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
                    icon("circle", class = "fa-6x similarity--green"),
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
                    icon("circle", class = "fa-6x text-orange"),
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
            )
          ),
          column(
            width = 2
          )
        ) %>% 
          margin(top = 5)
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
      navPane(
        id = "page_library",
        libraryUI(
          id = "lib"
        )
      )
    )
  )
)
