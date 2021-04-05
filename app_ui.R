NAV_ITEMS <- list(
  home = list(icon("home"), "Home"),
  binding = list(icon("circle"), "Binding"),
  selectivity = list(icon("circle", class = "selectivity--pink"), "Selectivity"),
  similarity = list(icon("circle", class = "similarity--green"), "Similarity"),
  library = list(icon("circle", class = "library--orange"), "Library"),
  download = "Download"
)

navbar_ui <- function() {
  navbar(
    brand = tags$a(
      href = "http://sorger.med.harvard.edu/",
      tags$img(
        src = "sms/assets/img/logo.png",
        style = "height: 2rem;"
      )
    ),
    class = "navbar-dark bg-dark",
    navInput(
      appearance = "pills",
      id = "tab",
      choices = unname(NAV_ITEMS),
      values = names(NAV_ITEMS)
    ) %>%
      margin(left = "auto"),
    tags$ul(
      class = "yonder-nav nav nav-pills",
      tagList(
        buttonInput(
          id = "about",
          label = "About",
          class = "nav-link btn-link"
        ),
        buttonInput(
          id = "funding",
          label = "Funding",
          class = "nav-link btn-link"
        ),
        tags$a(
          href = "https://forms.gle/dSpCJSsbaavTbCkP6",
          target = "_blank",
          icon("comments", class = "fa-lg"),
          "Feedback",
          class = "nav-link btn btn-link"
        ),
        bookmarkButton(
          label = "Share current view",
          icon = icon("share-square"),
          class = "btn-link nav-link"
        ),
        tags$a(
          href = "https://github.com/labsyspharm/sms-website",
          target = "_blank",
          icon("github", class = "fa-lg"),
          class = "nav-link btn btn-link"
        )
      ) %>%
        map(tags$li, class = "nav-item")
    ) %>%
      margin(left = 0)
  ) %>%
    padding(0, r = 3, l = 3) %>%
    margin(b = 4) %>%
    shadow()
}

home_page <- function() {
  container(
    centered = TRUE,
    columns(
      column(
        d3("The Small Molecule Suite") %>%
          font(align = "center"),
        p(
          "The Small Molecule Suite (SMS) is a free, open-acces tool developed by the",
          a("Harvard Program in Therapeutic Sciences (HiTS)", href = "https://hits.harvard.edu", target = "_blank"),
          "and", actionLink(inputId = "funding2", label = "funded by the NIH."),
          "The goal of the SMS is to help scientists understand and work with the targets of",
          "molecular probes, approved drugs and other drug-like molecules, while acknowliging the complexity of",
          "polypharmacology —	the phenomenon that virtually all drug-like molecules bind multiple target proteins.",
          "The SMS combines data from the", a("ChEMBL database", href = "https://www.ebi.ac.uk/chembl/", target = "_blank"),
          "with prepublished data from the Laboratory of Systems pharmacology.",
          "The methodology of calculating selectivities and similarities are explained in",
          a("Moret et al. Cell Chem Biol 2019", href = "https://doi.org/10.1016/j.chembiol.2019.02.018", target = "_blank"),
          "(which can also be used to cite the Small Molecule Suite).",
          style = "max-width: 50em;"
        ) %>%
          font(align = "justify") %>%
          margin(l = "auto", r = "auto", b = 5),
        columns(
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
                h5("I want pre-calculated optimized libraries for—"),
                tags$a(
                  type = "button",
                  class = "btn btn-primary",
                  icon("braille"),
                  "Kinome",
                  href = "?_inputs_&tab=%22library%22&library-gene_example=%22Kinome%22"
                ) %>%
                  tags$p(),
                tags$a(
                  type = "button",
                  class = "btn btn-primary",
                  icon("braille"),
                  "Liganded Proteome",
                  href = "?_inputs_&tab=%22library%22&library-gene_example=%22Full_LigandedGenome%22"
                ) %>%
                  tags$p()
              ) %>%
                shadow("small"),
              card(
                h5("I have a gene and—"),
                tags$a(
                  class = "btn btn-link text-left",
                  href = "?_inputs_&tab=%22selectivity%22",
                  icon("share"),
                  "I need a full list of small molecules that target a gene of interest."
                ) %>%
                  tags$p(),
                tags$a(
                  class = "btn btn-link text-left",
                  href = "?_inputs_&tab=%22library%22",
                  icon("share"),
                  "I need two orthogonal small molecules for a set of targets"
                ) %>%
                  tags$p()
              ) %>%
                shadow("small"),
              card(
                h5("I have a compound and—"),
                tags$a(
                  class = "btn btn-link text-left",
                  href = "?_inputs_&tab=%22similarity%22",
                  icon("share"),
                  "I would like to know which small molecules are similar to my of interest"
                ) %>%
                  tags$p(),
                tags$a(
                  class = "btn btn-link text-left",
                  href = "?_inputs_&tab=%22binding%22",
                  icon("share"),
                  "What are the targets of my compound?"
                ) %>%
                  tags$p()
              ) %>%
                shadow("small"),
              card(
                h5("I want to download data—"),
                tags$a(
                  class = "btn btn-link text-left",
                  href = "?_inputs_&tab=%22download%22",
                  icon("share"),
                  "As flat CSV files"
                ) %>%
                  tags$p(),
                tags$a(
                  class = "btn btn-link text-left",
                  href = "?_inputs_&tab=%22download%22",
                  icon("share"),
                  "As database tables"
                ) %>%
                  tags$p()
              ) %>%
                shadow("small")
            )
          )
        ) %>%
          margin(bottom = 5),
        columns(
          column(
            width = 12,
            h1("Applications") %>%
              margin(bottom = 3) %>%
              font(align = "center")
          ),
          column(
            tags$a(
              id = "link_selectivity",
              href = "?_inputs_&tab=%22selectivity%22",
              tags$img(src = "sms/assets/img/selectivity_logo.svg", style = "height: 10rem;"),
              h4("Selectivity") %>%
                margin(top = 2),
              "Show the affinity and selectivity of compounds for a protein of interest."
            ) %>%
              tags$div()
          ) %>%
            display("flex") %>%
            flex(direction = "column") %>%
            font(align = "center"),
          column(
            tags$a(
              id = "link_similarity",
              href = "?_inputs_&tab=%22similarity%22",
              tags$img(src = "sms/assets/img/similarity_logo.svg", style = "height: 10rem;"),
              h4("Similarity") %>%
                margin(top = 2),
              "Show the similarity of compounds to a reference probe or drug."
            ) %>%
              tags$div()
          ) %>%
            display("flex") %>%
            flex(direction = "column") %>%
            font(align = "center"),
          column(
            tags$a(
              id = "link_library",
              href ="?_inputs_&tab=%22library%22",
              tags$img(src = "sms/assets/img/library_logo.svg", style = "height: 10rem;"),
              h4("Library") %>%
                margin(top = 2),
              "Compose custom chemical genetics libraries for gene-sets of interest."
            ) %>%
              tags$div()
          ) %>%
            display("flex") %>%
            flex(direction = "column") %>%
            font(align = "center"),
        )
      )
    )
  )
}

download_page <- function() {
  tagList(
    columns(
      class = "justify-content-center",
      column(
        width = 10, style = "max-width: 80rem;",
        card(
          h4("Download Small Molecule Suite data"),
          h6(class = "text-muted mb-3", "SMS version based on ChEMBL v27"),
          p(
            "The entire Small Molecule Suite dataset is available for download.", tags$br(),
            "The data are organized in separate tables. Documentation",
            "for each table and their relationships are available."
          ),
          p(
            a(
              "Table documentation",
              class = "btn btn-outline-primary",
              href = "https://dbdocs.io/clemenshug/sms_db",
              target = "_blank"
            ),
            a(
              "Download tables from Synapse",
              class = "btn btn-outline-primary",
              target = "_blank",
              href = "https://www.synapse.org/#!Synapse:syn24874048"
            )
          ),
          div(
            class = "alert alert-warning", role = "alert",
            h5(class = "alert-heading", "Understanding compound and target identifiers"),
            p("Compounds and targets in all tables are referred to using ID numbers",
              "in the columns", tags$code("lspci_id"), "and", tags$code("lspci_target_id"), ",",
              "respectively."),
            p("Compound and target IDS can be translated into compound names and target",
              "symbols using the tables", tags$code("lsp_compound_dictionary"), "and",
              tags$code("lsp_target_dictionary"), "."),
            p("The table", tags$code("lsp_compound_dictionary"), "also contains mappings",
              "for the most common compound databases, such as ChEMBL, eMolecules and HMS LINCS.")
          )
        )
      )
    ) %>%
      margin(bottom = 3),
    columns(
      class = "justify-content-center",
      column(
        width = 10, style = "max-width: 80rem;",
        card(
          h5("Download tables in CSV format"),
          h6(class = "text-muted mb-3", "SMS version based on ChEMBL v27"),
          # p("All files are compressed using gzip."),
          tags$table(
            class = "table",
            tags$tr(
              tags$th("Name"), tags$th("Description"), tags$th("Size")
            ) %>%
              tags$thead(),
            map(
              list.files(here("www", "sms", "assets", "downloads"), full.name = TRUE) %>%
                magrittr::extract(str_detect(., fixed(".csv.gz"))),
              ~tags$tr(
                tags$td(
                  a(
                    .x %>%
                      basename() %>%
                      str_replace(fixed(".csv.gz"), ""),
                    href = file.path(
                      "sms", "assets", "downloads", basename(.x)
                    ),
                    target = "_blank"
                  )
                ),
                tags$td(
                  dl_table_descriptions %>%
                    filter(name == .x %>%
                             basename() %>%
                             str_replace(fixed(".csv.gz"), "")) %>%
                    pull(description)
                ),
                tags$td(
                  span(
                    class = "badge badge-secondary",
                    style = "font-size: 90%;",
                    R.utils::hsize(file.size(.x), standard = "SI")
                  )
                ),
              )
            ) %>%
              tags$tbody()
          )
        )
      )
    )
  )
}

page_headers <- function() {
  tagList(
    htmltools::htmlDependency(
      "font-awesome",
      "5.3.1", "www/shared/fontawesome",
      package = "shiny",
      stylesheet = c("css/all.min.css", "css/v4-shims.min.css")
    ),
    tags$head(
      tags$title("Small Molecule Suite"),
      tags$link(href = "https://fonts.googleapis.com/css?family=Lato:400,700&display=swap", rel = "stylesheet"),
      tags$link(rel = "stylesheet", type = "text/css", href = "sms/css/slider.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "sms/css/main.css"),
      tags$script(src = "sms/js/main.js"),
      tags$link(rel = "icon", type = "image/png", href = "sms/assets/img/favicon.png")
    )
  )
}

nav_content_ui <- function() {
  navContent(
    navPane(
      id = "home",
      home_page()
    ),
    navPane(
      id = "binding",
      tagList(
        h1(
          class = "text-center",
          "Compound affinity and binding assertions"
        ) %>%
          margin(b = 3),
        bindingDataUI(
          id = "binding"
        )
      )
    ),
    navPane(
      id = "selectivity",
      selectivityUI(
        id = "selectivity"
      )
    ),
    navPane(
      id = "similarity",
      similarityUI(
        id = "similarity"
      )
    ),
    navPane(
      id = "library",
      libraryUI(
        id = "library"
      )
    ),
    navPane(
      id = "download",
      download_page()
    )
  )
}
