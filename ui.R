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
      tags$title("Small Molecule Suite"),
      tags$link(href="https://fonts.googleapis.com/css?family=Lato:400,700&display=swap", rel="stylesheet"),
      tags$link(rel = "stylesheet", type = "text/css", href = "sms/css/slider.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "sms/css/main.css"),
      tags$script(src = "sms/js/main.js"),
      tags$link(rel = "icon", type = "image/png", href = "sms/assets/img/favicon.png")
    ),
    webpage(
      nav = navbar(
        tags$a(
          class = "navbar-brand",
          href="http://sorger.med.harvard.edu/",
          tags$img(class = "h-2", src = "sms/assets/img/logo.png")
        ),
        navInput(
          appearance = "pills",
          id = "nav",
          choices = list(
            list(icon("home"), "Home"),
            list(icon("circle", class = "selectivity--pink"), "Selectivity"),
            list(icon("circle", class = "similarity--green"), "Similarity"),
            list(icon("circle", class = "library--orange"), "Library"),
            "Download"
          ),
          values = c(
            "home",
            "selectivity",
            "similarity",
            "library",
            "download"
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
        tags$a(
          href = "https://forms.gle/dSpCJSsbaavTbCkP6",
          target = "_blank",
          icon("comments", class = "fa-lg"),
          " Feedback"
        ) %>%
          font(color = "white") %>%
          margin(l = 2),
        # buttonInput(
        #   id = "bookmark_begin",
        #   label = icon("link")
        # ) %>%
        #   background("black") %>%
        #   font(color = "white"),
        tags$a(
          href = "https://github.com/labsyspharm/sms-website",
          target = "_blank",
          icon("github", class = "fa-lg")
        ) %>%
          font(color = "white") %>%
          margin(l = 3)
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
                p(
                  "The Small Molecule Suite (SMS) is a free, open-acces tool developed by the",
                  a("Harvard Program in Therapeutic Sciences (HiTS)", href = "https://hits.harvard.edu", target = "_blank"),
                  "and", linkInput(id = "funding2", label = "funded by the NIH."),
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
                        mod_ui_set_library_vals_button("kinase_lib", "Kinases"),
                        mod_ui_set_library_vals_button("moa_lib", "MOA")
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
                              "I would like to know which small molecules are similar to my of interest"
                            )
                          )
                        ),
                        p(
                          linkInput(
                            id = "goto_binding",
                            label = list(
                              icon("share"),
                              "What are the targets of my compound?"
                            )
                          )
                        )
                      ) %>%
                        shadow("small"),
                      card(
                        h5("I want to download data—"),
                        p(
                          linkInput(
                            id = "goto_data_1",
                            label = list(
                              icon("share"),
                              "As flat CSV files"
                            )
                          )
                        ),
                        p(
                          linkInput(
                            id = "goto_data_2",
                            label = list(
                              icon("share"),
                              "As SQL database"
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
                        tags$img(src = "sms/assets/img/selectivity_logo.svg") %>%
                          height(10),
                        # icon("circle", class = "fa-6x selectivity--pink"),
                        h4("Selectivity") %>%
                          font(color = "black") %>%
                          margin(top = 2)
                      )
                    ),
                    p("Show the affinity and selectivity of compounds for a protein of interest.")
                  ) %>%
                    display("flex") %>%
                    flex(direction = "column") %>%
                    font(align = "center"),
                  column(
                    linkInput(
                      id = "link_similarity",
                      label = list(
                        tags$img(src = "sms/assets/img/similarity_logo.svg") %>%
                          height(10),
                        # icon("circle", class = "fa-6x similarity--green"),
                        h4("Similarity") %>%
                          font(color = "black") %>%
                          margin(top = 2)
                      )
                    ),
                    p("Show the similarity of compounds to a reference probe or drug.")
                  ) %>%
                    display("flex") %>%
                    flex(direction = "column") %>%
                    font(align = "center"),
                  column(
                    linkInput(
                      id = "link_library",
                      label = list(
                        tags$img(src = "sms/assets/img/library_logo.svg") %>%
                          height(10),
                        # icon("circle", class = "fa-6x text-orange"),
                        h4("Library") %>%
                          font(color = "black") %>%
                          margin(top = 2)
                      )
                    ),
                    p("Compose custom chemical genetics libraries for gene-sets of interest.")
                  ) %>%
                    display("flex") %>%
                    flex(direction = "column") %>%
                    font(align = "center")
                ),
                h1("Binding data") %>%
                  font(align = "center") %>%
                  margin(top = 5),
                p(
                  class = "lead",
                  "Quick reference compound binding data."
                ) %>%
                  font(align = "center", size = "sm") %>%
                  margin(top = -1, b = 3),
                bindingDataUI("bd") %>%
                  margin(top = 5, bottom = 5)
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
          navPane(
            id = "page_library",
            libraryUI(
              id = "lib"
            )
          ),
          # downloads ----
          navPane(
            id = "page_download",
            card(
              header = h4("Download Small Molecule Suite data"),
              p(
                "The entire Small Molecule Suite dataset is available for download.", tags$br(),
                "The data are organized in separate normalized tables. Documentation",
                "for each table and their relationship is available."
              ),
              a(
                h4("Table documentation", class = "btn btn-default btn-grey"),
                href = "https://dbdocs.io/clemenshug/sms_db",
                target = "_blank"
              )
            ) %>%
              margin(bottom = 3),
            columns(
              column(
                width = 6,
                card(
                  header = h4("SQL download"),
                  p(
                    "Gzip compressed SQL dump of the Small Molecule Suite database",
                    "in PostgreSQL format."
                  ),
                  p("Based on ChEMBL v25, size 799.9 MB"),
                  a(
                    h4("SQL database", class = "btn btn-default btn-grey"),
                    href = "sms/assets/downloads/sms_db_chembl_v25.sql.gz",
                    target = "_blank"
                  )
                )
              ),
              column(
                width = 6,
                card(
                  header = h4("CSV download"),
                  p(
                    "Tarball of gzip compressed CSV files."
                  ),
                  p("Based on ChEMBL v25, size 782.7 MB"),
                  a(
                    h4("CSV files", class = "btn btn-default btn-grey"),
                    href = "sms/assets/downloads/sms_tables_chembl_v25.tar"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
