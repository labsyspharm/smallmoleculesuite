NAV_ITEMS <- list(
  home = list(icon("home"), "Home"),
  binding = list(icon("circle"), "Binding"),
  selectivity = list(icon("circle", class = "selectivity--pink"), "Selectivity"),
  similarity = list(icon("circle", class = "similarity--green"), "Similarity"),
  library = list(icon("circle", class = "library--orange"), "Library"),
  download = "Download"
)

navbar_ui <- navbar(
  brand = tags$a(
    href = "http://sorger.med.harvard.edu/",
    tags$img(
      src = "sms/assets/img/logo.png",
      style = "height: 2rem;"
    )
  ),
  class = "navbar-dark bg-dark",
  tags$ul(
    class = "yonder-nav nav nav-pills navbar-nav",
    imap(
      NAV_ITEMS,
      ~tags$li(
        class = "nav-item",
        tags$a(
          `data-route` = .y,
          class = paste(
            "nav-link btn btn-link",
            if (.y == "home") "active"
          ),
          href = route_link(.y),
          .x
        )
      )
    ),
    list(
      buttonInput(
        id = "about",
        label = "About",
        class ="nav-link btn-link"
      ),
      buttonInput(
        id = "funding",
        label = "Funding",
        class ="nav-link btn-link"
      ),
      tags$a(
        href = "https://forms.gle/dSpCJSsbaavTbCkP6",
        target = "_blank",
        icon("comments", class = "fa-lg"),
        "Feedback",
        class = "nav-link btn btn-link"
      ),
      bookmarkButton(),
      tags$a(
        href = "https://github.com/labsyspharm/sms-website",
        target = "_blank",
        icon("github", class = "fa-lg"),
        class = "nav-link btn btn-link"
      )
    ) %>%
      map(tags$li, class ="nav-item")
  ) %>%
    margin(left = "auto")
) %>%
  padding(0, r = 3, l = 3) %>%
  margin(b = 4) %>%
  shadow() %>%
  tagList(
    tags$script(
      r"{
      const nav_links = $(".nav .nav-link");
      var switchNav = function(message) {
        const routes = $("#router-page-wrapper").find(".router");
        var active_route = routes.filter(function() {
          return $(this).data("path") == message;
        });
        var active_link = nav_links.filter(function() {
          return $(this).data("route") == message;
        });
        nav_links.removeClass("active");
        active_link.addClass("active");
        routes.addClass('router-hidden');
        active_route.removeClass('router-hidden');
      };
      Shiny.addCustomMessageHandler("switch-ui", switchNav);
      }"
    )
  )
  # tagList(
  #   tags$script(
  #     r"{
  #       $(".nav .nav-link").on("click", function(){
  #         const all_active = $(this).parent().closest(".nav").find(".active");
  #         all_active.removeClass("active");
  #         $(this).addClass("active");
  #       });
  #     }"
  #   )
  # )

home_page <- container(
  cantered = TRUE,
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
              p(mod_ui_set_library_vals_button("kinase_lib", "Kinases")),
              p(mod_ui_set_library_vals_button("moa_lib", "MOA"))
            ) %>%
              shadow("small"),
            card(
              h5("I have a gene and—"),
              tags$a(
                class = "btn btn-link text-left",
                href = route_link("selectivity"),
                icon("share"),
                "I need a full list of small molecules that target a gene of interest."
              ) %>%
                tags$p(),
              tags$a(
                class = "btn btn-link text-left",
                href = route_link("library"),
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
                href = route_link("similarity"),
                icon("share"),
                "I would like to know which small molecules are similar to my of interest"
              ) %>%
                tags$p(),
              tags$a(
                class = "btn btn-link text-left",
                href = route_link("binding"),
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
                href = route_link("download"),
                icon("share"),
                "As flat CSV files"
              ) %>%
                tags$p(),
              tags$a(
                class = "btn btn-link text-left",
                href = route_link("download"),
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
      columns( # ├ applications ----
        column(
          width = 12,
          h1("Applications") %>%
            margin(bottom = 3) %>%
            font(align = "center")
        ),
        column(
          tags$a(
            id = "link_selectivity",
            href = route_link("selectivity"),
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
            href = route_link("similarity"),
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
            href = route_link("library"),
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

download_page <- div(
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

page_headers <- tagList(
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