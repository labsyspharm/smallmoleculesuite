similarityUI <- function(id) {
  ns <- NS(id)
  
  columns(
    column(
      width = 4,
      card(
        header = navInput(
          appearance = "tabs",
          id = ns("nav"),
          choices = c("Filters", "Instructions"),
          values = c("filters", "instructions"),
          selected = "filters"
        ),
        navContent(
          navPane(
            id = ns("pane_filters"),
            formGroup(
              label = "Select reference compound",
              input = selectInput(
                id = ns("query_compound")
              ),
              help = "Search for a compound"
            ),
            formGroup(
              label = "Number of biological assays in common with reference compound",
              input = shiny::sliderInput(
                inputId = ns("n_common"),
                label = NULL,
                min = 0,
                max = 8,
                step = 1,
                value = 0
              )
            ),
            formGroup(
              label = "Number of phenotypic assays in common with reference compound",
              input = shiny::sliderInput(
                inputId = ns("n_pheno"),
                label = NULL,
                min = 0,
                max = 8,
                step = 1,
                value = 0
              )
            ),
            div(
              tags$label(
                `for` = ns("table_ref_compound"),
                "Reference compound"
              ),
              dataTableOutput(
                # binding_data
                outputId = ns("table_ref_compound")
              )
            )
          ),
          navPane(
            id = ns("pane_instructions"),
            p(
              "Similarity is regarded in threefold: structural similarity (Tanimoto similarity of Morgan2 fingerprints— calculated using RDKit), target affinity spectrum similarity (TAS) and phenotypic fingerprint similarity (PFP). To use SimilaritySelectR, select a reference compound and adjust filters as desired. From the main plots, select a region with compounds of interest. Then, select up to three compounds in the bottom table and view their known binding affinities in detail."
            ),
            p(
              "The Similarity application is designed to let you explore compounds that are similar to your compound of interest. Similarity is regarded in threefold:"
            ),
            tags$ol(
              tags$li(
                "Structural similarity - similarity of the chemical structure of a compound pair, calculated with tanimoto similarity based on Morgan2 fingerprints (calculated using RDKit) - from 0 (least similar) to 1 (most similar)"
              ),
              tags$li(
                "Target affinity spectrum (TAS) similarity - the similarity of all the targets of a compound pair, calculated with jaccard similarity - from 0 (least similar) to 1 (most similar)"
              ),
              tags$li(
                "Phenotypic fingerprint (PFP) correlation - the correlation of the results in phenotypic assays for a compound pair - from -1 (most anti-correlated) to 1 (most correlated)"
              )
            ),
            tags$p(
              "To view compounds that are similar, first select a reference compound of your choice. This will display plots of the structural similarity, target affinity spectrum similarity (TAS), and phenotypic fingerprint similarity (PFP) between your reference compound and compounds from the HMS Laboratory of Systems Pharmacology (LSP) library."
            ),
            tags$p(
              "Next, select an area of interest on one of the similarity plots. This will highlight the compounds in the other plots and show only the selected compounds in the output table (double click to de-select). You may also set thresholds for the number of biological and phenotypic assays that the LSP compounds share with your reference compound."
            ),
            tags$p(
              "Finally, select up to five similar compounds in the output table that you like to explore further. This will show the known gene targets and affinities for the selected compounds, for your information. Click the “Download tables” button to download the these tables as well as the table of binding data for your reference compound."
            )            
          )
        )
      )
    ),
    column(
      width = 8,
      card(
        h3("Compound similarity plots"),
        p("Select an area of similarity you are interested in. Hover over points for more information. Double-click on plot to un-select region."),
        div(
          columns(
            column(
              width = 4,
              plotly::plotlyOutput(
                outputId = ns("plot_pheno_struct") # mainplot1
              )
            ),
            column(
              width = 4,
              plotly::plotlyOutput(
                outputId = ns("plot_target_struct") # mainplot2
              )
            ),
            column(
              width = 4,
              plotly::plotlyOutput(
                outputId = ns("plot_pheno_target") # mainplot3
              )
            )
          )
        )
      ) %>% 
        margin(bottom = 3),
      card(
        h3("Compound similarity data"),
        p("Select up to three similar compounds for which target affinity information will be displayed"),
        div(
          dataTableOutput(
            # output_table
            outputId = ns("table_sim_compound"),
            height = "575px"
          )
        )
      ) %>% 
        margin(bottom = 3),
      card(
        h3("Compound similarity selections"),
        div(
          columns(
            column(
              width = 3,
              listGroupInput(
                class = "active--green",
                id = ns("compound_selection")
              )
            ),
            column(
              width = 9,
              navContent(
                lapply(1:5, function(i) {
                  navPane(
                    id = ns(paste0("pane_selection_", i)),
                    dataTableOutput(
                      outputId = ns(paste0("table_selection_", i)),
                      height = "500px"
                    )
                  )
                })
              )
            )
          )
        )
      )
    )
  )  
}

similarityServer <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$nav, {
    switch(
      input$nav,
      filters = showNavPane(ns("pane_filters")),
      instructions = showNavPane(ns("pane_instructions"))
    )
  })

  observe({
    x_compounds <- data_similarity %>% 
      dplyr::distinct(name_1) %>% 
      dplyr::arrange(name_1) %>% 
      dplyr::pull()
    
    updateSelectInput(
      id = "query_compound",
      choices = x_compounds,
      selected = x_compounds[1],
      session = session
    )
  })
  
  c_binding_msg <- reactive({
    if (NROW(r_ref_data()) > 0) {
      paste0(values$ref_hms_id, "; ", input$query_compound)
    } else {
      paste("No gene target binding data available for", input$query_compound)
    }    
  })
  
  output$binding_drug <- renderText({
    c_binding_msg()
  })
  
  output$reference_drug <- renderText({
    req(input$query_compound)

    paste(
      "Compound similarities for", input$query_compound, 
      "from HMS LINCS small molecule library"
    )
  })
  
  r_sim_data <- reactive({
    req(
      input$query_compound,
      input$n_common,
      input$n_pheno
    )
    
    data_similarity %>%
      filter(name_1 == input$query_compound) %>%
      filter(
        is.na(n_biol_assays_common_active) | 
          n_biol_assays_common_active >= 2 ^ input$n_common
      ) %>%
      filter(
        is.na(n_pheno_assays_active_common) | 
          n_pheno_assays_active_common >= 2 ^ input$n_pheno
      ) %>%
      mutate(
        PFP = ifelse(is.na(PFP), -1.1, PFP),
        TAS = ifelse(is.na(TAS), -0.1, TAS),
        structural_similarity = ifelse(is.na(structural_similarity), -0.1, structural_similarity)
      )
  })
  
  r_ref_data <- reactive({
    req(input$query_compound)
    
    data_affinity_selectivity %>% 
      dplyr::filter(name == input$query_compound) %>%
      dplyr::select(symbol, selectivity_class, `mean_Kd_(nM)`) %>% 
      dplyr::mutate(
        selectivity_class = factor(selectivity_class, SELECTIVITY_ORDER)
      ) %>%
      dplyr::arrange(selectivity_class, `mean_Kd_(nM)`)
      
  })
  
  r_selection_drugs <- reactive({
    if (is.null(input$table_sim_compound_rows_selected)) {
      return(NULL)
    }
    
    r_sim_data()$name_2[input$table_sim_compound_rows_selected]
  })
  
  r_selection_titles <- reactive({
    hms_id <- r_sim_data() %>% 
      dplyr::distinct(hmsID_1) %>% 
      dplyr::pull()
    
    name <- r_sim_data() %>% 
      dplyr::distinct(name_1) %>% 
      dplyr::pull()
    
    paste0(hms_id, "; ", name)
  })
  
  r_selection_data <- reactive({
    r_sim_data() %>% 
      dplyr::select(name_1, name_2, structural_similarity, PFP, TAS)
      # SharedData$new("name_2")
  })
  
  output$table_ref_compound <- DT::renderDataTable(
    r_ref_data(),
    class = "font-size-sm",
    options = list(
      # autoWidth = TRUE,
      dom = "t",
      ordering = FALSE,
      searchHighlight = TRUE
    ),
    rownames = FALSE,
    selection = "none"
  )
  
  # output_table
  output$table_sim_compound = DT::renderDataTable(
    r_sim_data(),
    extensions = 'Buttons',
    # fillContainer = TRUE,
    rownames = FALSE, 
    server = TRUE,
    # style = "bootstrap",
    options = list(
      autoWidth = TRUE,
      buttons = c('copy', 'csv', 'excel', 'colvis'),
      dom = 'lfrtipB',
      pagingType = "numbers",
      scrollCollapse = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE,
      stateSave = TRUE
    )
  )
  
  # mainplot1
  output$plot_pheno_struct <- renderPlotly({
    r_selection_data() %>%
      plot_ly(
        x = ~ structural_similarity, 
        y = ~ PFP, 
        type = "scatter",
        mode = "markers",
        color = I("black"),
        # name = ~ name_2,
        text = ~ paste(
          "Drug 1: ", name_1, "\n",
          "Drug 2: ", name_2, "\n",
          "x: ", structural_similarity, "\n",
          "y: ", PFP, 
          sep = ""
        )
        # hoverinfo = "text"
      ) %>%
      layout(
        showlegend = FALSE,
        shapes = list(
          list(type='line', x0= -0.1, x1= -0.1, y0=-1.2, y1=1.2,
               line=list(dash='dot', width=2, color = "red")),
          list(type='line', x0= -0.15, x1= 1.15, y0=-1.1, y1=-1.1,
               line=list(dash='dot', width=2, color = "red"))
        ),
        xaxis = list(
          range = c(-0.15, 1.15),
          title = "Structural similarity",
          tickmode = "array",
          tickvals = c(-0.1, seq(0,1,.25)),
          ticktext = c("NA", as.character(seq(0,1,.25))) 
        ),
        yaxis = list(
          range = c(-1.2, 1.2),
          title = "Phenotypic Correlation",
          tickmode = "array",
          tickvals = c(-1.1, seq(-1,1,.5)),
          ticktext = c("NA", as.character(seq(-1,1,.5))) 
        )
      )
      # highlight(
      #   on = "plotly_selected", 
      #   off = "plotly_deselect",
      #   color = I("red"), 
      #   selected = attrs_selected(name = ~ name_2)
      # )
    
    # if restoring from a bookmark, select previously selected points
    # p$x$highlight$defaultValues = values$c.data$name_2[points1]
    # p$x$highlight$color = "rgba(255,0,0,1)"
    # p$x$highlight$off = "plotly_deselect"
    
    # p %>% layout(dragmode = "select")
  })
  
  # mainplot2
  output$plot_target_struct <- renderPlotly({
    r_selection_data() %>%
      plot_ly(
        x = ~ structural_similarity, 
        y = ~ TAS, 
        type = "scatter",
        mode = "markers", 
        color = I("black"), 
        # name = ~ name_2,
        text = ~ paste(
          "Drug 1: ", name_1, "\n",
          "Drug 2: ", name_2, "\n",
          "x: ", structural_similarity, "\n",
          "y: ", TAS,
          sep = ""
        )
        # hoverinfo = "tooltip_2"
      ) %>%
      layout(
        showlegend = FALSE,
        shapes = list(
          list(type='line', x0= -0.1, x1= -0.1, y0= -0.15, y1= 1.15,
               line=list(dash='dot', width=2, color = "red")),
          list(type='line', x0= -0.15, x1= 1.15, y0= -0.1, y1= -0.1,
               line=list(dash='dot', width=2, color = "red"))
        ),
        xaxis = list(
          range = c(-0.15, 1.15),
          title = "Structural similarity",
          tickmode = "array",
          tickvals = c(-0.15, seq(0,1,.25)),
          ticktext = c("NA", as.character(seq(0,1,.25))) 
        ),
        yaxis = list(
          range = c(-0.15, 1.15),
          title = "Target Similarity",
          tickmode = "array",
          tickvals = c(-0.15, seq(0,1,.2)),
          ticktext = c("NA", as.character(seq(0,1,.2)))
        )
      )
      # highlight(
      #   on = "plotly_selected", 
      #   off = "plotly_deselect",
      #   color = I('red'), 
      #   selected = attrs_selected(name = ~ name_2)
      # )
    
    # if restoring from a bookmark, select previously selected points
    # p$x$highlight$defaultValues = values$c.data$name_2[points2]
    # p$x$highlight$color = "rgba(255,0,0,1)"
    # p$x$highlight$off = "plotly_deselect"
    
    # p %>% layout(dragmode = "select")
  })
  
  # mainplot3
  output$plot_pheno_target <- renderPlotly({
    r_selection_data() %>%
      plot_ly(
        x = ~ TAS, 
        y = ~ PFP, 
        type = "scatter",
        mode = "markers", 
        color = I("black"), 
        # name = ~ name_2,
        text = ~ paste(
          "Drug 1: ", name_1, "\n",
          "Drug 2: ", name_2, "\n",
          "x: ", TAS, "\n",
          "y: ", PFP,
          sep = ""
        )
      ) %>%
      layout(
        showlegend = FALSE,
        shapes = list(
          list(type='line', x0= -0.1, x1= -0.1, y0=-1.2, y1=1.2,
               line=list(dash='dot', width=2, color = "red")),
          list(type='line', x0= -0.15, x1= 1.15, y0=-1.1, y1=-1.1,
               line=list(dash='dot', width=2, color = "red"))
        ),
        xaxis = list(
          range = c(-0.15, 1.15),
          title = "Target Similarity",
          tickmode = "array",
          tickvals = c(-0.15, seq(0,1,.25)),
          ticktext = c("NA", as.character(seq(0,1,.25))) 
        ),
        yaxis = list(
          range = c(-1.2, 1.2),
          title = "Phenotypic Correlation",
          tickmode = "array",
          tickvals = c(-1.2, seq(-1,1,.5)),
          ticktext = c("NA", as.character(seq(-1,1,.5))))
      ) 
      # highlight(
      #   on = "plotly_selected",
      #   off = "plotly_deselect",
      #   color = I("red"), 
      #   selected = attrs_selected(name = ~ name_2)
      # )
    
    # if restoring from a bookmark, select previously selected points
    # p$x$highlight$defaultValues = values$c.data$name_2[points3]
    # p$x$highlight$color = "rgba(255,0,0,1)"
    # p$x$highlight$off = "plotly_deselect"
    
    # p %>% layout(dragmode = "select")
  })
  
  observeEvent(r_selection_drugs(), ignoreNULL = FALSE, {
    if (is.null(r_selection_drugs())) {
      updateListGroupInput(
        id = "compound_selection",
        choices = NULL,
        values = NULL,
        session = session
      )  
      return()
    }
    
    if (isTRUE(input$compound_selection %in% r_selection_drugs())) {
      x_selected <- which(input$compound_selection == r_selection_drugs())
    } else {
      x_selected <- 1
    }
    
    x_choices <- lapply(r_selection_drugs(), function(d) {
      paste0(get_hms_id_2(d), "; ", d)
    })
    
    updateListGroupInput(
      id = "compound_selection",
      choices = x_choices,
      values = seq_along(r_selection_drugs()),
      selected = x_selected,
      session = session
    )
  })
  
  observeEvent(input$compound_selection, {
    showNavPane(ns(paste0("pane_selection_", input$compound_selection)))
  })
  
  get_hms_id_2 <- function(drug) {
    if (is.null(drug) || is.na(drug) || length(drug) < 1) {
      return(NULL)
    }
    
    data_similarity %>%
      dplyr::filter(name_2 == drug) %>%
      dplyr::slice(1) %>% 
      dplyr::pull(hmsID_2)
  }
  
  get_compound_selection <- function(drug) {
    if (is.null(drug) || is.na(drug) || length(drug) < 1) {
      return(NULL)
    }
    
    data_affinity_selectivity %>%
      dplyr::filter(name == drug) %>%
      dplyr::mutate(
        selectivity_class = factor(selectivity_class, SELECTIVITY_ORDER)
      ) %>%
      dplyr::select(symbol, selectivity_class, `mean_Kd_(nM)`) %>% 
      dplyr::arrange(selectivity_class, `mean_Kd_(nM)`)
  }
  
  selection_table_options <- list(
    # autoWidth = TRUE,
    dom = "tp",
    scrollX = TRUE,
    pagingType = "numbers"
  )
  
  output$table_selection_1 <- DT::renderDataTable(
    get_compound_selection(r_selection_drugs()[1]),
    options = selection_table_options,
    rownames = FALSE
  )
  outputOptions(output, "table_selection_1", suspendWhenHidden = FALSE)
  
  output$table_selection_2 <- DT::renderDataTable(
    get_compound_selection(r_selection_drugs()[2]),
    options = selection_table_options,
    rownames = FALSE
  )
  outputOptions(output, "table_selection_2", suspendWhenHidden = FALSE)
  
  output$table_selection_3 <- DT::renderDataTable(
    get_compound_selection(r_selection_drugs()[3]),
    options = selection_table_options,
    rownames = FALSE
  )
  outputOptions(output, "table_selection_3", suspendWhenHidden = FALSE)
  
  output$table_selection_4 <- DT::renderDataTable(
    get_compound_selection(r_selection_drugs()[4]),
    options = selection_table_options,
    rownames = FALSE
  )
  outputOptions(output, "table_selection_4", suspendWhenHidden = FALSE)
  
  output$table_selection_5 <- DT::renderDataTable(
    get_compound_selection(r_selection_drugs()[5]),
    options = selection_table_options,
    rownames = FALSE
  )
  outputOptions(output, "table_selection_4", suspendWhenHidden = FALSE)
}
  
function() {
  
  
  observe({
    print("render selection tables")
    output$selection1 = DT::renderDataTable(
      values$selection.display_table1,
      rownames = F, options = list(
        dom = 'tp',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        autoWidth = TRUE)
    )
    
    output$selection2 = DT::renderDataTable(
      values$selection.display_table2,
      rownames = F, options = list(
        dom = 'tp',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        autoWidth = TRUE)
    )
    
    output$selection3 = DT::renderDataTable(
      values$selection.display_table3,
      rownames = F, options = list(
        dom = 'tp',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        autoWidth = TRUE)
    )
    
    output$selection4 = DT::renderDataTable(
      values$selection.display_table4,
      rownames = F, options = list(
        dom = 'tp',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        autoWidth = TRUE)
    )
    
    output$selection5 = DT::renderDataTable(
      values$selection.display_table5,
      rownames = F, options = list(
        dom = 'tp',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        autoWidth = TRUE)
    )
    
    output$sel1_drug = renderText({ values$selection.title1 })
    output$sel2_drug = renderText({ values$selection.title2 })
    output$sel3_drug = renderText({ values$selection.title3 })
    output$sel4_drug = renderText({ values$selection.title4 })
    output$sel5_drug = renderText({ values$selection.title5 })
    
  })
  
  output$downloadBind <- downloadHandler(
    filename = function() {
      return(paste0("BindingData_", format(Sys.time(), "%Y%m%d_%I%M%S"), 
                    ".zip", sep = ""))
    },
    content = function(filename) {
      files_all = list(values$c.binding_data,
                       values$selection.binding_data1,
                       values$selection.binding_data2,
                       values$selection.binding_data3,
                       values$selection.binding_data4,
                       values$selection.binding_data5)
      # take only tables that exist
      print(values$num_selected)
      files = files_all[1:(values$num_selected + 1)]
      drugs = input$query_compound
      if(values$num_selected > 0) {
        for(i in 1:5) {
          drugs = c(drugs, values[[paste0("selection.drug", i)]])
        }
      }
      zipped_csv(files, filename, paste0("BindingData_", drugs), format(Sys.time(), "%Y%m%d_%I%M%S") )
    }, contentType = "application/zip"
  )
  session$allowReconnect(TRUE)
}
  
  
simServer2 <- function() {
  
  similarity_table = read_csv("input/similarity_table_ChemblV22_1_20170804.csv") %>%
    mutate_at(c("PFP","TAS","structural_similarity"),
              function(x) round(x, 2))
  affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv") %>%
    mutate_at(vars(c(`mean_Kd_(nM)`, `SD_Kd_(nM)`:offtarget_IC50_N)),
              function(x) signif(x, 2))
  selectivity_order = c("Most selective","Semi-selective","Poly-selective","Unknown","Other")
  
  # Function for toolip values
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    paste0(names(x), ": ", format(x), collapse = "<br />")
  }
  
  contact.modal.js = "$('.ui.mini.modal')
$('#contact_modal').modal('show')
;"
  # open about modal
  about.modal.js = "$('.ui.small.modal')
.modal({
blurring: false
})
$('#about_modal').modal('show')
;"
  bookmark.modal.js = "$('.ui.mini.modal')
.modal({
blurring: false
})
$('#bookmark_modal').modal('show')
;"
  
    # Make app stop when you close the webpage
    #session$onSessionEnded(stopApp)
    observeEvent(input$contact, {
      runjs(contact.modal.js)
    })
    # Set locale so that sorting works correctly
    Sys.setlocale("LC_COLLATE","en_US.UTF-8")
    
    # Load "about" modal
    observeEvent(input$about, {
      runjs(about.modal.js)
    })
    
    # Run js to hide warning messages on click
    runjs(message.hide.js)
    
    new_input = NULL
    
    onRestore(function(state) {
      print("onRestore start")
      query_id = getQueryString()$bookmark
      input_name = paste0("sms_bookmarks/", query_id, "/input.rds")
      if( head_object(object = input_name, bucket = aws_bucket, check_region = F) ) {
        new_input <<- s3readRDS(object = input_name, bucket = aws_bucket, check_region = F)
      } else {
        showElement(id = "bookmark_not_found")
      }
      print("onRestore end")
    })
    
    onRestored(function(state) {
      print("onRestored start")
      ### restore the state if the bookmark is found
      if(!is.null(new_input)) {
        ## select the compound
        updateSelectizeInput(session, "query_compound", selected = new_input$query_compound)
        ## update sliders
        updateSliderInput(session, inputId = "n_common", value = new_input$n_common)
        updateSliderInput(session, inputId = "n_pheno", value = new_input$n_pheno)
        ## show/hide elements
        if(floor(new_input$filter_button/2) != new_input$filter_button/2) { shinyjs::click("filter_button") }
        if(floor(new_input$intro_hide/2) != new_input$intro_hide/2) { shinyjs::click("intro_hide") }
        ## restore other values
        values$points_selected1 = new_input$points_selected1
        values$points_selected2 = new_input$points_selected2
        values$points_selected3 = new_input$points_selected3
        values$rows_selected_save = new_input$output_table_rows_selected
        ## reset saved input placeholder object
        new_input <<- NULL
        #values$bookmark_restart = T
      }
      #updateQueryString("?")
      print("onRestored end")
    })
    
    onBookmark(function(state) {
      print("bookmark")
      if(exists("d")) {
        if(length(d$selection(ownerId = "mainplot1")) > 0) {
          values$points_selected1 = d$selection(ownerId = "mainplot1") %>% which()
          values$points_selected2 = d$selection(ownerId = "mainplot2") %>% which()
          values$points_selected3 = d$selection(ownerId = "mainplot3") %>% which()
          #values$groupId = d$groupName()
        }
      }
    })
    
    onBookmarked(function(url) {
      print("bookmarked")
      date_time = format(Sys.time(), "%Y%m%d-%H%M%S")
      id = substr(as.character(runif(1)), 3, 6)
      new_id = paste0(app_name, "-", date_time, "-", id)
      new_url = gsub("\\?_inputs_.*", paste0("?bookmark=",new_id), url)
      session$sendCustomMessage("bookmark_url", message = new_url)
      values$url = new_url
      input_list = reactiveValuesToList(input, all.names = T)
      print("input_list")
      print(names(input_list))
      #row_sel = grep("_rows_selected$", names(input_list), value = T)
      input_list_save = input_list[c("query_compound", "filter_button", "n_pheno", "intro_hide", "n_common",
                                     "output_table_rows_selected")]
      #values_list = reactiveValuesToList(values, all.names = T)
      #for(x in row_sel) { values_list[[x]] = input_list[[x]] }
      #points_selected_names = grep("^points_selected[1-3]", names(values_list), value = T)
      #selected_drug_names = grep("^selection.drug[1-5]", names(values_list), value = T)
      input_list_save$points_selected1 = values$points_selected1
      input_list_save$points_selected2 = values$points_selected2
      input_list_save$points_selected3 = values$points_selected3
      s3saveRDS(input_list_save, bucket = aws_bucket, object = paste0("sms_bookmarks/", new_id, "/", "input.rds"), check_region = F)
      updateQueryString(new_url)
    })
    
    ##### For updating URL query string
    # observe({
    #   # Needed to call input to trigger bookmark
    #   all_vars = reactiveValuesToList(input, all.names = T)
    #   # Don't delete above line -- needed for point selection bookmarking
    #   session$doBookmark()
    # })
    
    observeEvent(input$bookmark1, {
      runjs(bookmark.modal.js)
    })
    
    observeEvent(input$bookmark1, {
      session$doBookmark()
    })
    
    # Add clipboard buttons
    output$clip <- renderUI({
      rclipButton("clipbtn", "Copy", values$url, icon("clipboard"))
    })
    
    # Workaround for execution within RStudio
    #observeEvent(input$clipbtn, clipr::write_clip(values$url))
    
    
    observeEvent(values$c.binding_display, {
      print(values$c.binding_display)
      
      # Binding table output
      output$binding_data = DT::renderDataTable(
        if(dim(values$c.binding_display)[1] > 0) {
          as.data.frame(values$c.binding_display)
        } else {
          NULL
        }, rownames = F, options = list(
          dom = 'tp',
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          searchHighlight = TRUE,
          autoWidth = TRUE)
      )
      
      output$binding_drug = renderText(
        if(dim(values$c.binding_display)[1] > 0) {
          paste0(values$ref_hms_id, "; ", input$query_compound)
        } else {
          paste0("No gene target binding data available for ",input$query_compound)
        })
    })
    
    observeEvent(input$query_compound, {
      if(length(input$query_compound) > 0) {
        output$ref_drug = renderText(
          paste0("Compound similarities for ", input$query_compound, 
                 " from HMS LINCS small molecule library")
        )
      }
    })
    
    # reactive values
    values = reactiveValues(c.data = NULL, c.data_display = NULL, c.data_title = NULL, 
                            c.binding_data = NULL, c.binding_display = NULL, 
                            drug_select = NULL, num_selected = 0,
                            c.data_display_sub = NULL, c.data_sub = NULL)
    
    # show/hide intro
    observeEvent(input$intro_hide, {
      toggleElement(id = "intro", anim = T, animType = "fade")
      toggleElement(id = "caret_down")
      toggleElement(id = "caret_right")
    })
    
    # show/hide filters
    observeEvent(input$filter_button, {
      toggleElement(id = "filters", anim = T, animType = "fade")
      toggleElement(id = "caret_down_fil")
      toggleElement(id = "caret_right_fil")
    })
    
    # update the table upon parameter/input changes
    observeEvent(c(input$query_compound, input$n_common, input$n_pheno), {
      if(!is.null(input$query_compound) & input$query_compound != "") {
        print("main loop")
        #hideElement(id = "intro", anim = T, animType = "fade", time = 1)
        showElement("filters_head")
        showElement("button_row")
        #showElement("filters")
        showElement("result_row1")
        showElement("result_row2")
        showElement("result_row4")
        showElement("loader1")
        showElement("loader2")
        showElement("loader3")
        showElement("loader_tab")
        
        ## subset current data
        values$c.data = similarity_table %>%
          filter(name_1 == input$query_compound) %>%
          filter(n_biol_assays_common_active >= 2^input$n_common | is.na(n_biol_assays_common_active)) %>%
          filter(n_pheno_assays_active_common >= 2^input$n_pheno | is.na(n_pheno_assays_active_common)) %>%
          mutate_at(vars(PFP), funs(ifelse(is.na(PFP), -1.1, PFP))) %>%
          mutate_at(vars(TAS), funs(ifelse(is.na(TAS), -0.1, TAS))) %>%
          mutate_at(vars(structural_similarity), funs(ifelse(is.na(structural_similarity), -0.1, structural_similarity)))
        
        values$c.data_display = values$c.data
        
        values$c.data_title = paste0(unique(values$c.data$hmsID_1),";",unique(values$c.data$name_1))
        
        ## show affinity data of reference compound+ selected compounds
        # filter by name or hms id?
        values$c.binding_data = affinity_selectivity %>% filter(name == input$query_compound) %>%
          #filter(`mean_Kd_(nM)` >= 10^input$affinity[1]) %>%
          #filter(`mean_Kd_(nM)` <= 10^input$affinity[2]) %>%
          #filter(`SD_Kd_(nM)` <= 10^input$sd) %>%
          #filter(n_measurements >= input$min_measurements) %>%
          mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
          arrange(selectivity_class, `mean_Kd_(nM)`)
        
        d <<- SharedData$new(values$c.data, ~name_2)
        
        values$ref_hms_id = unique(values$c.binding_data$hms_id)
        
        values$c.binding_display = values$c.binding_data[,c(3,4,5)]
        
        points1 = values$points_selected1
        points2 = values$points_selected2
        points3 = values$points_selected3
        
        output$mainplot1 <- renderPlotly({
          p <- d %>%
            plot_ly(x = ~structural_similarity, y = ~PFP, mode = "markers", 
                    color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
                                                                      name_1, "\nDrug 2: ", name_2, "\nx: ", structural_similarity, "\ny: ", 
                                                                      PFP, sep = ""), hoverinfo = "text") %>%
            layout(showlegend = F,
                   shapes = list(list(type='line', x0= -0.1, x1= -0.1, y0=-1.2, y1=1.2,
                                      line=list(dash='dot', width=2, color = "red")),
                                 list(type='line', x0= -0.15, x1= 1.15, y0=-1.1, y1=-1.1,
                                      line=list(dash='dot', width=2, color = "red"))),
                   xaxis = list(range = c(-0.15, 1.15),
                                title = "Structural similarity",
                                tickmode = "array",
                                tickvals = c(-0.1, seq(0,1,.25)),
                                ticktext = c("NA", as.character(seq(0,1,.25))) ),
                   yaxis = list(range = c(-1.2, 1.2),
                                title = "Phenotypic Correlation",
                                tickmode = "array",
                                tickvals = c(-1.1, seq(-1,1,.5)),
                                ticktext = c("NA", as.character(seq(-1,1,.5))) )
            ) %>% 
            highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2), hoverinfo = "text")
          # if restoring from a bookmark, select previously selected points
          p$x$highlight$defaultValues = values$c.data$name_2[points1]
          p$x$highlight$color = "rgba(255,0,0,1)"
          p$x$highlight$off = "plotly_deselect"
          p %>% layout(dragmode = "select")
        })
        if(sum(values$points_selected1) > 0) {
          d$selection(points1, ownerId = "mainplot1")
          values$points_selected1 = F
        }
        
        output$mainplot2 <- renderPlotly({
          p <- d %>%
            plot_ly(x = ~structural_similarity, y = ~TAS, mode = "markers", 
                    color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
                                                                      name_1, "\nDrug 2: ", name_2, "\nx: ", structural_similarity, 
                                                                      "\ny: ", TAS, sep = ""), hoverinfo = "text") %>%
            layout(showlegend = F,
                   shapes = list(list(type='line', x0= -0.1, x1= -0.1, y0= -0.15, y1= 1.15,
                                      line=list(dash='dot', width=2, color = "red")),
                                 list(type='line', x0= -0.15, x1= 1.15, y0= -0.1, y1= -0.1,
                                      line=list(dash='dot', width=2, color = "red"))),
                   xaxis = list(range = c(-0.15, 1.15),
                                title = "Structural similarity",
                                tickmode = "array",
                                tickvals = c(-0.15, seq(0,1,.25)),
                                ticktext = c("NA", as.character(seq(0,1,.25))) ),
                   yaxis = list(range = c(-0.15, 1.15),
                                title = "Target Similarity",
                                tickmode = "array",
                                tickvals = c(-0.15, seq(0,1,.2)),
                                ticktext = c("NA", as.character(seq(0,1,.2))) )) %>% 
            highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2))
          # if restoring from a bookmark, select previously selected points
          p$x$highlight$defaultValues = values$c.data$name_2[points2]
          p$x$highlight$color = "rgba(255,0,0,1)"
          p$x$highlight$off = "plotly_deselect"
          p %>% layout(dragmode = "select")
        })
        if(sum(values$points_selected2) > 0) {
          d$selection(points2, ownerId = "mainplot2")
          values$points_selected2 = F
        }
        
        output$mainplot3 <- renderPlotly({
          p <- d %>%
            plot_ly(x = ~TAS, y = ~PFP, mode = "markers", 
                    color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
                                                                      name_1, "\nDrug 2: ", name_2, "\nx: ", TAS, "\ny: ", PFP, sep = ""),
                    hoverinfo = "text") %>%
            layout(showlegend = F,
                   shapes = list(list(type='line', x0= -0.1, x1= -0.1, y0=-1.2, y1=1.2,
                                      line=list(dash='dot', width=2, color = "red")),
                                 list(type='line', x0= -0.15, x1= 1.15, y0=-1.1, y1=-1.1,
                                      line=list(dash='dot', width=2, color = "red"))),
                   xaxis = list(range = c(-0.15, 1.15),
                                title = "Target Similarity",
                                tickmode = "array",
                                tickvals = c(-0.15, seq(0,1,.25)),
                                ticktext = c("NA", as.character(seq(0,1,.25))) ),
                   yaxis = list(range = c(-1.2, 1.2),
                                title = "Phenotypic Correlation",
                                tickmode = "array",
                                tickvals = c(-1.2, seq(-1,1,.5)),
                                ticktext = c("NA", as.character(seq(-1,1,.5))))) %>% 
            highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2))
          # if restoring from a bookmark, select previously selected points
          p$x$highlight$defaultValues = values$c.data$name_2[points3]
          p$x$highlight$color = "rgba(255,0,0,1)"
          p$x$highlight$off = "plotly_deselect"
          p %>% layout(dragmode = "select")
        })
        
        if(sum(values$points_selected3) > 0) {
          d$selection(points3, ownerId = "mainplot3")
          values$points_selected3 = F
        }
        
        output$output_table = DT::renderDataTable( {
          values$c.data_display_sub <- values$c.data_display[d$selection(), , drop = F]
          values$c.data_sub = values$c.data[d$selection(), , drop = F]
          m2 = values$c.data_display_sub
          dt <- values$c.data_display
          if(NROW(m2) == 0) {
            dt
          } else {
            m2
          }
        },
        extensions = 'Buttons',
        rownames = F, options = list(
          dom = 'lBfrtip',
          stateSave = TRUE,
          buttons = c('copy', 'csv', 'excel', 'colvis'),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          searchHighlight = TRUE,
          autoWidth = TRUE), server = T
        )
      }
    }, ignoreInit = T, ignoreNULL = T)
    
    ## On bookmark restart: load row selections in output table and binding table
    # observeEvent(values$bookmark_restart, {
    #   print("start select rows")
    #   if(values$bookmark_restart) {
    #     if(length(values$output_table_rows_selected) > 0) {
    #       print("select rows: output table")
    #       rows = values$output_table_rows_selected
    #       proxy %>% selectRows(rows)
    #     }
    #     if(length(values$binding_data_rows_selected) > 0) {
    #       print("select rows: output table")
    #       rows = values$binding_data_rows_selected
    #       proxy_bind %>% selectRows(rows)
    #     }
    #     values$bookmark_restart2 = T
    #   }
    #   
    # }, ignoreNULL = T, ignoreInit = T, autoDestroy = T)
    
    ## On bookmark restart: load row selections in selection tables
    # observeEvent(values$bookmark_restart2, {
    #   if(values$bookmark_restart2) {
    #     for(i in 1:5) {
    #       var_name = paste0("selection", i, "_rows_selected")
    #       if(length(values[[var_name]]) > 0) {
    #         print(paste0("select rows: ", "selection", i))
    #         rows = values[[var_name]]
    #         get(paste0("proxy", i)) %>% selectRows(rows)
    #       }
    #     }
    #   }
    # }, ignoreInit = T, ignoreNULL = T, autoDestroy = T)
    
    # Make other tables on row selection
    observeEvent(input$output_table_rows_selected, {
      print("rows selected")
      showElement("result_row3")
      showElement("row3_bind_data")
      row = input$output_table_rows_selected
      
      print("start select rows")
      # If restoring bookmarked session, select same rows as before
      if(length(values$rows_selected_save) > 0) {
        print("restore selections")
        row = values$rows_selected_save
        proxy %>%  selectRows(row)
        values$rows_selected_save = NULL
      }
      
      # show/hide the selection tables
      if(length(row) == 1) {
        showElement("row3_col1")
        hideElement("row3_col2")
        hideElement("row3_col3")
        hideElement("row3_col4")
        hideElement("row3_col5")
        #showElement("button_row")
      } else if(length(row) == 2) {
        showElement("row3_col1")
        showElement("row3_col2")
        hideElement("row3_col3")
        hideElement("row3_col4")
        hideElement("row3_col5")
        #showElement("button_row")
      } else if(length(row) == 3) {
        showElement("row3_col1")
        showElement("row3_col2")
        showElement("row3_col3")
        hideElement("row3_col4")
        hideElement("row3_col5")
        #showElement("button_row")
      } else if(length(row) == 4) {
        showElement("row3_col1")
        showElement("row3_col2")
        showElement("row3_col3")
        showElement("row3_col4")
        hideElement("row3_col5")
        #showElement("button_row")
      } else if(length(row) == 5) {
        showElement("row3_col1")
        showElement("row3_col2")
        showElement("row3_col3")
        showElement("row3_col4")
        showElement("row3_col5")
        #showElement("button_row")
      }
      for(i in 1:length(row)) {
        if(length(row) == 0) {
          hideElement("row3_col1")
          hideElement("row3_col2")
          hideElement("row3_col3")
          hideElement("row3_col4")
          hideElement("row3_col5")
          #hideElement("button_row")
          break
        }
        if(length(row) > 5) { break }
        name_data = paste("selection.binding_data", i, sep = "")
        name_display = paste("selection.display_table", i, sep = "")
        name_title = paste("selection.title", i, sep = "")
        name_file = paste0("selection.drug", i)
        if(NROW(values$c.data_display_sub) > 0) {
          dt1 = values$c.data_sub
        } else {
          dt1 = values$c.data
        }
        drug = dt1$name_2[ row[i] ]
        hms_id = dt1$hmsID_2[ row[i] ]
        values[[name_file]] = drug
        values[[name_title]] = paste0(hms_id, "; ", drug)
        values$num_selected = length(row)
        
        values[[name_data]] = affinity_selectivity %>%
          filter(name == drug) %>%
          #filter(`mean_Kd_(nM)` >= 10^input$affinity[1]) %>%
          #filter(`mean_Kd_(nM)` <= 10^input$affinity[2]) %>%
          #filter(`SD_Kd_(nM)` <= 10^input$sd) %>%
          #filter(n_measurements >= input$min_measurements) %>%
          mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
          arrange(selectivity_class, `mean_Kd_(nM)`)
        
        values[[name_display]] = values[[name_data]][,c(3,4,5)]
        output_name = paste("selection", i, sep = "")
      }
      # if(values$bookmark_restart2) {
      #   for(i in 1:5) {
      #     var_name = paste0("selection", i, "_rows_selected")
      #     if(length(values[[var_name]]) > 0) {
      #       print(paste0("select rows: ", "selection", i))
      #       rows = values[[var_name]]
      #       get(paste0("proxy", i), .GlobalEnv) %>% selectRows(rows)
      #     }
      #   }
      #   rows = values$selection1_rows_selected
      #   proxy1 %>% selectRows(rows)
      # }
    }, ignoreInit = T, ignoreNULL = F)
    
    proxy <<- dataTableProxy('output_table')
    proxy1 <<- dataTableProxy('selection1')
    proxy2 <<- dataTableProxy('selection2')
    proxy3 <<- dataTableProxy('selection3')
    proxy4 <<- dataTableProxy('selection4')
    proxy5 <<- dataTableProxy('selection5')
    proxy_bind <<- dataTableProxy('binding_data')
    
    
    # observe({
    #   print("start select rows")
    #   print(values$output_table_rows_selected)
    #   if(length(values$output_table_rows_selected) > 0) {
    #     print("select rows: output table")
    #     proxy %>% selectRows(values$output_table_rows_selected)
    #   }
    #   for(i in 1:5) {
    #     var_name = paste0("selection", i, "_rows_selected")
    #     if(length(values[[var_name]]) > 0) {
    #       print(paste0("select rows: ", "selection", i))
    #       get(paste0("proxy", i)) %>% selectRows(values[[var_name]])
    #     }
    #   }
    #   if(length(values$binding_data_rows_selected) > 0) {
    #     print("select rows: output table")
    #     proxy_bind %>% selectRows(values$binding_data_rows_selected)
    #   }
    # })
    
    observeEvent(input$clearButton, {
      proxy %>% selectRows(NULL)
      for(i in 1:5) {
        assign(paste0("values$selection.binding_data",i), NULL)
      }
      values$num_selected = 0
    })
    
    observe({
      print("render selection tables")
      output$selection1 = DT::renderDataTable(
        values$selection.display_table1,
        rownames = F, options = list(
          dom = 'tp',
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          autoWidth = TRUE)
      )
      
      output$selection2 = DT::renderDataTable(
        values$selection.display_table2,
        rownames = F, options = list(
          dom = 'tp',
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          autoWidth = TRUE)
      )
      
      output$selection3 = DT::renderDataTable(
        values$selection.display_table3,
        rownames = F, options = list(
          dom = 'tp',
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          autoWidth = TRUE)
      )
      
      output$selection4 = DT::renderDataTable(
        values$selection.display_table4,
        rownames = F, options = list(
          dom = 'tp',
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          autoWidth = TRUE)
      )
      
      output$selection5 = DT::renderDataTable(
        values$selection.display_table5,
        rownames = F, options = list(
          dom = 'tp',
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          autoWidth = TRUE)
      )
      
      output$sel1_drug = renderText({ values$selection.title1 })
      output$sel2_drug = renderText({ values$selection.title2 })
      output$sel3_drug = renderText({ values$selection.title3 })
      output$sel4_drug = renderText({ values$selection.title4 })
      output$sel5_drug = renderText({ values$selection.title5 })
      
    })
    
    output$downloadBind <- downloadHandler(
      filename = function() {
        return(paste0("BindingData_", format(Sys.time(), "%Y%m%d_%I%M%S"), 
                      ".zip", sep = ""))
      },
      content = function(filename) {
        files_all = list(values$c.binding_data,
                         values$selection.binding_data1,
                         values$selection.binding_data2,
                         values$selection.binding_data3,
                         values$selection.binding_data4,
                         values$selection.binding_data5)
        # take only tables that exist
        print(values$num_selected)
        files = files_all[1:(values$num_selected + 1)]
        drugs = input$query_compound
        if(values$num_selected > 0) {
          for(i in 1:5) {
            drugs = c(drugs, values[[paste0("selection.drug", i)]])
          }
        }
        zipped_csv(files, filename, paste0("BindingData_", drugs), format(Sys.time(), "%Y%m%d_%I%M%S") )
      }, contentType = "application/zip"
    )
    session$allowReconnect(TRUE)

  
}

