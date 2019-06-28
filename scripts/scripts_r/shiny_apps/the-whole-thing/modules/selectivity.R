selectivityUI <- function(id) {
  ns <- NS(id)
  
  shiny::htmlTemplate(
    filename = "templates/selectivity.html",
    document_ = FALSE,
    genes = selectizeInput(
      inputId = ns("query_gene"), 
      label = h5("Select gene target"), 
      choices = NULL,
      options = list(
        placeholder = 'Search for a gene target',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    affinity = sliderInput(
      inputId = ns("affinity"),
      label = h5("Minimum/maximum affinity"),
      min = -3,
      max = 10,
      step = 1,
      value = c(-3, 6)
    ),
    stddev = sliderInput(
      inputId = ns("sd"),
      label = h5("Maximum std. dev. of affinity"),
      min = 0,
      max = 10,
      step = 1,
      value = 5
    ),
    measurements = sliderInput(
      inputId = ns("min_measurements"),
      label = h5("Minimum number of measurements"),
      min = 1,
      max = 15,
      step = 1,
      value = 2
    )
  )
}

selectivityServer <- function(input, output, session) {
  app_name = "SelectivitySelectR"
  # source(".awspass")
  
  message.hide.js = "$('.message .close')
.on('click', function() {
  $(this)
  .closest('.message')
  .transition('fade')
  ;
})
;"
  
  affinity_selectivity = read_csv("data/affinity_selectivity_table_ChemblV22_1_20170804.csv") %>% 
    mutate_at(vars(c(`mean_Kd_(nM)`, `SD_Kd_(nM)`:offtarget_IC50_N)),
              function(x) signif(x, 2))
  
  selectivity_order = c("Most selective","Semi-selective","Poly-selective","Unknown","Other")
  
  contact.modal.js = "$('.ui.mini.modal')
$('#contact_modal').modal('show')
;"
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
  
  # Run js to hide warning messages on click
  runjs(message.hide.js)
  
  ## initialize variable for restoring input from bookmark
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
      updateSelectizeInput(session, "query_gene", selected = new_input$query_gene)
      
      updateSliderInput(session, inputId = "affinity", value = new_input$affinity)
      updateSliderInput(session, inputId = "sd", value = new_input$sd)
      updateSliderInput(session, inputId = "min_measurements", value = new_input$min_measurements)
      if(floor(new_input$filter_button/2) != new_input$filter_button/2) { shinyjs::click("filter_button") }
      updateCheckboxInput(session, inputId = "include_genes", value = new_input$include_genes)
      values$points_selected = new_input$points_selected
      values$rows_selected_save = new_input$output_table_rows_selected
      ## reset saved input placeholder object
      new_input <<- NULL
      #updateQueryString("?") 
    }
    print("onRestored end")
  })
  
  onBookmark(function(state) {
    print("bookmark")
    if(exists("d")) {
      values$points_selected = d$selection(ownerId = "mainplot")
      #values$groupId = d$groupName()
    }
  })
  
  on_bookmarked <- function(url) {
    url_list <- shiny::parseQueryString(url)
    
    date_time <- format(Sys.time(), "%Y%m%d-%H%M%S")
    rando_id <- substr(as.character(runif(1)), 3, 6)
    
    # `new_id`
    state_id <- glue::glue("{ app_name }-{ date_time }-{ id }")

    url_list[[session$ns("bookmark")]]
    new_url = gsub("\\?_inputs_.*", paste0("?bookmark=",new_id), url)
    session$sendCustomMessage("bookmark_url", message = new_url)
    values$url = new_url
    input_list = reactiveValuesToList(input, all.names = T)
    print("input_list")
    print(names(input_list))
    input_list_save = input_list[c("query_gene", "include_genes", "filter_button",
                                   "affinity", "sd", "min_measurements",
                                   "output_table_rows_selected")]
    input_list_save$points_selected = values$points_selected
    s3saveRDS(input_list_save, bucket = aws_bucket, object = paste0("sms_bookmarks/", new_id, "/", "input.rds"), check_region = F)
    updateQueryString(new_url)
  }
  
  # observeEvent(input$bookmark1, {
  #   session$doBookmark()
  # })
  
  # Load "bookmark" modal
  observeEvent(input$bookmark1, {
    runjs(bookmark.modal.js)
  })
  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  
  # Add clipboard buttons
  output$clip <- renderUI({
    rclipButton("clipbtn", "Copy", values$url, icon("clipboard"))
  })
  
  # Workaround for execution within RStudio
  #observeEvent(input$clipbtn, clipr::write_clip(values$url))
  
  ##### For updating URL query string
  # observe({
  #   # Needed to call input to trigger bookmark
  #   all_vars = reactiveValuesToList(input, all.names = T)
  #   # Don't delete above line -- needed for point selection bookmarking
  #   session$doBookmark()
  # })
  
  # reactive values
  values = reactiveValues(c.binding_data = NULL, selection_table = NULL,
                          num_selected = 0, query_gene = NULL)
  
  # show/hide filters
  observeEvent(input$filter_button, {
    toggleElement(id = "filters", anim = T, animType = "fade")
    toggleElement(id = "filter_down")
    toggleElement(id = "filter_right")
  })
  
  observeEvent(input$query_gene, {
    values$query_gene = input$query_gene
  })
  
  observeEvent(values$query_gene, {
    output$plot_title = renderText(paste0("Affinity and selectivity for drugs targeting ", values$query_gene))
    output$table_title = renderText(paste0("Data for drugs targeting ", values$query_gene))
  })
  
  observeEvent(c(values$query_gene, input$affinity, input$sd, input$min_measurements) , {
    if(values$query_gene != "" && !is.null(values$query_gene) ) {
      print("main")
      showElement("loader1")
      showElement("plot_col")
      showElement("table_row")
      showElement("loader_table")
      showElement("plot_column")
      
      values$c.binding_data = affinity_selectivity %>%
        filter(symbol == values$query_gene) %>%
        filter(`mean_Kd_(nM)` >= 10^input$affinity[1] | is.na(`mean_Kd_(nM)`)) %>%
        filter(`mean_Kd_(nM)` <= 10^input$affinity[2] | is.na(`mean_Kd_(nM)`)) %>%
        filter(`SD_Kd_(nM)` <= 10^input$sd | is.na(`SD_Kd_(nM)`)) %>%
        filter(n_measurements >= input$min_measurements) %>%
        mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
        arrange(selectivity_class, `mean_Kd_(nM)`) %>%
        mutate(selectivity_plot = coalesce(selectivity, -0.5))
      
      if(!input$include_genes) {
        values$c.binding_data = values$c.binding_data %>%
          filter(tax_id == 9606)
      }
      
      values$selection_table = values$c.binding_data
      
      
      if(length(values$points_selected) > 0) {
        print("groupId")
        #print(values$c.binding_data[ values$points_selected, ])
        #d <<- SharedData$new(values$c.binding_data, ~name, group = values$groupId)
        d <<- SharedData$new(values$c.binding_data, ~name)
        
        #d$selection(values$points_selected, ownerId = "mainplot")
        print(d$selection())
      } else {
        print("noGroupId")
        d <<- SharedData$new(values$c.binding_data, ~name)
      }
      
      points = values$points_selected
      
      output$mainplot <- renderPlotly({
        p <- d %>%
          plot_ly(x = ~selectivity_plot, y = ~`mean_Kd_(nM)`, mode = "markers", 
                  source = "Z",
                  color = ~selectivity_class, text = ~paste("Drug name: ", 
                                                            name, "\nDrug HMS ID: ", hms_id, "\nGene symbol: ", symbol,"\nx: ", selectivity, "\ny: ", 
                                                            `mean_Kd_(nM)`, sep = ""), hoverinfo = "text") %>%
          layout(showlegend = T,
                 shapes = list(list(type='line', x0= -0.5, x1= -0.5, y0= 10^(input$affinity[1]), y1= 10^(input$affinity[2]),
                                    line=list(dash='dot', width=2, color = "red"))),
                 xaxis = list(range = c(-0.6, 1.3),
                              title = "Selectivity",
                              tickmode = "array",
                              tickvals = c(-0.5, seq(-0.25, 1.25, .25)),
                              ticktext = c("NA", as.character(seq(-0.25, 1.25, .25)))),
                 yaxis = list(range = c(input$affinity[1], input$affinity[2]),
                              title = "Mean Kd (nM)",
                              type = "log")
          ) %>% highlight("plotly_selected", color = I('red'), hoverinfo = "text")
        # if restoring from a bookmark, select previously selected points
        p$x$highlight$defaultValues = values$c.binding_data$name[points]
        p$x$highlight$color = "rgba(255,0,0,1)"
        p$x$highlight$off = "plotly_deselect"
        p %>% layout(dragmode = "select")
      })
      
      if(sum(values$points_selected) > 0) {
        d$selection(points, ownerId = "mainplot")
        values$points_selected = F
      }
      
      # display results table
      output$output_table = DT::renderDataTable({
        print("output_table")
        
        values$c.binding_data_sub = values$c.binding_data[d$selection(), 
                                                          -which(names(values$c.binding_data) %in% c("selectivity_plot")), drop = F]
        m2 = values$c.binding_data_sub
        dt <- values$c.binding_data[ , -which(names(values$c.binding_data) %in% 
                                                c("selectivity_plot")), drop = F]
        if(NROW(m2) == 0) {
          dt
        } else {
          m2
        }
      }, 
      extensions = 'Buttons',
      rownames = F, 
      options = list(
        columnDefs = list(list(visible=F, targets=match( c("investigation_bias", 
                                                           "wilcox_pval", "IC50_diff"), names(values$c.binding_data)) - 1 )),
        dom = 'lBfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        searchHighlight = TRUE,
        autoWidth = TRUE), server = T
      )
      proxy <<- dataTableProxy('output_table')
      
      # if(length(values$rows_selected_save > 0)) {
      #   print("select rows")
      #   proxy %>% selectRows(values$rows_selected_save)
      # }
      
    }
  }, ignoreInit = T)
  #})
  
  observeEvent(input$include_genes, {
    if(input$include_genes) {
      print("genes1")
      values$genes = c("", sort(unique(affinity_selectivity$symbol)))
      # all genes
    } else {
      print("genes2")
      values$genes = affinity_selectivity %>%
        filter(tax_id == 9606) %>% extract2("symbol") %>% unique() %>% sort() %>% c("", .)
      # just human genes
    }
    print(values$query_gene)
    if(length(values$query_gene) > 0 && values$query_gene != "") {
      if(values$query_gene %in% values$genes) {
        print("selectize1")
        updateSelectizeInput(session, inputId = "query_gene", label = "", choices = values$genes, selected = values$query_gene)
      }
    } else {
      print("selectize2")
      updateSelectizeInput(session, inputId = "query_gene", label = "", choices = values$genes,
                           options = list(
                             placeholder = 'Search for a gene target',
                             onInitialize = I('function() { this.setValue(""); }')
                           )
      )
    }
  })
  
  # Make other tables on row selection
  
  observeEvent(input$output_table_rows_selected, {
    print("table selection")
    showElement("result_row3")
    row = input$output_table_rows_selected
    # If restoring bookmarked session, select same rows as before
    if(length(values$rows_selected_save) > 0) {
      print("restore selections")
      row = values$rows_selected_save
      proxy %>%  selectRows(row)
      values$rows_selected_save = NULL
    }
    if(length(row) == 0) {
      hideElement("row3_col1")
      hideElement("row3_col2")
      hideElement("row3_col3")
      hideElement("button_row")
    } else{
      # show/hide the selection tables
      if(length(row) == 1) {
        showElement("row3_col1")
        hideElement("row3_col2")
        hideElement("row3_col3")
        showElement("button_row")
      } else if(length(row) == 2) {
        showElement("row3_col1")
        showElement("row3_col2")
        hideElement("row3_col3")
        showElement("button_row")
      } else if(length(row) == 3) {
        showElement("row3_col1")
        showElement("row3_col2")
        showElement("row3_col3")
        showElement("button_row")
      }
      for(i in 1:length(row)) {
        if(length(row) > 3) { break }
        name_data = paste("selection.binding_data", i, sep = "")
        name_display = paste("selection.display_table", i, sep = "")
        name_title = paste("selection.title", i, sep = "")
        name_file = paste0("selection.drug", i)
        if(NROW(values$c.binding_data_sub) > 0) {
          dt1 = values$c.binding_data_sub
        } else {
          dt1 = values$selection_table
        }
        drug = dt1$name[ row[i] ]
        hms_id = dt1$hms_id[ row[i] ]
        values[[name_title]] = paste0(hms_id, "; ", drug)
        values$num_selected = length(row)
        values[[name_file]] = drug
        
        values[[name_data]] = affinity_selectivity %>%
          filter(name == drug) %>%
          filter(`mean_Kd_(nM)` >= 10^input$affinity[1] | is.na(`mean_Kd_(nM)`)) %>%
          filter(`mean_Kd_(nM)` <= 10^input$affinity[2] | is.na(`mean_Kd_(nM)`)) %>%
          filter(`SD_Kd_(nM)` <= 10^input$sd | is.na(`SD_Kd_(nM)`)) %>%
          filter(n_measurements >= input$min_measurements) %>%
          mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
          arrange(selectivity_class, `mean_Kd_(nM)`) %>%
          mutate(`mean_Kd_(nM)` = round(`mean_Kd_(nM)`, 3))
        
        values[[name_display]] = values[[name_data]][,c(3,4,5)]
        output_name = paste("selection", i, sep = "")
      }
    }
  }, ignoreInit = T, ignoreNULL = F)
  
  observeEvent(input$clearButton, {
    proxy %>% selectRows(NULL)
    for(i in 1:3) {
      assign(paste0("values$selection.binding_data",i), NULL)
    }
    values$num_selected = 0
  })
  
  observe({
    print("render selection tables")
    output$selection1 = DT::renderDataTable(
      values$selection.display_table1,
      extensions = c('Buttons'),
      rownames = F, options = list(
        dom = 'tp',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        autoWidth = TRUE)
    )
    
    output$selection2 = DT::renderDataTable(
      values$selection.display_table2,
      extensions = c('Buttons'),
      rownames = F, options = list(
        dom = 'tp',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        autoWidth = TRUE)
    )
    
    output$selection3 = DT::renderDataTable(
      values$selection.display_table3,
      extensions = c('Buttons'),
      rownames = F, options = list(
        dom = 'tp',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        autoWidth = TRUE)
    )
    
    output$sel1_drug = renderText({ values$selection.title1 })
    output$sel2_drug = renderText({ values$selection.title2 })
    output$sel3_drug = renderText({ values$selection.title3 })
    
  })
  
  output$downloadBind <- downloadHandler(
    filename = function() {
      return(paste0("BindingData_", format(Sys.time(), "%Y%m%d_%I%M%S"), 
                    ".zip", sep = ""))
    },
    content = function(filename) {
      files_all = list(values$selection.binding_data1,
                       values$selection.binding_data2,
                       values$selection.binding_data3)
      # take only tables that exist
      drugs = NULL
      if(values$num_selected > 0) {
        files = files_all[1:values$num_selected]
        for(i in 1:3) {
          drugs = c(drugs, values[[paste0("selection.drug", i)]])
        }
      } else {
        files = NULL
        drugs = NULL
      }
      zipped_csv(files, filename, paste0("BindingData_", drugs), format(Sys.time(), "%Y%m%d_%I%M%S") )
    }, contentType = "application/zip"
  )
  
  session$allowReconnect(TRUE)
  
  list(
    bookmark = reactive({
      input$bookmark1
    })
  )
}
