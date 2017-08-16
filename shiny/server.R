function(input, output, session) {
    
  # create reactive copy of myroadless to save checkbox inputs
  myroadless_reactive <- reactiveValues(
    myroadless = myroadless
  )
  
  # create reactive object to hold class-wide checks
  results_list_reactive <- reactiveValues(
    results_list = list()
  )
  
  # for buttons and checkboxes in the datatable
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    
    # assemble columns for datatable
    df <- cbind(
      # column to contain buttons that generate google map
      Visualize = shinyInput(actionButton, nsamp, 'button_', label = "Show on Map", 
                          onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ), 
      # myroadless df containing Lat, Lon
      myroadless[, c('Latitude', 'Longitude')], 
      # on land checkbox
      OnLand = shinyInput(checkboxInput, nsamp, id = "onland_", label = "", value = 0, width=1), 
      # within 1 mile checkbox
      Within1Mile = shinyInput(checkboxInput, nsamp, id = "within1_", label = "", value = 0, width=1)
    )
    
    # display datatable
    output$data <- DT::renderDataTable(
      DT::datatable(df, options = list(ordering = FALSE,
                                       dom = 'tip', 
                                       # lengthMenu = c(10, 25, 50),
                                       pageLength = 10,
                                       drawCallback= JS(
                                          'function(settings) {
                                          Shiny.bindAll(this.api().table().node());}')
                                        ), 
                        escape = FALSE, selection = 'none'
                    )
    )
    
    # function for google map button
    observeEvent(input$select_button, {
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      getLocation(selectedRow)
    })

    # react to clicking row
    rowSelect <- reactive({

      # get on land checks
      rows=names(input)[grepl(pattern = "onland_", names(input))]
      onland_checks <- paste(unlist(lapply(rows,function(i){
        if(input[[i]]==T){
          return(substr(i,gregexpr(pattern = "_",i)[[1]]+1,nchar(i)))
        }
      })))
      
      # get within 1 mile checks
      rows=names(input)[grepl(pattern = "within1_", names(input))]
      within1_checks <- paste(unlist(lapply(rows,function(i){
        if(input[[i]]==T){
          return(substr(i,gregexpr(pattern = "_",i)[[1]]+1,nchar(i)))
        }
      })))
      
      return(list(onland_checks, within1_checks))
      
     })
    
    # when a row is clicked & checkbox selected, 
    # (1) modify the reactive copy of `myroadless`
    # (2) update binom.test()
    observe({
      # indices of on land checkboxes that are checked
      onland_checks <- rowSelect()[[1]]
      # indices of within 1 mile checkboxes that are checked
      within1_checks <- rowSelect()[[2]]
      
      # where checked, make cell in myroadless 1
      myroadless_reactive$myroadless[onland_checks, 'OnLand'] <- 1
      myroadless_reactive$myroadless[within1_checks, 'Within1Mile'] <- 1
      
      # where not checked, make cell in myroadless NA
      myroadless_reactive$myroadless[setdiff(1:nsamp, onland_checks), 'OnLand'] <- NA
      myroadless_reactive$myroadless[setdiff(1:nsamp, within1_checks), 'Within1Mile'] <- NA
      
      # num within 1 mile
      # note: only count "within 1 mile" if the "on land" is also checked
      k <- length(intersect(rowSelect()[[1]], rowSelect()[[2]]))
      
      # total on land
      n <- length(rowSelect()[[1]])
      
      # if any locations marked as on land
      if (n > 0) {
        binom <- binom.test(k, n)
        
        # proportion
        prop.est <- round(k/n, 3)

        # confidence interval
        cont.int <- paste(round(binom$conf.int[1], 3), round(binom$conf.int[2], 3), sep =", ")
        
      } else {
        # if no locations on land
        
        # proportion
        prop.est <- 'NA'
        
        #  confidence interval
        cont.int <- 'NA'
      }
      
      # update my roadless results
      output$results <- renderUI({
        str1 <- paste0("Locations within 1 Mile: ", k)
        str2 <- paste0("Total on Land: ", n)
        str3 <- paste0("Estimated Proportion: ", prop.est)
        str4 <- paste0("Confidence Interval: ", cont.int)
        
        results.text <- paste(str1, str2, str3, str4, sep = '<br/>')
        
        HTML(paste0('<div class="results">', results.text, '</div>'))
      })
      
    })

    # download the edited myroadless csv
    observeEvent(input$download, {
      filename <- paste0('myroadless', Sys.Date(), '.csv')
      write.csv(myroadless_reactive$myroadless, filename, row.names=FALSE)
    })
    
    # upload myroadless csv to class spreadsheet
    observeEvent(input$upload, {
      # prompt user to input google credentials in browser
      if (!exists("class_roadless")) {
        gs_auth(new_user = TRUE)
      }
      
      # get key to class roadless
      key <- input$key
      
      # get student name
      name <- input$name
      
      # --- INSERT ALERT IF KEY OR NAME INVALID
      
      # register class roadless
      class_roadless <- key %>%
        gs_key()
      
      # if class roadless already has sheet with this name, delete
      if (name %in% gs_ws_ls(class_roadless)) {
        class_roadless <- class_roadless %>% 
          gs_ws_delete(ws = name)
      }
      
      # filter myroadless sheet to be uploaded to only On Land observations
      myroadless_upload <- myroadless_reactive$myroadless %>% 
        filter(!is.na(OnLand), 
               OnLand == 1)
      
      # upload
      class_roadless <- class_roadless %>% 
      gs_ws_new(ws_title = name, input = myroadless_upload,
                trim = TRUE, verbose = FALSE)
    })

    # download class roadless results
    observeEvent(input$download_class, {
      # prompt user to input google credentials in browser
      if (!exists("class_roadless")) {
        gs_auth(new_user = TRUE)
        
        # get key to class roadless
        key <- input$key
      }
      
      # register class roadless (also needed simply to refresh worksheet list)
      class_roadless <- key %>%
        gs_key()
      
      # wait a sec... :-)
      Sys.sleep(1)

      # download results
      worksheets <- gs_ws_ls(class_roadless) # get list of worksheets
      Sys.sleep(1)

      results_list_reactive$results_list <- list()
      i <- 1
      for(ws in worksheets) {
        # read in single worksheet
        group_ws <- class_roadless %>% gs_read_csv(ws = ws)
        Sys.sleep(5) # otherwise Too Many Requests error
        results_list_reactive$results_list[[i]] <- group_ws
        i <- i + 1
      }
      results <- bind_rows(results_list_reactive$results_list) # combine all worksheets
      # dedupe results and make sure all observations on land
      results <- results[!duplicated(results[, c('Latitude', 'Longitude')]), ] %>%
        filter(!is.na(OnLand),
               OnLand == 1)

      # class-wide N, class-wide K
      class_n <- sum(results$OnLand, na.rm=TRUE)
      class_k <- sum(results$Within1Mile, na.rm=TRUE)

      if(class_n > 0) {

        class_binom <- binom.test(class_k, class_n)

        # proportion
        class.prop.est <- round(class_k/class_n, 3)

        # confidence interval
        class.cont.int <- paste(round(class_binom$conf.int[1], 3),
                                round(class_binom$conf.int[2], 3), sep =", ")
      } else {
        # proportion
        class.prop.est <- 'NA'
        
        # confidence interval
        class.cont.int <- 'NA'
      }
      
      # update class-wide results
      output$class.results <- renderUI({
        str1 <- paste0("Locations within 1 Mile: ", class_k)
        str2 <- paste0("Total on Land: ", class_n)
        str3 <- paste0("Estimated Proportion: ", class.prop.est)
        str4 <- paste0("Confidence Interval: ", class.cont.int)
        
        results.text <- paste(str1, str2, str3, str4, sep = '<br/>')
        
        HTML(paste0('<div class="results">', results.text, '</div>'))
      })
      
    })
    
    output$plot <- renderPlot({

      # declare dataframe to hold confidence intervals for student groups
      ci.df <- data.frame()
      
      if (length(results_list_reactive$results_list) > 0) {
        # generate group CIs
        ci.df <- generateGroupCIs(results_list_reactive$results_list)
      }

      if (nrow(ci.df) > 0) {
        # generate plot
        plot.new()
        
        # draw plot with correct number of spaces (num student groups + spot for class-wide)
        plot(c(1, nrow(ci.df)),
             c(0, 1), xlab="Group", xaxt="n",
             ylab="P(Within 1 Mile)", pch=" ") 
        
        # add point estimate, CI for each group
        col='blue'
        for (pt in 1:nrow(ci.df)) {
          # last row in dataframe is class-wide. color differently.
          if (pt == nrow(ci.df)) {
            col='red'
          }
          points(pt, ci.df[pt, 'est'], pch=8)
          lines(c(pt, pt), ci.df[pt, c('ci1', 'ci2')], col=col)
        }
        
      }

    })
}