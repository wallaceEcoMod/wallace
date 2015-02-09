library(shiny)
library(shinyIncubator)
library(dismo)
library(rgbif)
library(spThin)
library(ggplot2)

source("functions.R")


shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  GBIFsearch <- reactive({
    input$goName
    isolate({
      withProgress(message = "Searching GBIF...", {
        results <- occ_search(scientificName = input$gbifName, limit = 50, 
                              fields = 'minimal', hasCoordinate = TRUE)
        locs <- results$data[!is.na(results$data[,3]),][,c(4,3)]
        names(locs) <- c('lon', 'lat')
        values$df <- locs
      })
    })
  })
  
  output$GBIFtxt <- renderUI({
    if (input$goName == 0) return()
    input$goName
    GBIFsearch()
    name <- isolate(input$gbifName)
    str1 <- paste('Total records for', name, 'found:', nrow(values$df), '(limit 500).')
    str2 <- "Please enter bounding box coordinates for the analysis."
    HTML(paste(str1, str2, sep='<br/>'))
  })
  
  output$GBIFmap <- renderPlot({
    if (input$goMap == 0) return()
    input$goMap
    isolate({
      xl <- c(min(values$df$lon) - 5, max(values$df$lon) + 5)
      yl <- c(min(values$df$lat) - 5, max(values$df$lat) + 5)
      mapWorld <- borders('world', colour = 'white', fill = 'white')
      mp <- ggplot() + mapWorld + 
        theme(panel.background = element_rect(fill = 'lightblue')) +
        geom_point(data = values$df, mapping=aes(x = lon, y = lat), color = 'blue', size = 3) +
        coord_cartesian(xlim = xl, ylim = yl)
      print(mp)
    })
  })
  
  output$mapText <- renderText({
    if (input$goMap == 0) return()
    input$goMap
    isolate({
        ptsNum <- nrow(values$df)
      })
      paste('Currently displaying', ptsNum, 'points')
  })
  
  runThin <- reactive({
    input$goThin
    isolate({
      withProgress(message = "Thinning...", {
        df <- cbind(name=rep('sp', nrow(values$df)), values$df)
        output <- thin(df, 'lat', 'lon', 'name', thin.par = 150, 
                       reps = 10, locs.thinned.list.return = TRUE, write.files = FALSE)
        names(output[[1]]) <- c('lon', 'lat')
        values$df <- output[[1]]
      })
    })
  })
  
    output$thinText <- renderText({
      if (input$goThin == 0) return()
      input$goThin
      locsThin <- runThin()
      paste('Total records thinned to:', nrow(values$df))
    })
  
#   output$thinConsole <- renderPrint({
#     if (input$goThin == 0) return()
#     input$goThin
#     isolate({values[["log"]] <- capture.output(runThin())})
#     return(print(values[["log"]]))
#   })
})