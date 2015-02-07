library(shiny)
library(shinyIncubator)
#source("helpers.R")
library(dismo)
library(rgbif)
library(spThin)

library(ggplot2)


shinyServer(function(input, output, session) {

  values <- reactiveValues()
  
  GBIFsearch <- reactive({
    input$goName
    isolate({
      withProgress(message = "Searching GBIF...", {
        occ_search(scientificName = input$gbifName, limit = 50, 
                   fields = 'minimal', hasCoordinate = TRUE)
      })
    })
  })
  
  output$GBIFtxt <- renderUI({
    if (input$goName == 0) return()
    input$goName
    results <- GBIFsearch()
    name <- isolate(input$gbifName)
    str1 <- paste('There are', results$meta$count, 'total records for', name, '(limit 500 displayed).')
    str2 <- "Please enter bounding box coordinates for the analysis."
    HTML(paste(str1, str2, sep='<br/>'))
  })
  
  mapGBIF <- reactive({
    input$goName
    isolate({
      results <- GBIFsearch()
      df <- results$data[!is.na(results$data[,3]),][,c(4,3)]
      mapWorld <- borders('world', colour='white', fill='white')
      mp <- ggplot() + mapWorld + 
        theme(panel.background = element_rect(fill = 'lightblue')) +
        geom_point(aes(x = df[,1], y = df[,2]), color = 'blue', size = 3) +
        coord_cartesian(xlim=c(min(df[,1]) - 5, max(df[,1]) + 5), 
                        ylim=c(min(df[,2]) - 5, max(df[,2]) + 5))
      mp
    })
  })
  
  output$GBIFmap <- renderPlot({
    if (input$goName == 0) return()
    input$goName
    results <- GBIFsearch()
    if (nrow(results$data) > 0) mapGBIF()
#     } else {
#       locsThin <- runThin()
#       mapGBIF(locsThin[[1]])
    
  })
  
  runThin <- reactive({
    input$goThin
    isolate({
      withProgress(message = "Thinning...", {
        results <- GBIFsearch()
        df <- results$data[!is.na(results$data[,3]),][,c(1,4,3)]
        locsThin <- thin(df, 'decimalLatitude', 'decimalLongitude', 'name', 
                         thin.par = 25, reps = 10, locs.thinned.list.return = TRUE, 
                         write.files = FALSE)
      })
    })
  })

#   output$thinTxt <- renderText({
#     if (input$goThin == 0) return()
#     input$goThin
#     locsThin <- runThin()
#     name <- input$gbifName
#     paste('Thinned records for', name, 'with', nrow(locsThin[[1]]), 'records')
#   })
  
  output$thinConsole <- renderPrint({
    if (input$goName == 0 | input$goThin == 0) return()
    input$goThin
    isolate({values[["log"]] <- capture.output(runThin())})
    return(print(values[["log"]]))
  })
  
    
})

