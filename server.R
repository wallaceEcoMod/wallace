library(shiny)
library(shinyIncubator)
library(dismo)
library(rgbif)
library(spThin)
library(ggplot2)

source("functions.R")



shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  curMap <- reactiveValues(cur=1)
  
  GBIFsearch <- reactive({
    input$goName
    curMap[['cur']] <- 1
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
  
  output$GBIFmap <- renderPlot({
    if (input$goMap == 0) return()
    input$goMap
    isolate({
      if (curMap[['cur']] == 1) {
        results <- GBIFsearch()
        df <- results$data[!is.na(results$data[,3]),][,c(4,3)]
        if (nrow(df) > 0) drawMap(df)  
      } else {
        df <- runThin()[[1]]
        drawMap(df)
      }
    })
  })
  
  output$mapText <- renderText({
    if (input$goMap == 0) return()
    input$goMap
    isolate({
      if (curMap[['cur']] == 1) {
        results <- GBIFsearch()
        df <- results$data[!is.na(results$data[,3]),][,c(4,3)]
        if (nrow(df) > 0) ptsNum <- nrow(df)
      } else {
        df <- runThin()[[1]]
        ptsNum <- nrow(df)
      }
      paste('Currently displaying', ptsNum, 'points')
    })
  })
  
  runThin <- reactive({
    input$goThin
    curMap[['cur']] <- 2
    isolate({
      withProgress(message = "Thinning...", {
        results <- GBIFsearch()
        df <- results$data[!is.na(results$data[,3]),][,c(1,4,3)]
        thin(df, 'decimalLatitude', 'decimalLongitude', 'name', 
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
  
  output$writeCurMap <- renderText({curMap[['cur']]})
  
})