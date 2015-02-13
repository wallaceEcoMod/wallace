list.of.packages <- c("shiny", "shinyIncubator", "ggplot2", "maps", "rgbif", "spThin")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
#library(shinyIncubator)
library(dismo)
library(rgbif)
library(spThin)
library(ENMeval)
library(dismo)
library(ggplot2)

#source("functions.R")


shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  GBIFsearch <- reactive({
    if (input$gbifName == '') return(NULL)
    input$goName
    isolate({
      withProgress(message = "Searching GBIF...", {
        results <- occ_search(scientificName = input$gbifName, limit = 50, 
                              fields = 'minimal', hasCoordinate = TRUE)
        locs <- results$data[!is.na(results$data[,3]),][,c(4,3)]
        names(locs) <- c('lon', 'lat')
        values$df <- locs
        c(nrow(locs), results$meta$count)
      })
    })
  })
  
  getData <- reactive({
    if (is.null(input$csvInput)) {
      return(NULL)
    } else {
      values$df <- read.csv(input$csvInput$datapath, header = TRUE)
    }
  })
  
  output$fileUploaded <- reactive({
    return(is.null(getData()))
  })
  
  output$gbifSearched <- reactive({
    return(is.null(GBIFsearch()))
  })
  
  output$occTbl <- renderTable({values$df})
  
  output$GBIFtxt <- renderText({
    if (input$goName == 0) return()
    input$goName
    out <- GBIFsearch()
    name <- isolate(input$gbifName)
    paste('Total records for', name, 'returned [', out[1], '] out of [', out[2], '] total (limit 500).')
  })
  
  output$CSVtxt <- renderText({
    if (is.null(input$csvInput)) return()
    getData()
    name <- values$df[1,1]
    paste('Total records for', name, '[', nrow(values$df), ']')
  })
  
  output$a <- renderText({
    if (is.null(input$csvInput)) return()
    'YIPPEE'})
  
  output$GBIFmap <- renderPlot({
    mapWorld <- borders('world', colour = 'white', fill = 'white')
    mp <- ggplot() + mapWorld + 
      theme(panel.background = element_rect(fill = 'lightblue'))
    if (input$goMap == 0) return(print(mp))
    input$goMap
    isolate({
      xl <- c(min(values$df$lon) - 5, max(values$df$lon) + 5)
      yl <- c(min(values$df$lat) - 5, max(values$df$lat) + 5)
      mp <- mp + geom_point(data = values$df, mapping=aes(x = lon, y = lat), color = 'blue', size = 3) +
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
      paste('Currently displaying [', ptsNum, '] points.')
  })
  
  runThin <- reactive({
    input$goThin
    isolate({
      withProgress(message = "Thinning...", {
        df <- cbind(name=rep('sp', nrow(values$df)), values$df)
        output <- thin(df, 'lat', 'lon', 'name', thin.par = input$thinDist, 
                       reps = 10, locs.thinned.list.return = TRUE, write.files = FALSE)
        names(output[[1]]) <- c('lon', 'lat')
        values$df <- output[[1]]
      })
    })
  })
  
    output$thinText <- renderText({
      if (input$goThin == 0) return()
      input$goThin
      runThin()
      paste('Total records thinned to [', nrow(values$df), '] points.')
    })
  
  runENMeval <- reactive({
    input$goEval
    isolate({
      withProgress(message = "Downloading Worldclim data...", {
        bioclim <- getData('worldclim', var = 'bio', res = 10)
      })
      rms <- seq(input$rms[1], input$rms[2], input$rmsBy)
      progress <- shiny::Progress$new()
      progress$set(message = "Evaluating ENMs...", value = 0)
      on.exit(progress$close())
      n <- length(rms) * length(input$fcs)
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$inc(amount = 1/n, detail = detail)
      }
      e <- ENMevaluate(values$df, bioclim, RMvalues = rms, fc = input$fcs, 
                       method = input$method, updateProgress = updateProgress)
      values$evalTbl <- e@results
#       withProgress(message = "Iteratively evaluating parameters in Maxent...", {
#         rms <- seq(input$rms[1], input$rms[2], input$rmsBy)
#         e <- ENMevaluate(values$df, bioclim, RMvalues = rms, fc = input$fcs, method = input$method)
#         values$evalTbl <- e@results
#       })
    })
  })
  
  output$evalTbl <- renderTable({values$evalTbl})
  
  output$evalTxt <- renderText({
    if (input$goEval == 0) return()
    input$goEval
    runENMeval()
    paste("Ran ENMeval and output table with", nrow(values$evalTbl), "rows.")
  })
  
#   output$thinConsole <- renderPrint({
#     if (input$goThin == 0) return()
#     input$goThin
#     isolate({values[["log"]] <- capture.output(runThin())})
#     return(print(values[["log"]]))
#   })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  outputOptions(output, 'gbifSearched', suspendWhenHidden=FALSE)
})
