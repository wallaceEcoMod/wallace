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
library(rgeos)
library(ggplot2)

#source("functions.R")

mcp <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1))))
}

shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  GBIFsearch <- reactive({
    if (input$gbifName == '') return(NULL)
    input$goName
    isolate({
      withProgress(message = "Searching GBIF...", {
        results <- occ_search(scientificName = input$gbifName, limit = 50, 
                              fields = 'minimal', hasCoordinate = TRUE)
        locs <- results$data[!is.na(results$data[,3]),][,c(1,4,3)]
        names(locs) <- c('spname', 'lon', 'lat')
        values$df <- locs
        c(nrow(locs), results$meta$count)
      })
    })
  })
  
  output$occTbl <- renderTable({values$df})
  
  output$GBIFtxt <- renderText({
    if (input$goName == 0) return()
    input$goName
    out <- GBIFsearch()
    name <- isolate(input$gbifName)
    paste('Total records for', name, 'returned [', out[1], '] out of [', out[2], '] total (limit 500).')
  })
  
  plotMap <- function(pts=NULL, poly=NULL) {
    mapWorld <- borders('world', colour = 'white', fill = 'white')
    mp <- ggplot() + mapWorld + 
      theme(panel.background = element_rect(fill = 'lightblue'))
    if (!is.null(pts)) {
      xl <- c(min(pts$lon) - 5, max(pts$lon) + 5)
      yl <- c(min(pts$lat) - 5, max(pts$lat) + 5)
      mp <- mp + geom_point(aes(x = lon, y = lat), pts, color = 'blue', size = 3) +
        coord_cartesian(xlim = xl, ylim = yl)
      if (!is.null(poly)) {
        mp <- mp + geom_path(aes(x = long, y = lat), fortify(poly), color = 'red')
      }
    }
    print(mp)
  }
  
  output$GBIFmap <- renderPlot({
    if (input$goMap == 0) return(plotMap())
    input$goMap
    makeBackgExt()
    isolate({
      plotMap(values$df, values$backgExt)
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
        output <- thin(values$df, 'lat', 'lon', 'spname', thin.par = input$thinDist, 
                       reps = 10, locs.thinned.list.return = TRUE, write.files = FALSE)
        names(output[[1]]) <- c('lon', 'lat')
        values$df <- cbind(rep(values$df[1,1], nrow(output[[1]])), output[[1]])
      })
    })
  })
  
  output$thinText <- renderText({
    if (input$goThin == 0) return()
    input$goThin
    runThin()
    paste('Total records thinned to [', nrow(values$df), '] points.')
  })
  
  makeBackgExt <- reactive({
    if (input$backg == "") return()
    if (input$backg == 'bb') {
      xmin <- min(values$df$lon) - input$backgBuf
      xmax <- max(values$df$lon) + input$backgBuf
      ymin <- min(values$df$lat) - input$backgBuf
      ymax <- max(values$df$lat) + input$backgBuf
      bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)
      bb <- SpatialPolygons(list(Polygons(list(Polygon(bb)), 1)))
      values$backgExt <- bb
    } else if (input$backg == 'mcp') {
      xy_mcp <- mcp(values$df[,2:3])
      xy_mcp <- gBuffer(xy_mcp, width = input$backgBuf)
      values$backgExt <- xy_mcp
    }
  })
  
  output$predExtTxt <- renderText({
    if (input$pred == "") return()
    path <- paste0('worldclim/', input$pred)
    values$predPath <- file.path(path, list.files(path, pattern = "bil$"))
    paste("Using Worldclim bio1-19 at", substring(input$pred, 3), "resolution.")
  })
  
  runENMeval <- reactive({
    input$goEval
    isolate({
      withProgress(message = "Processing prediction rasters...", {
        preds <- stack(values$predPath)
        preds <- crop(preds, values$backgExt)
        preds <- mask(preds, values$backgExt)
      })
      withProgress(message = "Generating background points...", {
        backg_pts <- randomPoints(preds, 10000)
      })
      
      rms <- seq(input$rms[1], input$rms[2], input$rmsBy)
      progress <- shiny::Progress$new()
      progress$set(message = "Evaluating ENMs...", value = 0)
      on.exit(progress$close())
      n <- length(rms) * length(input$fcs)
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$inc(amount = 1/n, detail = detail)
      }
      e <- ENMevaluate(values$df[,2:3], preds, bg.coords = backg_pts, RMvalues = rms, fc = input$fcs, 
                       method = input$method, updateProgress = updateProgress)
      values$evalTbl <- e@results
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

})
