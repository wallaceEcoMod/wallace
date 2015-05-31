# check package dependencies, and download if necessary
list.of.packages <- c("shiny", "ggplot2", "maps", "rgdal", "spThin", "colorRamps", "dismo", "rgeos", "XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# rgbif needs to be downloaded from source
if (!require('rgbif')) install.packages('rgbif', type='source')
# use devtools to install leaflet and new unreleased version of ENMeval from github
if (!require('devtools')) install.packages('devtools')
library(devtools)
if (!require('leaflet')) devtools::install_github('rstudio/leaflet')
#install_github("bobmuscarella/ENMeval@edits")
if (!require("DT")) devtools::install_github("rstudio/DT")

# load libraries
library(shiny)
library(rgbif)
library(spThin)
library(ENMeval)
library(dismo)
library(rgeos)
library(ggplot2)
library(leaflet)

source("functions.R")

shinyServer(function(input, output, session) {
  # this list carries data that is used by multiple reactive functions
  values <- reactiveValues()
  
  # query GBIF based on user input, remove duplicate records
  observeEvent(input$goName, {
    withProgress(message = "Searching GBIF...", {
      results <- occ_search(scientificName = input$gbifName, limit = input$occurrences, 
                            fields = c('name', 'decimalLongitude', 'decimalLatitude', 'basisOfRecord'), 
                            hasCoordinate = TRUE)
      if (results$meta$count != 0) {
        locs <- results$data[!is.na(results$data[,3]),][,c(1,3,4,2)]
        dup <- duplicated(locs)
        locs <- locs[!dup, ]
        names(locs)[2:3] <- c('lon', 'lat')
        
        popUpContent <- function(x) {
          as.character(tagList(
            tags$strong(paste("ID:", x['row'])),
            tags$br(),
            tags$strong(paste("Latitude:", x['lat'])),        
            tags$strong(paste("Longitude:", x['lon']))
          ))
        }
        locs$row <- row.names(locs)
        locs$pop <- unlist(apply(locs, 1, popUpContent))
        
        values$df <- locs
        values$gbifoccs <- locs
        
        inName <- isolate(input$gbifName)
        nameSplit <- length(unlist(strsplit(inName, " ")))
        
        if (nameSplit == 1 && !is.null(locs)) {
          x <- paste("Please input both genus and species names. More than one species with this genus was found.")
        } else {if (nameSplit == 1 && is.null(locs)) {
          x <- paste("Please input both genus and species names.")      
        } else {if (nameSplit != 1 && is.null(locs)) {
          x <- paste0('No records found for ', inName, ". Please check the spelling.")
        } else {if (nameSplit != 1 && !is.null(locs)) {
          x <- paste('Total records for', values$gbifoccs[1,1], 'returned [', nrow(locs),
                     '] out of [', results$meta$count, '] total (limit 500).
                    Duplicated records removed [', sum(dup), "].")
        }}}}
        output$GBIFtxt <- renderText(x)
        # render the GBIF records data table
        output$occTbl <- DT::renderDataTable({DT::datatable(values$df[,1:4])})
      }
    })
  })
  
  # governs point removal behavior and modifies tables in "values"
  observe({
    if (input$remove == 0) return()
    isolate({
      rows <- as.numeric(rownames(values$gbifoccs))
      remo <- which(input$num == rows)
      if(length(remo)>0){
        values$df <- values$gbifoccs[-remo, ]
        values$gbifoccs <- values$gbifoccs[-remo, ]
      }
    })  
  })
  
  # handle downloading of GBIF csv
  output$downloadGBIFcsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$gbifoccs, file)
    }
  )
  
  # map of GBIF records
  map <- leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2)
  output$map <- renderLeaflet(map)
  
  # proxies
  proxy <- leafletProxy("map")
  
  observeEvent(input$goName, {
    proxy %>% clearShapes()
    lati <- values$gbifoccs[,3]
    longi <- values$gbifoccs[,2]
    proxy %>% fitBounds(min(longi), min(lati), max(longi), max(lati))
    
    # this section makes letter icons for occs based on basisOfRecord
    #     occIcons <- makeOccIcons()
    #     iconList <- list(HUMAN_OBSERVATION=1, OBSERVATION=2, PRESERVED_SPECIMEN=3, 
    #                      UNKNOWN_EVIDENCE=4, FOSSIL_SPECIMEN=5, MACHINE_OBSERVATION=6, 
    #                      LIVING_SPECIMEN=7, LITERATURE_OCCURRENCE=8, MATERIAL_SAMPLE=9)
    #     values$gbifoccs$basisNum <- unlist(iconList[values$gbifoccs$basisOfRecord])
    #     proxy %>% addMarkers(data = values$gbifoccs, lat = ~lat, lng = ~lon, 
    #                          layerId = as.numeric(rownames(values$gbifoccs)), 
    #                          icon = ~icons(occIcons[basisNum]))
    
    proxy %>% addCircleMarkers(data = values$gbifoccs, lat = ~lat, lng = ~lon, 
                               layerId = as.numeric(rownames(values$gbifoccs)), 
                               radius = 5, color = 'red', fill = FALSE, weight = 2,
                               popup = ~pop)
  })
  
  # map thinned records when Thin button is pressed
  observeEvent(input$goThin, {
    withProgress(message = "Thinning...", {
      output <- thin(values$gbifoccs, 'lat', 'lon', 'name', thin.par = input$thinDist, 
                     reps = 10, locs.thinned.list.return = TRUE, write.files = FALSE,
                     verbose = FALSE)
      # pull max, not first
      #       thinout <- cbind(rep(values$gbifoccs[1,1], nrow(output[[1]])), output[[1]])
      #       names(thinout) <- c('name', 'lon', 'lat')
      thinned <- values$gbifoccs[rownames(output[[1]]),]
      values$df <- thinned
      values$thinoccs <- thinned
    })
    output$thinText <- renderText(paste('Total records thinned to [', nrow(thinned), '] points.'))
    # render the thinned records data table
    output$occTbl <- DT::renderDataTable({DT::datatable(values$df[,1:4])})
    
    lati2 <- values$thinoccs[,3]
    longi2 <- values$thinoccs[,2]
    #     proxy %>% fitBounds(min(longi2), min(lati2), max(longi2), max(lati2))
    proxy %>% addCircleMarkers(data = values$gbifoccs, lat = ~lat, lng = ~lon, 
                               layerId = as.numeric(rownames(values$gbifoccs)), 
                               radius = 5, color = 'red', fill = FALSE, weight = 2,
                               popup = ~pop) %>%
      addCircleMarkers(data = values$thinoccs, lat = ~lat, lng = ~lon, 
                       layerId = as.numeric(rownames(values$thinoccs)), 
                       radius = 5, color = 'blue', fill = FALSE, weight = 2,
                       popup = ~pop)
  })
  
  # handle download for thinned records csv
  output$downloadThincsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_thinned_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$thinoccs, file)
    }
  )
  
  observe({
    ## Check if predictor path exists. If not, use the dismo function getData()
    if (input$pred == "" || input$pred == 'user') return()
    if (!is.null(values$df)) {
      ## Check if predictor path exists. If not, use the dismo function getData()
      withProgress(message = "Downloading WorldClim data...", {
        values$pred <- getData(name = "worldclim", var = "bio", res = input$pred)
      })
      
      withProgress(message = "Processing...", {
        locs.vals <- extract(values$pred[[1]], values$df[,2:3])
        values$df <- values$df[!is.na(locs.vals),]        
      })
      str1 <- paste("Using WorldClim bio1-19 at", input$pred, " arcmin resolution.")
      if (sum(is.na(locs.vals)) > 0) {
        str2 <- paste0("Removed records with NA environmental values with IDs: ", 
                       paste(row.names(values$df[is.na(locs.vals),]), collapse=', '), ".")
      } else {
        str2 <- ""
      }
      output$predTxt1 <- renderUI({HTML(paste(str1, str2, sep = '<br/>'))})
    }
  })
  
  
  # future user input functionality for rasters
  #   output$predTxt2 <- renderUI({
  #     if (input$userPred == "") return()
  #     isolate({
  #       files <- file.path(input$userPred, list.files(input$userPred))
  #       values$pred <- stack(files)
  #       paste("Using user-provided environmental data.")
  #     })
  #   })
  
  observe({
    if (input$backg == "") return()
    # generate background extent
    if (input$backg == 'bb') {
      xmin <- min(values$df$lon) - (input$backgBuf + res(values$pred)[1])
      xmax <- max(values$df$lon) + (input$backgBuf + res(values$pred)[1])
      ymin <- min(values$df$lat) - (input$backgBuf + res(values$pred)[1])
      ymax <- max(values$df$lat) + (input$backgBuf + res(values$pred)[1])
      bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)
      values$backgExt <- SpatialPolygons(list(Polygons(list(Polygon(bb)), 1)))
    } else if (input$backg == 'mcp') {
      xy_mcp <- mcp(values$df[,2:3])
      xy_mcp <- gBuffer(xy_mcp, width = input$backgBuf + res(values$pred)[1])
      values$backgExt <- xy_mcp
      bb <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords
    }
    lati <- values$df[,2:3][,2]
    longi <- values$df[,2:3][,1]
    proxy %>% fitBounds(max(lati), max(longi), min(lati), min(longi))
    proxy %>% addPolygons(lng=bb[,1], lat=bb[,2], layerId="1",
                          options= list(weight=10, col="red"))

  })
  
  # clip and mask rasters based on study region, make random points for background, run ENMeval via user inputs
  runENMeval <- reactive({
    if (input$goEval == 0) return()
    input$goEval
    isolate({
      validate(
        need(input$method != "randomkfold" && input$method != "user", "Please select a functional method.")
      )
      withProgress(message = "Processing environmental rasters...", {
        #preds <- stack(values$predPath)
        preds <- values$pred        
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
      values$evalPreds <- e@predictions
      # render table of ENMeval results
      output$evalTbl <- DT::renderDataTable({DT::datatable(values$evalTbl)})
    })
  })
  
  # out text for ENMeval run
  output$evalTxt <- renderText({
    if (input$goEval == 0) return()
    input$goEval
    runENMeval()
    if (!is.null(values$evalTbl)) {
      paste("ENMeval ran successfully and output table with", nrow(values$evalTbl), "rows.")  
    }
  })
  
  # plotting functionality for ENMeval graphs
  output$evalPlot <- renderPlot ({
    if (input$goEval == 0) return()
    if (!is.null(values$evalTbl)) {
      par(mfrow=c(3,2))
      fc <- length(unique(values$evalTbl$features))
      col <- rainbow(fc)
      rm <- length(unique(values$evalTbl$rm))
      plot(rep(1, times=fc), 1:fc, ylim=c(.5,fc+1), xlim=c(0,3), axes=F, ylab='', xlab='', cex=2, pch=21, bg=col)
      segments(rep(.8, times=fc), 1:fc, rep(1.2, times=fc), 1:fc, lwd=1, col=col)
      points(rep(1, times=fc), 1:fc, ylim=c(-1,fc+2), cex=2, pch=21, bg=col)
      text(x=rep(1.3, times=fc), y=1:fc, labels=unique(values$evalTbl$features), adj=0)
      text(x=1, y=fc+1, labels="Feature Classes", adj=.20, cex=1.3, font=2)
      eval.plot(values$evalTbl, legend=FALSE, value="delta.AICc")
      eval.plot(values$evalTbl, legend=FALSE, value="Mean.AUC", variance="Var.AUC")
      eval.plot(values$evalTbl, legend=FALSE, value="Mean.AUC.DIFF", variance="Var.AUC.DIFF")
      eval.plot(values$evalTbl, legend=FALSE, value="Mean.ORmin", variance="Var.ORmin")
      eval.plot(values$evalTbl, legend=FALSE, value="Mean.OR10", variance="Var.OR10")
    }
  })
  
  # handle downloads for ENMeval results table csv
  output$downloadEvalcsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_enmeval_results.csv")},
    content = function(file) {
      write.csv(values$evalTbl, file)
    }
  )
  
  # generates user selection of rasters to plot dynamically after they are created
  output$predSel <- renderUI({
    if (is.null(values$evalPreds)) return()
    n <- names(values$evalPreds)
    predNameList <- setNames(as.list(seq(1, length(n))), n)
    values$rdyPlot <- 'rdy'
    selectInput("predSelServer", label = "Choose a model",
                choices = predNameList)
  })
  
  # plot functionality for rasters -- currently uses plotMap
  output$plotPred <- renderPlot({
    plotMap(pred = values$evalPreds[[as.numeric(input$predSelServer)]], 
            pts2 = values$df, addpo = input$plotpoints)
  })
  
  # handle download for rasters, as TIFF
  output$downloadPred <- downloadHandler(
    filename = function() {paste0(names(values$evalPreds[[as.numeric(input$predSelServer)]]), "_pred.tif")},
    content = function(file) {
      res <- writeRaster(values$evalPreds[[as.numeric(input$predSelServer)]], file, format = "GTiff", overwrite = TRUE)
      file.rename(res@file@name, file)
    }
  )
  
  # legacy console printing for spThin
  #   output$thinConsole <- renderPrint({
  #     if (input$goThin == 0) return()
  #     input$goThin
  #     isolate({values[["log"]] <- capture.output(runThin())})
  #     return(print(values[["log"]]))
  #   })
  
})
