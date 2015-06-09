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

# dev version of leaflet for overlaying rasters
#devtools::install_github("jcheng5/rasterfaster")
#devtools::install_github("rstudio/leaflet@joe/feature/raster-image")

shinyServer(function(input, output, session) {
  # make list to carry data used by multiple reactive functions
  values <- reactiveValues()
  
  output$log <- renderUI({tags$div(id='header', "LOG",
                         tags$div(id='header-content', HTML(paste0(values$log, "<br>", collapse = ""))))})
  
  # create map
  map <- leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2)
  output$map <- renderLeaflet(map)
  
  # make map proxy to make further changes to existing map
  proxy <- leafletProxy("map")
  
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
        values$log <- paste(values$log, x, sep='<br>')
      }
    })
  })
  
  observe({
    if (is.null(values$df)) return()
    # render the GBIF records data table
    output$occTbl <- DT::renderDataTable({DT::datatable(values$df[,1:4])})
  })
  
  observe({
    if (is.null(input$userCSV)) return()
    inFile <- read.csv(input$userCSV$datapath, header = TRUE)
    
    values$spname <- inFile[1,1]
    names(inFile)[2:3] <- c('lon', 'lat')
    if (!("basisOfRecord" %in% names(inFile))) {
      inFile$basisOfRecord <- NA
    }
    inFile$row <- row.names(inFile)
    inFile$pop <- unlist(apply(inFile, 1, popUpContent))
    values$gbifoccs <- isolate(rbind(values$gbifoccs, inFile))
    values$df <- isolate(rbind(values$df, inFile))
    # this makes an infinite loop. not sure why...
#     x <- paste0("User input ", input$userCSV$name, " with [", nrow(values$df), "[ records.")
#     values$log <- paste(values$log, x, sep='<br>')
  })
    
  
  # map gbif occs
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
  
  # functionality for drawing polygons on map
  observe({
    click <- input$map_click
    if (is.null(click)) return()
    latlng <- c(click$lng, click$lat)
    values$drawPolyCoords <- isolate(rbind(values$drawPolyCoords, latlng))
    proxy %>% removeShape("drawPoly")
    proxy %>% addPolygons(values$drawPolyCoords[,1], values$drawPolyCoords[,2], 
                          layerId='drawPoly', fill=FALSE, weight=3, color='green')      
  })
  
  # erase poly with button click
  observeEvent(input$erasePoly, {
    proxy %>% clearShapes()
    values$drawPolyCoords <- NULL
    values$ptsSel
    if (!is.null(values$gbifoccs)) {
      values$df <- values$gbifoccs
      proxy %>% addCircleMarkers(data = values$df, lat = ~lat, lng = ~lon, 
                                 layerId = as.numeric(rownames(values$df)), 
                                 radius = 5, color = 'red', fill = FALSE, weight = 2,
                                 popup = ~pop)
    }
  })
  
  # select points intersecting drawn polygon (replace values$df)
  observeEvent(input$selectPoly, {
    pts <- SpatialPoints(values$gbifoccs[,2:3])
    poly <- SpatialPolygons(list(Polygons(list(Polygon(values$drawPolyCoords)), ID='p')))
    ptsSel <- values$gbifoccs[!(is.na(over(pts, poly))),]
    proxy %>% addCircleMarkers(data = ptsSel, lat = ~lat, lng = ~lon, 
                               layerId = as.numeric(rownames(ptsSel)), 
                               radius = 5, color = 'red', fill = TRUE, fillColor = 'yellow', 
                               weight = 2, popup = ~pop, fillOpacity=1)
    values$ptsSel <- ptsSel
    values$df <- ptsSel
  })
  
  observe({
    if (input$tabs != "1) Get Data") {
      proxy %>% clearShapes()
      proxy %>% clearMarkers()
      proxy %>% addCircleMarkers(data = values$df, lat = ~lat, lng = ~lon, 
                                 layerId = as.numeric(rownames(values$df)), 
                                 radius = 5, color = 'red', fill = FALSE, weight = 2,
                                 popup = ~pop)
    }
  })
  
  # handle downloading of GBIF csv
  output$downloadGBIFcsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$gbifoccs, file, row.names=FALSE)
    }
  )
  
  # map thinned records when Thin button is pressed
  observeEvent(input$goThin, {
    withProgress(message = "Thinning...", {
      output <- thin(values$gbifoccs, 'lat', 'lon', 'name', thin.par = input$thinDist, 
                     reps = 10, locs.thinned.list.return = TRUE, write.files = FALSE,
                     verbose = FALSE)
      # pull max, not first (don't think this is implemented yet)
      # this code is old, and doesn't do what it says it does...
      #       thinout <- cbind(rep(values$gbifoccs[1,1], nrow(output[[1]])), output[[1]])
      #       names(thinout) <- c('name', 'lon', 'lat')
      thinned <- values$gbifoccs[rownames(output[[1]]),]
      values$df <- thinned
      values$thinoccs <- thinned
    })
    values$log <- paste(values$log, paste('Total records thinned to [', nrow(thinned), '] points.'), sep='<br>')
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
      str1 <- paste("Using WorldClim bio1-19 at", input$pred, " arcmin resolution.")
      withProgress(message = "Processing...", {
        locs.vals <- extract(values$pred[[1]], values$df[,2:3])
        values$df <- values$df[!is.na(locs.vals),]        
      })
      if (sum(is.na(locs.vals)) > 0) {
        str2 <- paste0("Removed records with NA environmental values with IDs: ", 
                       paste(row.names(values$df[is.na(locs.vals),]), collapse=', '), ".")
      } else {
        str2 <- ""
      }
    }
    values$predTxt <- paste(str1, str2, sep='<br>')
  })
  
  # this is necessary because the above is not observeEvent, and thus for some
  # reason when values$log is modified within observe, there's an infinite loop
  observe({
    if (!is.null(values$predTxt)) {
      values$log <- paste(values$log, values$predTxt, sep='<br>')
      values$predTxt <- NULL
      print(values$log)
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
    lati <- values$df[,3]
    longi <- values$df[,2]
    values$bb <- bb
    #proxy %>% fitBounds(max(lati), max(longi), min(lati), min(longi))
    proxy %>% addPolygons(lng=bb[,1], lat=bb[,2], layerId="backext",
                          options= list(weight=10, col="red"))

  })
  
  # removes backext polygon if not on tab 3
  observe({
    if (input$tabs != "3) Variables") {
      proxy %>% removeShape(layerId='backext')
    } else {
      if (!is.null(values$bb)) {
        proxy %>% addPolygons(lng=values$bb[,1], lat=values$bb[,2], layerId="backext",
                              options= list(weight=10, col="red"))  
      }
    }
  })
  
  # clip and mask rasters based on study region, make random points for background, run ENMeval via user inputs
  observeEvent(input$goEval, {
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
    print(e@results)
    # render table of ENMeval results
    # code to do fixed columns -- problem is it makes the page selection disappear and you
    # can't seem to pan around the table to see the other rows... likely a bug
    # (extensions=list(FixedColumns=list(leftColumns=2)), options=list(dom='t', scrollX=TRUE, scrollCollapse=TRUE))
    output$evalTbl <- DT::renderDataTable({DT::datatable(cbind(e@results[,1:3], round(e@results[,4:15], digits=3), nparam=e@results[,16]))})
    
    if (!is.null(values$evalTbl)) {
      x <- paste("ENMeval ran successfully and output table with", nrow(values$evalTbl), "rows.")
      values$log <- paste(values$log, x, sep='<br>')
      # plotting functionality for ENMeval graphs
      output$evalPlot <- renderPlot({
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
      })
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
  
  # plot raster on proxy
  observeEvent(input$plotRas, {
    if (input$tabs == "5) Predict") {
      values$predCur <- values$evalPreds[[as.numeric(input$predSelServer)]]
      proxy %>% addRasterImage(values$predCur, layerId='ras', colors="Spectral")
    } 
  })

  observe({
    if ((input$tabs == "5) Predict" | input$tabs == "ABOUT") & !is.null(values$predCur)) {
      proxy %>% addRasterImage(values$predCur, layerId='ras', colors="Spectral") 
    } else {
      proxy %>% removeImage(layerId='ras')
    }
  })
  
  # handle download for rasters, as TIFF
  output$downloadPred <- downloadHandler(
    filename = function() {paste0(names(values$evalPreds[[as.numeric(input$predSelServer)]]), "_pred.tif")},
    content = function(file) {
      res <- writeRaster(values$evalPreds[[as.numeric(input$predSelServer)]], file, format = "GTiff", overwrite = TRUE)
      file.rename(res@file@name, file)
    }
  )
})
