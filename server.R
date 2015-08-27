# check package dependencies, and download if necessary
list.of.packages <- c("shiny", "leaflet", "ggplot2", "maps", "RColorBrewer", "rgdal", 
                      "spThin", "colorRamps", "dismo", "rgeos", "XML", "repmis", "Rcpp", "RCurl", "curl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# rgbif needs to be downloaded from source
if (!require('rgbif')) install.packages('rgbif', type='source')
# use devtools to install leaflet and new unreleased version of ENMeval from github
if (!require('devtools')) install.packages('devtools')
if (!require('leaflet')) devtools::install_github('rstudio/leaflet')
# for exp version of ENMeval with special updateProgress param for shiny
#install_github("bobmuscarella/ENMeval@edits")
if (!require("DT")) devtools::install_github("rstudio/DT")

# load libraries
library(devtools)
library(shiny)
library(rgbif)
library(maptools)
library(spThin)
library(ENMeval)
library(dismo)
library(rgeos)
library(ggplot2)
# library(shinyFiles)
library(RColorBrewer)
library(leaflet)
library(repmis)

source("functions.R")

#devtools::install_github("jcheng5/rasterfaster")

shinyServer(function(input, output, session) {
  # make list to carry data used by multiple reactive functions
  values <- reactiveValues(polyID=0, polyErase=FALSE, log=c())
  
  output$log <- renderUI({tags$div(id='logHeader',
                                   tags$div(id='logContent', HTML(paste0(values$log, "<br>", collapse = ""))))})
  # add text to log
  writeLog <- function(x) {
    values$log <- paste(values$log, x, sep='<br>')
  }
  
  # create map
  map <- leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2)
  output$map <- renderLeaflet(map)
  
  # make map proxy to make further changes to existing map
  proxy <- leafletProxy("map")
  
  # query GBIF based on user input, remove duplicate records
  observeEvent(input$goName, {
    writeLog("...Searching GBIF...")
    results <- occ_search(scientificName = input$gbifName, limit = input$occurrences, 
                          fields = c('name', 'decimalLongitude', 'decimalLatitude', 'basisOfRecord'), 
                          hasCoordinate = TRUE)
    # Control species not found
    if (results$meta$count == 0) {
      writeLog(paste('* No records found for ', input$gbifName, ". Please check the spelling."))
    }
                      
    if (results$meta$count != 0) {
      locs.in <- results$data[!is.na(results$data[,3]),][,c(1,3,4,2)]
      locs <- remDups(locs.in)
      names(locs)[2:3] <- c('lon', 'lat')
      
      locs$origID <- row.names(locs)
      locs$pop <- unlist(apply(locs, 1, popUpContent))
      
      values$gbifoccs <- rbind(values$gbifoccs, locs)
      values$gbifoccs <- remDups(values$gbifoccs)
      values$df <- values$gbifoccs
      
      inName <- isolate(input$gbifName)
      nameSplit <- length(unlist(strsplit(inName, " ")))
      
      if (nameSplit == 1 && !is.null(locs)) {
        x <- paste("* Please input both genus and species names. More than one species with this genus was found.")
      } else {if (nameSplit == 1 && is.null(locs)) {
        x <- paste("* Please input both genus and species names.")      
      } else {if (nameSplit != 1 && is.null(locs)) {
        x <- paste0('* No records found for ', inName, ". Please check the spelling.")
      } else {if (nameSplit != 1 && !is.null(locs)) {
        x <- paste('* Total GBIF records for', values$gbifoccs[1,1], 'returned [', nrow(locs.in),
                   '] out of [', results$meta$count, '] total (limit 500).
                    Duplicated records removed [', nrow(locs.in) - nrow(locs), "]: Remaining records [", nrow(locs), "].")
      }}}}
      writeLog(x)
      }
  })
  
  observe({
    if (is.null(values$df)) return()
    # render the GBIF records data table
    output$occTbl <- DT::renderDataTable({DT::datatable(values$df[,1:4])})
  })
  
  # functionality for input of user CSV
  observe({    
    if (is.null(input$userCSV)) return()
    inFile <- read.csv(input$userCSV$datapath, header = TRUE)
    names(inFile)[2:3] <- c('lon', 'lat')
    values$inFile <- inFile
    # make dynamic field selections for ui user-defined kfold groups
    output$occgrpSel <- renderUI({
      selectInput('occgrp', 'Occurrence Group Field', names(inFile))
    })
    output$bggrpSel <- renderUI({
      selectInput('bggrp', 'Background Group Field', names(inFile))
    })
    # subset to only occs, not backg, and just fields that match df
    values$spname <- inFile[1,1]
    inFile.occs <- inFile[inFile[,1] == values$spname,]
    inFile.occs <- inFile.occs[,c('name', 'lon', 'lat')]
    if (!("basisOfRecord" %in% names(inFile.occs))) {
      inFile.occs$basisOfRecord <- NA
    }
    inFile.occs$origID <- row.names(inFile.occs)
    inFile.occs$pop <- unlist(apply(inFile.occs, 1, popUpContent))
    values$inFileOccs
    # bind new csv occs to existing ones in df and gbifoccs (without duplicates)
    addCSVpts <- function(df) {
      df <- rbind(df, inFile.occs)
      df <- remDups(df)
    }
    values$gbifoccs <- isolate(addCSVpts(values$gbifoccs))
    values$df <- isolate(addCSVpts(values$df))
    # this makes an infinite loop. not sure why...
    #     x <- paste0("User input ", input$userCSV$name, " with [", nrow(values$df), "[ records.")
    #     values$log <- paste(values$log, x, sep='<br>')
  })
  
  # map gbif occs
  observeEvent(input$goName, {
    if (is.null(values$gbifoccs)) {return()}
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
                               layerId = as.numeric(values$gbifoccs$origID), 
                               radius = 5, color = 'red', fill = FALSE, weight = 2,
                               popup = ~pop)
  })
  
  # functionality for drawing polygons on map
  observe({
    if (input$tabs == 1) {
      latlng <- c(input$map_click$lng, input$map_click$lat)
      # this functionality prevents existing map click from being added to new polygon
      if (values$polyErase) {
        if (identical(latlng, values$mapClick)) {
          return()
        } else {
          values$polyErase <- FALSE 
        }
      }
      if (is.null(input$map_click)) return()
      values$mapClick <- latlng
      values$drawPolyCoords <- isolate(rbind(values$drawPolyCoords, latlng))
      proxy %>% removeShape("drawPoly")
      proxy %>% addPolygons(values$drawPolyCoords[,1], values$drawPolyCoords[,2], 
                            layerId='drawPoly', fill=FALSE, weight=3, color='green')
    }
  })
  
  # erase poly with button click
  observeEvent(input$erasePoly, {
    proxy %>% clearShapes()
    proxy %>% clearMarkers()
    values$drawPolyCoords <- NULL
    values$drawPolys <- NULL
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    x <- paste('* Erased polygons, dataset is now back to', nrow(values$gbifoccs), 'records.')
    isolate(writeLog(x))
    if (!is.null(values$gbifoccs)) {
      values$df <- values$gbifoccs
      proxy %>% addCircleMarkers(data = values$df, lat = ~lat, lng = ~lon, 
                                 layerId = as.numeric(values$df$origID), 
                                 radius = 5, color = 'red', fill = FALSE, weight = 2,
                                 popup = ~pop)
    }
  })
  
  # select points intersecting drawn polygons (replace values$df)
  observeEvent(input$selectPoly, {
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    values$polyID <- values$polyID + 1
    if (is.null(values$gbifoccs)) return()
    if (is.null(values$drawPolyCoords)) return()
    pts <- SpatialPoints(values$gbifoccs[,2:3])
    newPoly <- SpatialPolygons(list(Polygons(list(Polygon(values$drawPolyCoords)), ID=values$polyID)))
    if (is.null(values$drawPolys)) {
      values$drawPolys <- newPoly
    } else {
      values$drawPolys <- spRbind(values$drawPolys, newPoly)
    }
    
    ptsSel <- values$gbifoccs[!(is.na(over(pts, values$drawPolys))),]
    proxy %>% addCircleMarkers(data = ptsSel, lat = ~lat, lng = ~lon, 
                               layerId = as.numeric(ptsSel$origID), 
                               radius = 5, color = 'red', fill = TRUE, fillColor = 'yellow', 
                               weight = 2, popup = ~pop, fillOpacity=1)
    values$drawPolyCoords <- NULL
    values$ptsSel <- ptsSel
    values$df <- ptsSel
    isolate(writeLog(paste('* Selected', nrow(values$df), 'points.')))
  })
  
  # behavior for plotting points and their colors based on which tab is active
  observe({
    if (is.null(values$df)) return()
    
    if (input$tabs == 1) {
      proxy %>% clearMarkers()
      proxy %>% clearShapes()
      proxy %>% clearImages()
      proxy %>% addCircleMarkers(data = values$gbifoccs, lat = ~lat, lng = ~lon, 
                                 radius = 5, color = 'red', 
                                 fill = TRUE, fillColor = 'red', weight = 2, popup = ~pop)
      # draw all user-drawn polygons and color according to colorBrewer
      if (!is.null(values$drawPolys)) {
        curPolys <- values$drawPolys@polygons
        numPolys <- length(curPolys)
        colors <- brewer.pal(numPolys, 'Accent')
        for (i in numPolys) {
          curPoly <- curPolys[i][[1]]@Polygons[[1]]@coords
          proxy %>% addPolygons(curPoly[,1], curPoly[,2], 
                                group = 'tab1', weight=3, color=colors[i])   
        }        
      }

      if (!is.null(values$ptsSel)) {
        proxy %>% addCircleMarkers(data = values$ptsSel, lat = ~lat, lng = ~lon, 
                                   radius = 5, color = 'red', 
                                   fill = TRUE, fillColor = 'yellow', 
                                   weight = 2, popup = ~pop, fillOpacity=1)
        proxy %>% addLegend("bottomright", colors = c('red','yellow'), 
                  title = "GBIF Records", labels = c('original', 'selected'),
                  opacity = 1, layerId = 1)
      }
    }

    if (input$tabs == 2) {
      proxy %>% clearMarkers()
      proxy %>% clearShapes()
      proxy %>% clearImages()
      if (!is.null(values$prethinned)) {
        lati <- values$prethinned[,3]
        longi <- values$prethinned[,2]
        proxy %>% fitBounds(min(longi), min(lati), max(longi), max(lati))
        proxy %>% addCircleMarkers(data = values$prethinned, lat = ~lat, lng = ~lon, 
                                   radius = 5, color = 'red', 
                                   fill = TRUE, fillColor = 'red', weight = 2, popup = ~pop)
        proxy %>% addCircleMarkers(data = values$df, lat = ~lat, lng = ~lon, 
                                   radius = 5, color = 'red', 
                                   fill = TRUE, fillColor = 'blue',
                                   fillOpacity = 0.8, weight = 2, popup = ~pop)
        proxy %>% addLegend("bottomright", colors = c('red', 'blue'), 
                            title = "GBIF Records", labels = c('thinned', 'current'),
                            opacity = 1, layerId = 1)
      } else {
        proxy %>% addCircleMarkers(data = values$df, lat = ~lat, lng = ~lon, 
                                   radius = 5, color = 'red', 
                                   fill = TRUE, fillColor = 'red', weight = 2, popup = ~pop)
        proxy %>% addLegend("bottomright", colors = c('red'), 
                            title = "GBIF Records", labels = c('current'),
                            opacity = 1, layerId = 1)
      }
    }
    
    if (input$tabs == 3 | input$tabs == 4 | input$tabs == 5) {
      proxy %>% clearMarkers()
      proxy %>% clearShapes()
      proxy %>% clearImages()
      proxy %>% addCircleMarkers(data = values$df, lat = ~lat, lng = ~lon, 
                                 radius = 5, color = 'red', 
                                 fill = TRUE, fillColor = 'red', weight = 2, popup = ~pop)
      proxy %>% addLegend("bottomright", colors = c('red'), 
                          title = "GBIF Records", labels = c('current'),
                          opacity = 1, layerId = 1)
      if (!is.null(values$bb) & input$tabs == 4) {
        proxy %>% addPolygons(lng=values$bb[,1], lat=values$bb[,2], layerId="backext",
                              options= list(weight=10, col="red"))  
      }
    }
    
    # erase raster if user goes to other tabs, puts it back when return to tab 5
    if (input$tabs == 7) {
      proxy %>% clearMarkers()
      proxy %>% clearShapes()
      proxy %>% clearImages()
      proxy %>% addCircleMarkers(data = values$df, lat = ~lat, lng = ~lon, 
                                 radius = 5, color = 'black', 
                                 fill = TRUE, weight = 4, popup = ~pop)
      proxy %>% addLegend("bottomright", colors = c('black'), 
                          title = "GBIF Records", labels = c('current'),
                          opacity = 1, layerId = 1)
      
      if (!is.null(values$predCur)) {
        if (input$predThresh == 'mtp' | input$predThresh == 'p10') {
          pal <- c('gray', 'blue')
          proxy %>% addLegend("bottomright", colors = pal, 
                              title = "Thresholded Suitability", labels = c(0, 1),
                              opacity = 1, layerId = 1)
        } else {
          r <- values(values$predCur)
          pal <- colorNumeric(c('yellow', 'green', 'blue'), r, na.color='transparent')  
          proxy %>% addLegend("bottomright", pal = pal, title = "Predicted Suitability", 
                              values = r, layerId = 1)
        }
        proxy %>% addRasterImage(values$predCur, colors = pal, opacity = 0.8)
      }
    }
  })
  
  # handle downloading of GBIF csv
  output$downloadGBIFcsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$df), "_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$df[,1:5], file, row.names=FALSE)
    }
  )
  
  # map thinned records when Thin button is pressed
  observeEvent(input$goThin, {
      if (is.null(values$df)) {
      writeLog("* Obtain the species occurrence record first")
      return()
      }
    withProgress(message = "Spatially Thinning Records...", {
      output <- thin(values$df, 'lat', 'lon', 'name', thin.par = input$thinDist, 
                     reps = 10, locs.thinned.list.return = TRUE, write.files = FALSE,
                     verbose = FALSE)
    
      values$prethinned <- values$df
      # pull thinned dataset with max records, not just the first in the list
      maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
      maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]  # if more than one max, pick first
      values$df <- values$df[as.numeric(rownames(maxThin)),]
      
      if (!is.null(values$inFile)) {
        thinned.inFile <- values$inFile[as.numeric(rownames(output[[1]])),] 
      }
    })
    writeLog(paste('* Total records thinned to [', nrow(values$df), '] points.'))
    # render the thinned records data table
    output$occTbl <- DT::renderDataTable({DT::datatable(values$df[,1:4])})
  })
  
  # handle download for thinned records csv
  output$downloadThincsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_thinned_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$df, file)
    }
  )
  
  # download predictor variables
  observe({
    ## Check if predictor path exists. If not, use the dismo function getData()
    if (input$pred == "" || input$pred == 'user') return()
    if (!is.null(values$df)) {
      ## Check if predictor path exists. If not, use the dismo function getData()
      withProgress(message = "Downloading WorldClim data...", {
        values$pred <- getData(name = "worldclim", var = "bio", res = input$pred)
      })
      proxy %>% addLegend("topright", colors = c(), 
                          title = "Predictors: Worldclim bio 1-19", labels = c(),
                          opacity = 1, layerId = 2)
      isolate(writeLog(paste("* Environmental predictors: WorldClim bio1-19 at", input$pred, " arcmin resolution.")))
      withProgress(message = "Processing...", {
        locs.vals <- extract(values$pred[[1]], values$df[,2:3])
        if (sum(is.na(locs.vals)) > 0) {
          isolate(writeLog(paste0("* Removed records with NA environmental values with IDs: ", 
                       paste(row.names(values$df[is.na(locs.vals),]), collapse=', '), ".")))
        }
        values$df <- values$df[!is.na(locs.vals),]
        if (!is.null(values$inFile)) {
          values$inFile <- values$inFile[!is.na(locs.vals),]  
        }
      })
    }
  })
  
  # functionality for downloading .asc files from dropbox
  observeEvent(input$dbAscGet, {
               dbAsc <- source_DropboxData(input$dbAscFname, input$dbAscKey)
               dims <- strsplit(input$dbAscDims, split=',')[[1]]
               dbRas <- raster(dbAsc, crs=input$dbAscCRS, xmn=dims[1], xmx=dims[2],
                               ymn=dims[3], ymx=dims[4])
  })
  
  # this is necessary because the above is not observeEvent, and thus for some
  # reason when values$log is modified within observe, there's an infinite loop
  observe({
    if (!is.null(values$predTxt)) {
      writeLog(values$predTxt)
      values$predTxt <- NULL
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
  
  # background extents
  observe({
    if (is.null(input$backgSelect) | is.null(values$pred)) return()
    # generate background extent
    if (input$backgSelect == 'bb') {
      xmin <- min(values$df$lon) - (input$backgBuf + res(values$pred)[1])
      xmax <- max(values$df$lon) + (input$backgBuf + res(values$pred)[1])
      ymin <- min(values$df$lat) - (input$backgBuf + res(values$pred)[1])
      ymax <- max(values$df$lat) + (input$backgBuf + res(values$pred)[1])
      bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)
      values$backgExt <- SpatialPolygons(list(Polygons(list(Polygon(bb)), 1)))
      bbTxt <- 'bounding box'
    } else if (input$backgSelect == 'mcp') {
      xy_mcp <- mcp(values$df[,2:3])
      xy_mcp <- gBuffer(xy_mcp, width = input$backgBuf + res(values$pred)[1])
      values$backgExt <- xy_mcp
      bb <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords
      bbTxt <- 'minimum convex polygon'
    } else if (input$backgSelect == 'user') {
      if (is.null(input$userBackg)) return()
#       file <- shinyFileChoose(input, 'userBackg', root=c(root='.'))
#       path <- input$userBackg$datapath
      names <- input$userBackg$name
      inPath <- input$userBackg$datapath
      pathdir <- dirname(inPath)
      pathfile <- basename(inPath)
      # get extensions of all input files
      exts <- sapply(strsplit(names, '\\.'), FUN=function(x) x[2])
      if (length(exts) == 1 & exts == 'csv') {
        shp <- read.csv(inPath, header = TRUE)
        shp <- SpatialPolygons(list(Polygons(list(Polygon(bb)), 1)))
        shp <- gBuffer(shp, width = input$backgBuf + res(values$pred)[1])
        values$backgExt <- shp
        bb <- shp@polygons[[1]]@Polygons[[1]]@coords
      } else if (length(exts) > 1 & 'shp' %in% exts) {
        # rename temp files to their original names - nice hack for inputting shapefiles in shiny
        file.rename(inPath, file.path(pathdir, names))
        # get index of .shp
        i <- which(exts == 'shp')
        # read in shapefile and extract coords
        poly <- readOGR(pathdir[i], strsplit(names[i], '\\.')[[1]][1])
        poly <- gBuffer(poly, width = input$backgBuf + res(values$pred)[1])
        values$backgExt <- poly
        bb <- poly@polygons[[1]]@Polygons[[1]]@coords
      }
      bbTxt <- 'user-defined'
    }
    isolate(writeLog(paste0("* Background extent: ", bbTxt, ".")))
    isolate(writeLog(paste('* Background extent buffered by', input$backgBuf, 'degrees.')))
    
    values$bb <- bb
    proxy %>% fitBounds(max(bb[,1]), max(bb[,2]), min(bb[,1]), min(bb[,2]))
    proxy %>% addPolygons(lng=bb[,1], lat=bb[,2], layerId="backext",
                          options= list(weight=10, col="red"))
    
    # clip and mask rasters based on study region
    withProgress(message = "Processing environmental rasters...", {
      predCrop <- crop(values$pred, values$backgExt)
      values$predMsk <- mask(predCrop, values$backgExt)
    })
    isolate(writeLog(paste0('* Environmental rasters masked by ', bbTxt, '.')))
  })
  
  observeEvent(input$goPart, {
    if (is.null(input$partSelect)) return()
    # if user kfold, get groups and assign occs and backg from inFile, 
    # and if not, make backg pts and assign user kfold groups to NULL
    if (input$partSelect == 'user') {
      occs <- values$inFile[values$inFile[,1] == values$spname,]
      bg.coords <- values$inFile[values$inFile[,1] != values$spname,]
      group.data <- list()
      group.data[[1]] <- as.numeric(occs[,input$occ.grp])
      group.data[[2]] <- as.numeric(backg_pts[,input$bg.grp])
      occs <- occs[,2:3]
      values$bg.coords <- backg_pts[,2:3]
    } else {
      occs <- values$df[,2:3]
      if (is.null(values$bg.coords)) {
        withProgress(message = "Generating background points...", {
          bg.coords <- randomPoints(values$predMsk, 10000)
          values$bg.coords <- as.data.frame(bg.coords)        
        })
      }
    }
    
    if (input$partSelect == 'block') {group.data <- get.block(occs, values$bg.coords)}
    if (input$partSelect == 'cb1') {group.data <- get.checkerboard1(occs, values$predMsk, values$bg.coords, input$aggFact)}
    if (input$partSelect == 'cb2') {group.data <- get.checkerboard2(occs, values$predMsk, values$bg.coords, input$aggFact)}
    if (input$partSelect == 'jack') {group.data <- get.jackknife(occs, values$bg.coords)}
    if (input$partSelect == 'random') {group.data <- get.randomkfold(occs, values$bg.coords, input$kfolds)}
    values$modParams <- list(occ.pts=occs, bg.pts=values$bg.coords, occ.grp=group.data[[1]], bg.grp=group.data[[2]])
    
    #newColors <- brewer.pal(max(group.data[[1]]), 'Accent')
    newColors <- palette(rainbow(max(group.data[[1]])))
    print(newColors)
    proxy %>% addCircleMarkers(data = values$df, lat = ~lat, lng = ~lon, 
                               radius = 5, color = 'red', fill = TRUE, 
                               fillColor = newColors, weight = 2, popup = ~pop, fillOpacity = 1)
  })
  
  # run ENMeval via user inputs
  observeEvent(input$goEval, {
    
    if (input$modSelect == "Maxent") {
      rms <- seq(input$rms[1], input$rms[2], input$rmsBy)
      progress <- shiny::Progress$new()
      progress$set(message = "Evaluating ENMs...", value = 0)
      on.exit(progress$close())
      n <- length(rms) * length(input$fcs)
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$inc(amount = 1/n, detail = detail)
      }
      
      e <- ENMevaluate(values$modParams$occ.pts, values$predMsk, bg.coords = values$modParams$bg.pts, 
                       RMvalues = rms, fc = input$fcs, method = 'user', occ.grp = values$modParams$occ.grp, 
                       bg.grp = values$modParams$bg.grp, updateProgress = updateProgress)
      values$eval <- e  
    }
    
    
    # get mtp and p10 for all models (as output is raw we can't get this from the model results table)
    # this code is mostly pulled with modifications from ENMeval -- thanks Bob!
    occVals <- extract(values$eval@predictions, occs)
    values$mtps <- apply(occVals, MARGIN = 2, min)
    if (nrow(occVals) < 10) {
      n90 <- floor(nrow(occVals) * 0.9)
    } else {
      n90 <- ceiling(nrow(occVals) * 0.9)
    }
    values$p10s <- apply(occVals, MARGIN = 2, function(x) rev(sort(x))[n90])
    
    # render table of ENMeval results
    # code to do fixed columns -- problem is it makes the page selection disappear and you
    # can't seem to pan around the table to see the other rows... likely a bug
    # (extensions=list(FixedColumns=list(leftColumns=2)), options=list(dom='t', scrollX=TRUE, scrollCollapse=TRUE))
    output$evalTbl <- DT::renderDataTable({DT::datatable(cbind(e@results[,1:3], round(e@results[,4:15], digits=3)))})
    
    if (!is.null(values$eval@results)) {
      writeLog(paste("* ENMeval ran successfully and output results for", nrow(values$eval@results), "models."))
      
      # plotting functionality for ENMeval graphs
      output$evalPlot <- renderPlot({
        par(mfrow=c(3,2))
        fc <- length(unique(values$eval@results$features))
        col <- rainbow(fc)
        rm <- length(unique(values$eval@results$rm))
        plot(rep(1, times=fc), 1:fc, ylim=c(.5,fc+1), xlim=c(0,3), axes=F, ylab='', xlab='', cex=2, pch=21, bg=col)
        segments(rep(.8, times=fc), 1:fc, rep(1.2, times=fc), 1:fc, lwd=1, col=col)
        points(rep(1, times=fc), 1:fc, ylim=c(-1,fc+2), cex=2, pch=21, bg=col)
        text(x=rep(1.3, times=fc), y=1:fc, labels=unique(values$eval@results$features), adj=0)
        text(x=1, y=fc+1, labels="Feature Classes", adj=.20, cex=1.3, font=2)
        eval.plot(values$eval@results, legend=FALSE, value="delta.AICc")
        eval.plot(values$eval@results, legend=FALSE, value="Mean.AUC", variance="Var.AUC")
        eval.plot(values$eval@results, legend=FALSE, value="Mean.AUC.DIFF", variance="Var.AUC.DIFF")
        eval.plot(values$eval@results, legend=FALSE, value="Mean.ORmin", variance="Var.ORmin")
        eval.plot(values$eval@results, legend=FALSE, value="Mean.OR10", variance="Var.OR10")
      })
    }
    # a tabset within tab 4 to organize the enmEval outputs
    output$evalTabs <- renderUI({
      tabsetPanel(tabPanel("Results Table", DT::dataTableOutput('evalTbl')),
                  tabPanel("Evaluation Graphs", plotOutput('evalPlot', width = 600))
      )
    })
  })
  
  # handle downloads for ENMeval results table csv
  output$downloadEvalcsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_enmeval_results.csv")},
    content = function(file) {
      write.csv(values$eval@results, file)
    }
  )
  
  # generates user selection of rasters to plot dynamically after they are created
  output$predSel <- renderUI({
    if (is.null(values$eval)) return()
    n <- names(values$eval@predictions)
    predNameList <- setNames(as.list(seq(1, length(n))), n)
    values$rdyPlot <- 'rdy'
    selectInput("predSelServer", label = "Choose a model",
                choices = predNameList)
  })
  
  # set predCur based on user selection of threshold
  observeEvent(input$plotPred, {
    selRas <- values$eval@predictions[[as.numeric(input$predSelServer)]]
    values$rasName <- names(selRas)
    if (input$predThresh == 'raw') {
      values$predCur <- selRas
    } else if (input$predThresh == 'mtp') {
      mtp <- values$mtps[as.numeric(input$predSelServer)]
      values$predCur <- selRas > mtp
    } else if (input$predThresh == 'p10') {
      p10 <- values$p10s[as.numeric(input$predSelServer)]
      values$predCur <- selRas > p10
    }
  })
  
  # handle download for rasters, as TIFF
  output$downloadPred <- downloadHandler(
    filename = function() {paste0(values$rasName, "_", input$predThresh, "_pred.tif")},
    content = function(file) {
      res <- writeRaster(values$predCur, file, format = input$predFileType, overwrite = TRUE)
      file.rename(res@file@name, file)
    }
  )
})
