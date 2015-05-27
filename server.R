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
install_github("bobmuscarella/ENMeval@edits")

# load libraries
library(colorRamps)
library(shiny)
library(rgbif)
library(spThin)
library(ENMeval)
library(dismo)
library(rgeos)
library(ggplot2)
library(leaflet)


## -------------------------------------------------------------------- ##
## Define functions
## -------------------------------------------------------------------- ##

# for naming files
nameAbbr <- function(spname) {
  namespl <- strsplit(tolower(spname[1,1]), " ")
  genusAbbr <- substring(namespl[[1]][1], 1, 1)
  fullNameAbbr <- paste0(genusAbbr, "_", namespl[[1]][2])
  return(fullNameAbbr)
}

# make a minimum convex polygon as SpatialPolygons object
mcp <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1))))
}

# this is currently only used for the raster mapping, but was originally used for all maps
# before we implemented leaflet (the version of leaflet we used did not have raster plotting
# functionality)-- we plan to recode using the new leaflet version (with new syntax) as a next 
# step, whereupon a leaflet map will be used for plotting rasters too
plotMap <- function(pts=NULL, poly=NULL, pred=NULL, addpo=NULL, pts2=NULL) {
  mapWorld <- borders('world', colour = 'white', fill = 'white')
  if (!is.null(pts)) {
    xl <- c(min(pts$lon) - 5, max(pts$lon) + 5)
    yl <- c(min(pts$lat) - 5, max(pts$lat) + 5)
    mp <- ggplot(pts, aes(x = lon, y = lat)) + 
      mapWorld + theme(panel.background = element_rect(fill = 'lightblue')) +
      geom_point(size = 3, colour = 'blue', position = 'jitter', shape = 1) +
      coord_cartesian(xlim = xl, ylim = yl) + 
      geom_text(label = row.names(pts), hjust = 1, vjust = -1)
    if (!is.null(poly)) {
      mp <- mp + geom_path(aes(x = long, y = lat), fortify(poly), colour = 'red')
    }
    print(mp)
  }
  if (!is.null(pred)) {
    # Convert raster to points
    pred.p <- rasterToPoints(pred)
    # Make the points a dataframe for ggplot
    pred.df <- data.frame(pred.p)
    #Make appropriate column headings
    colnames(pred.df) <- c("lon", "lat", "val")
    e <- extent(pred)
    if(!addpo){
    mp <- ggplot(data = pred.df, aes(x = lon, y = lat)) + 
      mapWorld + theme(panel.background = element_rect(fill = 'lightblue')) + 
      geom_raster(aes(fill = val)) + 
      coord_equal() +
      coord_cartesian(xlim =c(e[1] - 2, e[2] + 2), ylim =c(e[3] - 2, e[4] + 2)) +
      #scale_fill_gradient("relative suitability (raw output)", limits = c(pred@data@min, pred@data@max), low = 'grey', high = 'blue')
      scale_fill_gradientn("relative suitability (raw output)", limits = c(pred@data@min, pred@data@max), colours=matlab.like2(50))
    }else{
      mp <- ggplot(data = pred.df, aes(x = lon, y = lat)) + 
        mapWorld + theme(panel.background = element_rect(fill = 'lightblue')) + 
        geom_raster(aes(fill = val)) + 
        coord_equal() +
        coord_cartesian(xlim =c(e[1] - 2, e[2] + 2), ylim =c(e[3] - 2, e[4] + 2)) +
        #scale_fill_gradient("relative suitability (raw output)", limits = c(pred@data@min, pred@data@max), low = 'grey', high = 'blue')
        scale_fill_gradientn("relative suitability (raw output)", limits = c(pred@data@min, pred@data@max), colours=matlab.like2(50))
      mp <- mp + geom_point(data=pts2, aes(x = lon, y = lat), size = 3, colour = 'red',
                            position = 'jitter', shape = 1)  
    }
    print(mp)
  }
}

## -------------------------------------------------------------------- ##
## Main shinyServer code
## -------------------------------------------------------------------- ##

shinyServer(function(input, output, session) {
  
  # this list carries data that is used by multiple reactive functions
  values <- reactiveValues()
  
  # query GBIF based on user input, remove duplicate records
  GBIFsearch <- reactive({
    if (input$gbifName == '') return(NULL)
    input$goName
    isolate({
      withProgress(message = "Searching GBIF...", {
        results <- occ_search(scientificName = input$gbifName, limit = input$occurrences, 
                              fields = c('name', 'decimalLongitude', 'decimalLatitude', 'basisOfRecord'), 
                              hasCoordinate = TRUE)
        if (results$meta$count != 0) {
          locs <- results$data[!is.na(results$data[,3]),][,c(1,3,4,2)]
          dup <- duplicated(locs)
          locs <- locs[!dup, ]
          names(locs)[2:3] <- c('lon', 'lat')
          values$df <- locs
          values$gbifoccs <- locs
          return(c(nrow(locs), results$meta$count, sum(dup)))
        }
      })
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
  
  # render the GBIF records data table
  output$gbifOccTbl <- renderTable({
    if (input$goName == 0) return()
    input$goName
    input$remove
    isolate({
      out <- GBIFsearch()
      if (is.null(out)) {
        NULL
      } else {
        values$gbifoccs
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
      
  # some error handling and text output for GBIF record downloads
  output$GBIFtxt <- renderText({
    if (input$goName == 0) return()
    isolate({
    out <- GBIFsearch()
    inName <- isolate(input$gbifName)
    nameSplit <- length(unlist(strsplit(inName, " ")))
    
    if (nameSplit == 1 && !is.null(out)) {
      paste("Please input both genus and species names. More than one species with this genus was found.")
    } else {
      if (nameSplit == 1 && is.null(out)) {
        paste("Please input both genus and species names.")      
      } else {
        if (nameSplit != 1 && is.null(out)) {
          paste0('No records found for ', inName, ". Please check the spelling.")
        } else {
          if (nameSplit != 1 && !is.null(out)) {
              paste('Total records for', values$gbifoccs[1,1], 'returned [', out[1],
                    '] out of [', out[2], '] total (limit 500).
                    Duplicated records removed [', out[3], "].")
          }
        }
      }
    }
    })
  })
  
  # map of GBIF records
  map <- createLeafletMap(session, 'map')
  observe({
    if (input$goName == 0) return() 
    input$goName
    input$remove
    isolate({
      map$clearShapes()
      lati <- values$gbifoccs[, 3]
      longi <- values$gbifoccs[, 2]
      map$fitBounds(max(lati), max(longi), min(lati), min(longi))
      map$addCircle(lati, longi, layerId=as.numeric(rownames(values$gbifoccs)), 
                    radius=8, options=list(weight=8, fill=FALSE, color='red'))
    })
  })

  observe({
    map$clearPopups()    
    event <- input$map_shape_click
    if (is.null(event)){return()}
    isolate({
      content <- as.character(tagList(
        tags$strong(paste("ID:", event$id)),
        tags$br(),
        tags$strong(paste("Latitude:", event$lat)),        
        tags$strong(paste("Longitude:", event$lng))                    
        ))
      map$showPopup(event$lat, event$lng, content)
    })
  })

  # run spThin and return one of the optimal solutions as data.frame
  runThin <- reactive({
    input$goThin
    isolate({
      withProgress(message = "Thinning...", {
        output <- thin(values$gbifoccs, 'lat', 'lon', 'name', thin.par = input$thinDist, 
                       reps = 10, locs.thinned.list.return = TRUE, write.files = FALSE,
                       verbose = FALSE)
        # pull max, not first
        thinout <- cbind(rep(values$gbifoccs[1,1], nrow(output[[1]])), output[[1]])
        names(thinout) <- c('name', 'lon', 'lat')
        values$df <- thinout
        values$thinoccs <- thinout
      })
    })
  })
  
  # render thinned records table
  output$thinOccTbl <- renderTable({values$thinoccs})
  
  # map for thinned records
  map2 <- createLeafletMap(session, 'map2')
  observe({
    if (is.null(values$thinoccs)) return()
    input$goThin
    isolate({
    map2$clearShapes()
    lati2 <- values$thinoccs[, 3]
    longi2 <- values$thinoccs[, 2]
    map2$fitBounds(max(lati2), max(longi2), min(lati2), min(longi2))
    map2$addCircle(lati2, longi2, layerId=as.numeric(rownames(values$thinoccs)),
                   radius=8, options=list(weight=8, fill=FALSE, color='red'))
    })
  })
  
  observe({
    map2$clearPopups()    
    event <- input$map2_shape_click
    if (is.null(event)){return()}
    isolate({
      content <- as.character(tagList(
        tags$strong(paste("ID:", event$id)),
        tags$br(),
        tags$strong(paste("Latitude:", event$lat)),        
        tags$strong(paste("Longitude:", event$lng))                    
      ))
      map2$showPopup(event$lat, event$lng, content)
    })
  })

  # text output for thinned records
  output$thinText <- renderText({
    if (input$goThin == 0) return()
    input$goThin
    runThin()
    paste('Total records thinned to [', nrow(values$df), '] points.')
  })
  
  # handle download for thinned records csv
  output$downloadThincsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_thinned_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$thinoccs, file)
    }
  )
  
  # text output for raster selection and removal of NA records
  output$predTxt1 <- renderUI({
    ## Check if predictor path exists. If not, use the dismo function getData()
    if (input$pred == "" || input$pred == 'user') return()
    isolate({
      withProgress(message = "Downloading WorldClim data...", {
        values$pred <- getData(name = "worldclim", var = "bio", res = input$pred)
      })
      withProgress(message = "Checking environmental values...", {
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
      HTML(paste(str1, str2, sep = '<br/>'))
    })
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
  
  # make a study region extent via user selection
  makeBackgExt <- reactive({
    if (input$backg == "" || input$pred == "") return()
    if (input$backg == 'bb') {
      xmin <- min(values$df$lon) - (input$backgBuf + res(values$pred)[1])
      xmax <- max(values$df$lon) + (input$backgBuf + res(values$pred)[1])
      ymin <- min(values$df$lat) - (input$backgBuf + res(values$pred)[1])
      ymax <- max(values$df$lat) + (input$backgBuf + res(values$pred)[1])
      bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)
      values$backgExt <- SpatialPolygons(list(Polygons(list(Polygon(bb)), 1)))
      values$backgExt2Map <- bb
    } else if (input$backg == 'mcp') {
      xy_mcp <- mcp(values$df[,2:3])
      xy_mcp <- gBuffer(xy_mcp, width = input$backgBuf + res(values$pred)[1])
      values$backgExt <- xy_mcp
      values$backgExt2Map <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords
    }
  })
  
  # map for study region
  map3 <- createLeafletMap(session, 'map3')
  observe({
    if (input$backg == "") return()
    input$backg
    valores <- values$df[, 2:3]
    makeBackgExt()
    back <- values$backgExt2Map
    isolate({
      map3$clearShapes()      
      lati3 <- valores[, 2]
      longi3 <- valores[, 1]
      map3$fitBounds(max(lati3), max(longi3), min(lati3), min(longi3))
      map3$addCircle(    
        lati3,
        longi3,
        layerId=as.numeric(rownames(valores)),
        radius=8,
        options=list(
          weight=8,
          fill=FALSE,
          color='red'))
        map3$addPolygon(lng=back[, 1], lat=back[, 2], layerId="1",
                     options= list(weight=10, col="red"))
      
    })
  })
  
  observe({
    map3$clearPopups()    
    event <- input$map3_shape_click
    if (is.null(event)){return()}
    isolate({
      content <- as.character(tagList(
        tags$strong(paste("ID:", event$id)),
        tags$br(),
        tags$strong(paste("Latitude:", event$lat)),        
        tags$strong(paste("Longitude:", event$lng))                    
      ))
      map3$showPopup(event$lat, event$lng, content)
    })
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
    })
  })
  
  # render table of ENMeval results
  output$evalTbl <- renderTable({values$evalTbl})
  
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
