list.of.packages <- c("shiny", "ggplot2", "maps", "rgbif", "spThin", "ENMeval")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
if (!("leaflet" %in% installed.packages()[,"Package"])) devtools::install_github("rstudio/leaflet")

library(shiny)
library(dismo)
library(rgbif)
library(spThin)
library(ENMeval)
library(dismo)
library(rgeos)
library(ggplot2)
library(leaflet)
library(devtools)
#install_github("rCharts", "ramnathv", ref = "dev")

#source("functions.R")

## -------------------------------------------------------------------- ##
## Define funtions
## -------------------------------------------------------------------- ##

nameAbbr <- function(spname) {
  namespl <- strsplit(tolower(spname[1,1]), " ")
  genusAbbr <- substring(namespl[[1]][1], 1, 1)
  fullNameAbbr <- paste0(genusAbbr, "_", namespl[[1]][2])
  return(fullNameAbbr)
}

mcp <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1))))
}

plotMap <- function(pts=NULL, poly=NULL, pred=NULL) {
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
    mp <- ggplot(data = pred.df, aes(x = lon, y = lat)) + 
      mapWorld + theme(panel.background = element_rect(fill = 'lightblue')) + 
      geom_raster(aes(fill = val)) + 
      coord_cartesian(xlim =c(e[1] - 2, e[2] + 2), ylim =c(e[3] - 2, e[4] + 2)) +
      scale_fill_gradient("relative suitability", limits = c(pred@data@min, pred@data@max), low = 'grey', high = 'blue')
    print(mp)
  }
}

## -------------------------------------------------------------------- ##
## Main shinyServer code
## -------------------------------------------------------------------- ##

shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
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
          #Remove duplicated rows
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
  
  output$downloadGBIFcsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$gbifoccs, file)
    }
  )
      
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
  
  # map1 <- createLeafletMap(session, 'map1')
  map1 <- leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2)
  output$map1 <- renderLeaflet(map1)
  
  observe({
    if (input$goName == 0) return()
    input$goName
    input$remove
    isolate({
      map1$clearShapes()
      lati <- values$gbifoccs[, 3]
      longi <- values$gbifoccs[, 2]

      output$map1 <- renderLeaflet(map1)
      map1$fitBounds(max(lati), max(longi), min(lati), min(longi))
      map1$addCircle(lati, longi, layerId=as.numeric(rownames(values$gbifoccs)), 
                    radius=8, options=list(weight=8, fill=FALSE, color='red'))
    })
  })

  observe({
    # map1$clearPopups()
    event <- input$map_shape_click
    if (is.null(event)){return()}
    isolate({
      content <- as.character(tagList(
        tags$strong(paste("ID:", event$id)),
        tags$br(),
        tags$strong(paste("Latitude:", event$lat)),        
        tags$strong(paste("Longitude:", event$lng))                    
        ))
      map1 <- map1 %>% addPopups(event$lat, event$lng, content)
      # map1$showPopup(event$lat, event$lng, content)
      map1
    })
  })

  runThin <- reactive({
    input$goThin
    isolate({
      withProgress(message = "Thinning...", {
        output <- thin(values$df, 'lat', 'lon', 'name', thin.par = input$thinDist, 
                       reps = 10, locs.thinned.list.return = TRUE, write.files = FALSE,
                       verbose = FALSE)
        thinout <- cbind(rep(values$df[1,1], nrow(output[[1]])), output[[1]])
        names(thinout) <- c('name', 'lon', 'lat')
        values$df <- thinout
        values$thinoccs <- thinout
      })
    })
  })
  
  output$thinOccTbl <- renderTable({values$thinoccs})
  
  map2 <- createLeafletMap(session, 'map2')
  
  observe({
    if (input$goThin == 0) return()
    input$goThin
    isolate({
      map2$clearShapes()
      lati <- values$gbifoccs[, 3]
      longi <- values$gbifoccs[, 2]
      lati2 <- values$thinoccs[, 3]
      longi2 <- values$thinoccs[, 2]
#       lats <- c(lati, lati2)
#       lons <- c(longi, longi2)
#       allrownames <- c(rownames(values$gbifoccs), rownames(values$thinoccs))
#       ngbif <- nrow(values$gbifoccs)
#       nthin <- nrow(values$thinoccs)
#       ntot <- ngbif + nthin
      map2$fitBounds(max(lati), max(longi), min(lati), min(longi))
      map2$addCircle(lati2, longi2, layerId = as.numeric(rownames(values$thinoccs)), 
                     radius=8, options=list(weight=8, fill=FALSE, color='blue'))
      map2$addCircle(lati, longi, layerId = as.numeric(rownames(values$gbifoccs)), 
                     radius=8, options=list(weight=8, fill=FALSE, color='red'))
#       map2$addCircle(lats, lons, layerId = as.numeric(allrownames), 
#                      radius=8, eachOptions=list(weight=rep(8, times=ntot), 
#                                                 fill=rep(FALSE, times=ntot), 
#                                                 color=c(rep('red', times=ngbif), rep('blue', times=nthin))))
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

  output$thinText <- renderText({
    if (input$goThin == 0) return()
    input$goThin
    runThin()
    paste('Total records thinned to [', nrow(values$df), '] points.')
  })
  
  output$downloadThincsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_thinned_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$thinoccs, file)
    }
  )
  
  
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
  
#   output$predTxt2 <- renderUI({
#     if (input$userPred == "") return()
#     isolate({
#       files <- file.path(input$userPred, list.files(input$userPred))
#       values$pred <- stack(files)
#       paste("Using user-provided environmental data.")
#     })
#   })
  
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
      values$backgExt <- gBuffer(xy_mcp, width = input$backgBuf + res(values$pred)[1])
      values$backgExt2Map <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords
    }
  })
  
  map3 <- createLeafletMap(session, 'map3')
  
  observe({
    if (input$backg == "") return()
    input$backg
    valores <- values$df
    makeBackgExt()
    back <- values$backgExt2Map
    isolate({
      map3$clearShapes()      
      lati3 <- valores[, 3]
      longi3 <- valores[, 2]
      map3$fitBounds(max(lati3), max(longi3), min(lati3), min(longi3))
      map3$addCircle(lati3, longi3, layerId = as.numeric(rownames(valores)),
                     radius=8, options=list(weight=8, fill=FALSE, color='red'))
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
  
  output$evalTbl <- renderTable({values$evalTbl})
  
  output$evalTxt <- renderText({
    if (input$goEval == 0) return()
    input$goEval
    runENMeval()
    if (!is.null(values$evalTbl)) {
      paste("Ran ENMeval and output table with", nrow(values$evalTbl), "rows.")  
    }
  })
  
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
      text(x=1, y=fc+.75, labels="Feature Classes", adj=.20, cex=1.3, font=2)
      eval.plot(values$evalTbl, legend=FALSE, value="delta.AICc")
      eval.plot(values$evalTbl, legend=FALSE, value="Mean.AUC", variance="Var.AUC")
      eval.plot(values$evalTbl, legend=FALSE, value="Mean.AUC.DIFF", variance="Var.AUC.DIFF")
      eval.plot(values$evalTbl, legend=FALSE, value="Mean.ORmin", variance="Var.ORmin")
      eval.plot(values$evalTbl, legend=FALSE, value="Mean.OR10", variance="Var.OR10")
    }
  })
   
  output$downloadEvalcsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_enmeval_results.csv")},
    content = function(file) {
      write.csv(values$evalTbl, file)
    }
  )
  
  output$predSel <- renderUI({
    if (is.null(values$evalPreds)) return()
    n <- names(values$evalPreds)
    predNameList <- setNames(as.list(seq(1, length(n))), n)
    values$rdyPlot <- 'rdy'
    selectInput("predSelServer", label = "Choose a model",
                choices = predNameList)
  })
  
  
#   observe({
#     if (values$predPlotRdy) {
#       values$goPlot <- TRUE
#     }
#   })
  
  output$plotPred <- renderPlot({
    plotMap(pred = values$evalPreds[[as.numeric(input$predSelServer)]])
  })
  
  output$downloadPred <- downloadHandler(
    filename = function() {paste0(names(values$evalPreds[[as.numeric(input$predSelServer)]]), "_pred.tif")},
    content = function(file) {
      res <- writeRaster(values$evalPreds[[as.numeric(input$predSelServer)]], file, format = "GTiff", overwrite = TRUE)
      file.rename(res@file@name, file)
    }
  )
  
#   output$thinConsole <- renderPrint({
#     if (input$goThin == 0) return()
#     input$goThin
#     isolate({values[["log"]] <- capture.output(runThin())})
#     return(print(values[["log"]]))
#   })

})
