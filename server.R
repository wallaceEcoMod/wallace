# check package dependencies, and download if necessary
list.of.packages <- c("shiny", "maps", "RColorBrewer", "rmarkdown", "shinyjs", "rgbif", "devtools",
                      "spThin", "colorRamps", "dismo", "rgeos", "XML", "repmis", "Rcpp", "RCurl", "curl",
                      "maptools", "rgdal", "rJava", "devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
# use devtools to install leaflet and new unreleased version of ENMeval from github
if (!require('leaflet')) devtools::install_github('rstudio/leaflet')
# for exp version of ENMeval with special updateProgress param for shiny
if (!require('ENMeval')) {
  install_github("bobmuscarella/ENMeval@ENMeval_v0.1.2")
} else {
  if (packageVersion('ENMeval') != '0.1.2') install_github("bobmuscarella/ENMeval@ENMeval_v0.1.2")
}
#if (!require("DT")) devtools::install_github("rstudio/DT")
#options(shiny.error=browser)  # for debugging

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
library(shinyjs)
library(RColorBrewer)
library(leaflet)
library(repmis)
library(rmarkdown)
library(rgdal)

source("functions.R")

# make list to carry data used by multiple reactive functions
brk <- paste(rep('------', 14), collapse='')
logInit <- c(paste('***WELCOME TO WALLACE***', brk, 'Please find messages for the user in this log window.', brk, sep='<br>'))
values <- reactiveValues(polyID=0, polyErase=FALSE, log=logInit)
gtext <- reactiveValues()

# add text to log
writeLog <- function(x) {
  values$log <- paste(values$log, x, sep='<br>')
}

## functions for text formatting in userReport.Rmd
makeCap <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
getGBIFname <- function() deparse(substitute(input$gbifName))
printVecAsis <- function(x) {
  if (is.character(x)) {
    if (length(x) == 1) {
      return(paste0("\'", x, "\'"))
    } else {
      return(paste0("c(", paste(sapply(x, function(a) paste0("\'", a, "\'")), collapse=", "), ")"))
    }
  } else {
    if (length(x) == 1) {
      return(x)
    } else {
      return(paste0("c(", paste(x, collapse=", "), ")"))
    }
  }
}
  # ifelse(length(x) == 1, x,
  #        ifelse(is.character(x), paste0("c(", paste(sapply(x, function(a) paste0("\'",a,"\'")), collapse=", "), ")"),
  #               paste0("c(", paste(x, collapse=", "), ")")))}

#devtools::install_github("jcheng5/rasterfaster")

shinyServer(function(input, output, session) {
  # disable download buttons
  shinyjs::disable("downloadOrigOccs")
  shinyjs::disable("downloadThincsv")
  shinyjs::disable("predDnld")
  shinyjs::disable("downloadMskPreds")
  shinyjs::disable("downloadPart")
  shinyjs::disable("downloadEvalcsv")
  shinyjs::disable("downloadEvalPlots")
  shinyjs::disable("downloadPred")
  shinyjs::disable("downloadPj")

  # load modules
  for (f in list.files('./modules')) {
    source(file.path('modules', f), local=TRUE)
  }

  # UI for guidance text collapse bar
  output$gtextOut <- renderUI({
    includeMarkdown(gtext$cur)
  })

#########################
### INITIALIZE ####
#########################

  output$log <- renderUI({tags$div(id='logHeader',
                                   tags$div(id='logContent', HTML(paste0(values$log, "<br>", collapse = ""))))})
  curWD <- getwd()

  # create map
  map <- leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2)
  output$map <- renderLeaflet(map)

  # make map proxy to make further changes to existing map
  proxy <- leafletProxy("map")

#########################
### COMPONENT 1 FUNCTIONALITY ####
#########################

  # guidance text behavior
  observe({
    if (input$tabs == 1) {
      if (input$occSel == 'GBIF') gtext$cur <- "www/tab1_gbif.Rmd"
      if (input$occSel == 'user') gtext$cur <- "www/tab1_user.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
    }
  })

  # module GBIF
  observeEvent(input$goName, {
    if (input$gbifName == "") return()
    isolate(resetV(values))
    getGbifOccs(input$gbifName, input$gbifNum)
    shinyjs::enable("downloadOrigOccs")
  })

  # module userOccs
  observe({
    if (is.null(input$userCSV)) return()  # exit if userCSV not specifed
    isolate(resetV(values))
    getUserOccs(input$userCSV$datapath)
    print(values$spname)
  })

  # render the GBIF records data table
  observe({
    if (is.null(values$df)) return()
    if (length(names(values$df)) >= 7) {
      options <- list(autoWidth = TRUE, columnDefs = list(list(width = '40%', targets = 7)),
                      scrollX=TRUE, scrollY=400)
    } else {
      options <- list()
    }
    output$occTbl <- DT::renderDataTable({DT::datatable(values$df[, -which(names(values$df) %in% c('origID', 'pop'))], options = options)})
  })

  # handle downloading of original GBIF records after cleaning
  output$downloadOrigOccs <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$df), "_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$gbifOrig, file, row.names=FALSE)
    }
  )

#########################
### COMPONENT 2 FUNCTIONALITY ####
#########################

  # guidance text
  observe({
    if (input$tabs == 2) {
      if (input$procOccSel == 'selpts') gtext$cur <- "www/tab2_selpts.Rmd"
      if (input$procOccSel == 'spthin') gtext$cur <- "www/tab2_spthin.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
    }
  })

  # functionality for drawing polygons on map
  observe({
    if (input$tabs == 2 && input$procOccSel == "selpts") {
      if (is.null(input$map_click)) return()
      lonlat <- c(input$map_click$lng, input$map_click$lat)

      if (values$polyErase) {
        if (identical(lonlat, values$mapClick)) return()
        values$polyErase <- FALSE
      }

      values$mapClick <- lonlat
      values$polyPts1 <- isolate(rbind(values$polyPts1, lonlat))
      proxy %>% removeShape("poly1")
      proxy %>% addPolygons(values$polyPts1[,1], values$polyPts1[,2],
                            layerId='poly1', fill=FALSE, weight=3, color='green')
    }
  })

  # plots all polygons used for selection and fills them with ColorBrewer colors
  observe({
    if (!is.null(values$poly1)) {
      curPolys <- values$poly1@polygons
      numPolys <- length(curPolys)
      colors <- brewer.pal(numPolys, 'Accent')
      for (i in numPolys) {
        curPoly <- curPolys[i][[1]]@Polygons[[1]]@coords
        proxy %>% addPolygons(curPoly[,1], curPoly[,2],
                              weight=3, color=colors[i])
      }
    }
  })

  # Module Select Localities: select points intersecting drawn polygons (replace values$df)
  observeEvent(input$selectPoly, {
    polySelLocs()
  })

  # governs point removal behavior and modifies tables in "values"
  observeEvent(input$remove, {
    if (!is.null(values$ptsSel)) {
      writeLog('NOTICE: Remove localities by ID before selecting with polygons. Press "Reset Localities" to start over.')
      return()
      }
    remSelLocs(input$remLoc)
  })

  # erase select localities polygon with button click
  observeEvent(input$erasePolySelLocs, {
    values$ptsSel <- NULL
    values$polyPts1 <- NULL
    values$poly1 <- NULL
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    proxy %>% clearShapes()
    map_plotLocs(values$origOccs)
    x <- paste('* RESET: localities dataset is now back to', nrow(values$origOccs), 'records.')
    isolate(writeLog(x))
    if (!is.null(values$origOccs)) {
      values$origOccs <- rbind(values$origOccs, values$removed)
      values$df <- values$origOccs
    }
    lati <- values$df[,3]
    longi <- values$df[,2]
    z <- smartZoom(longi, lati)
    proxy %>% fitBounds(z[1], z[2], z[3], z[4])
  })

  # Module Spatial Thin
  observeEvent(input$goThin, {
    thinOccs(input$thinDist)
    shinyjs::enable("downloadThincsv")
  })

  # handle download for thinned records csv
  output$dlProcOccCsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$origOccs), "_procOccs.csv")},
    content = function(file) {
      write.csv(values$df[,1:9], file, row.names = FALSE)
    }
  )

#########################
### COMPONENT 3 FUNCTIONALITY ####
#########################

  # guidance text
  observe({
    if (input$tabs == 3) {
      if (input$envSel == 'WorldClim') gtext$cur <- "www/tab3_worldclim.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
      proxy %>% removeControl('selLegend')
      proxy %>% removeControl('thinLegend')
    }
  })

  # enable download button
  observe({if (input$bcRes != "") shinyjs::enable("predDnld")})

  observe({
    if (is.null(values$df)) return()
    if (input$tabs == 3) map_plotLocs(values$df)
  })

  # module WorldClim
  observeEvent(input$predDnld, {
    if (!is.null(values$df)) {
      comp3_bioclim(input$bcRes)
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

#########################
### COMPONENT 4 FUNCTIONALITY ####
#########################

  # guidance text
  observe({
    if (input$tabs == 4) {
      if (input$envProcSel == 'backg') gtext$cur <- "www/tab4_backg.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
    }
  })

  # module Select Study Region - set buffer, extent shape
  observe({
    if (is.null(values$preds)) return()
    if (input$tabs == 4) comp4_studyReg(input$backgBuf, input$backgSel)
  })

  # module Select Study Region - mask out environmental predictors by background extent
  observeEvent(input$goBackgMask, {
    if (is.null(values$preds)) {
      writeLog("* Obtain the environmental data first...")
      return()
    }
    if (is.null(values$preds)) return()
    comp4_mskStudyReg()
    shinyjs::enable("downloadMskPreds")
  })

  # handle download for masked predictors, with file type as user choice
  output$downloadMskPreds <- downloadHandler(
    filename = function() {'mskBioPreds.zip'},
    content = function(file) {
      tmpdir <- tempdir()
      writeRaster(values$predsMsk, file.path(tmpdir, 'mskBio'), bylayer = TRUE,
                  format = input$mskPredsFileType, overwrite = TRUE)
      nr <- nlayers(values$predsMsk)
      ext <- ifelse(input$mskPredsFileType == 'raster', 'grd',
                    ifelse(input$mskPredsFileType == 'ascii', 'asc',
                           ifelse(input$mskPredsFileType == 'GTiff', 'tif', 'png')))
      fs <- file.path(tmpdir, paste0(rep('mskBio_', nr), 1:nr, '.', ext))
      if (ext == 'grd') {
        fs <- c(fs, file.path(tmpdir, paste0(rep('mskBio_', nr), 1:nr, '.gri')))
      }
      zip(zipfile=file, files=fs, extras = '-j')
    },
    contentType = "application/zip"
  )

#########################
### COMPONENT 5 FUNCTIONALITY ####
#########################

  # guidance text
  observe({
    if (input$tabs == 5) {
      if (input$partSel == 'nsp') gtext$cur <- "www/tab5_nsp.Rmd"
      if (input$partSel == 'sp') gtext$cur <- "www/tab5_sp.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
    }
  })

  observe({
    if (input$partSel == 'nsp') {
      updateSelectInput(session, "partSel2", choices=list("None selected" = '',
                                                             "Jackknife (k = n)" = "jack",
                                                             "Random k-fold" = "random"))
    } else if (input$partSel == 'sp') {
      updateSelectInput(session, "partSel2", choices=list("None selected" = '',
                                                             "Block (k = 4)" = "block",
                                                             "Checkerboard 1 (k = 2)" = "cb1",
                                                             "Checkerboard 2 (k = 4)" = "cb2"))
    }
    if (is.null(values$df)) return()
    if (input$tabs == 5) map_plotLocs(values$df)
  })

  # module Set Partitions
  observeEvent(input$goPart, {
    if (is.null(values$predsMsk)) {
      writeLog("* WARNING: Clip the environmental variables by the study extent polygon first in COMPONENT 4.")
      return()
    }
    if (input$partSel2 == "") {
      writeLog("* Select one of the modules available first...")
      return()
    }
    values$partSel2 <- input$partSel2  # save it to values or else it disappears
    comp5_setPartitions(values$partSel2, input$kfolds, input$aggFact, proxy)
    shinyjs::enable("downloadPart")


  })

  # handle download for partitioned occurrence records csv
  output$downloadPart <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$origOccs), "_partitioned_occs.csv")},
    content = function(file) {
      bg.bind <- cbind(rep('background', nrow(values$bg.coords)), values$bg.coords)
      names(bg.bind) <- c('species', 'longitude', 'latitude')
      dfbg.bind <-rbind(values$df[,1:3], bg.bind)
      all.bind <- cbind(dfbg.bind, c(values$modParams$occ.grp, values$modParams$bg.grp))
      names(all.bind)[4] <- "partitionID"
      write.csv(all.bind, file, row.names = FALSE)
    }
  )

#########################
### COMPONENT 6 FUNCTIONALITY ####
#########################

  # guidance text
  observe({
    if (input$tabs == 6) {
      if (input$enmSel == 'BIOCLIM') gtext$cur <- "www/tab6_bc.Rmd"
      if (input$enmSel == 'Maxent') gtext$cur <- "www/tab6_maxent.Rmd"
    }
  })

  # niche model selection and warnings
  observeEvent(input$goEval, {
    if (is.null(values$predsMsk)) {
      writeLog("* WARNING: Mask the environmental variables first in COMPONENT 4.")
      return()
    }
    if (is.null(values$modParams)) {
      writeLog("* WARNING: Partition your localities first in COMPONENT 5.")
      return()
    }
    values$predsLog <- NULL  # reset predsLog if models are rerun
    values$enmSel <- input$enmSel 

    # Module BIOCLIM
    if (input$enmSel == "BIOCLIM") {
      comp6_bioclimMod()
      # Module BIOCLIM Envelope Plots (for component 7)
      output$bcEnvelPlot <- renderPlot(plot(values$evalMods[[1]], a = input$bc1, b = input$bc2, p = input$bcProb))
    }
    # Module Maxent
     else if (input$enmSel == "Maxent") {
       comp6_maxentMod(input$rms, input$fcs)
       # Module Maxent Evaluation Plots (for component 7): ENMeval graphs
       output$mxEvalPlot <- renderPlot(evalPlot(values$evalTbl, input$mxEvalSel))
       shinyjs::enable("downloadEvalPlots")
     }
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    shinyjs::enable("downloadEvalcsv")
  })

  # handle downloads for ENMeval results table csv
  output$downloadEvalcsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$origOccs), "_enmeval_results.csv")},
    content = function(file) {
      write.csv(values$evalTbl, file, row.names = FALSE)
    }
  )

#########################
### COMPONENT 7 FUNCTIONALITY ####
#########################

  # guidance text
  observe({
    if (input$tabs == 7) {
      if (input$visSel == 'map') gtext$cur <- "www/tab7_map.Rmd"
      if (input$visSel == 'response') gtext$cur <- "www/tab7_respCurves.Rmd"
      if (input$visSel == 'bcEnvel') gtext$cur <- "www/tab7_bcPlots.Rmd"
      if (input$visSel == 'mxEval') gtext$cur <- "www/tab7_mxEvalPlots.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
    }
  })

  observe({
    if (is.null(values$df)) return()
    if (input$tabs == 7) {
      map_plotLocs(values$df, clearImages=FALSE)
      proxy %>% removeControl('r2Legend')
      if (!is.null(values$leg1)) {
        proxy %>% addLegend("topright", pal = values$leg1$pal, title = "Predicted Suitability",
                            values = values$leg1$rasVals, layerId = 'r1Legend')
      }
      proxy %>% showGroup('r1')
      proxy %>% hideGroup('r2')
    }
  })

  # generates user selection of rasters to plot dynamically after they are created
  output$modelSel1 <- renderUI({
    if (is.null(values$evalPreds)) return()
    n <- names(values$evalPreds)
    predNameList <- setNames(as.list(seq(1, length(n))), n)
    selectInput("modelSel1", label = "Choose a model",
                choices = predNameList)
  })

  # copy for response curve model selection
  output$modelSel2 <- renderUI({
    if (is.null(values$evalPreds)) return()
    n <- names(values$evalPreds)
    predNameList <- setNames(as.list(n), n)
    selectInput("modelSel2", label = "Choose a model",
                choices = predNameList, selected = predNameList[[1]])
  })

  # generates list of predictor variables with non-zero coeffs for currently selected model
  output$predVarSel <- renderUI({
    if (is.null(values$evalPreds)) return()

      if(input$enmSel == "Maxent"){
        values$curMod <- values$evalMods[[which(as.character(values$evalTbl[, 1]) == input$modelSel2)]]
        nonZeroPreds <- mxNonzeroPreds(values$curMod)
        nonZeroPredNames <- names(values$predsMsk[[nonZeroPreds]])
        nonZeroPredNames <- nonZeroPredNames[order(as.integer(sub('bio', '', nonZeroPredNames)))]  # order by name
        predVarNameList <- setNames(as.list(nonZeroPredNames), nonZeroPredNames)
      } else {
        values$curMod <- values$evalMods[[1]]
        predVarNameList <- names(values$curMod@presence)
      }

    radioButtons("predVarSel", "Choose a predictor variable:",
                 choices = predVarNameList, selected = predVarNameList[[1]])

  })

  # handle downloads for ENMeval plots png
  output$downloadEvalPlots <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$origOccs), "_enmeval_plots.png")},
    content = function(file) {
      png(file)
      evalPlot(values$evalTbl)
      dev.off()
    }
  )

  # Module Response Curves
  observe({
    if (is.null(input$visSel)) return()
    if (input$visSel != 'response') return()
    if (is.null(values$curMod)) return()
      output$respCurv <- renderPlot(response(values$curMod, var = input$predVarSel))
      # switch to Results tab
      updateTabsetPanel(session, 'main', selected = 'Results')
  })

  # Module Plot Prediction
  observeEvent(input$plotPred, {
    comp7_mapPred(input$modelSel1, input$predForm, input$predThresh, proxy)
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Map')
  })

  # handle download for rasters, with file type as user choice
  output$downloadPred <- downloadHandler(
    filename = function() {
      ext <- ifelse(input$predFileType == 'raster', 'zip',
                    ifelse(input$predFileType == 'ascii', 'asc',
                           ifelse(input$predFileType == 'GTiff', 'tif', 'png')))
      paste0(values$rasName, "_", input$predForm, "_", input$predThresh, "_pred.", ext)},
    content = function(file) {
      if (input$predFileType == 'png') {
        png(file)
        image(values$predCur)
        dev.off()
      } else if (input$predFileType == 'raster') {
        fileName <- paste0(values$rasName, "_", input$predForm, "_", input$predThresh, "_pred")
        tmpdir <- tempdir()
        writeRaster(values$predCur, file.path(tmpdir, fileName), format = input$predFileType, overwrite = TRUE)
        fs <- file.path(tmpdir, paste0(fileName, c('.grd', '.gri')))
        zip(zipfile=file, files=fs, extras = '-j')
      } else {
        res <- writeRaster(values$predCur, file, format = input$predFileType, overwrite = TRUE)
        file.rename(res@file@name, file)
      }
    }
  )

#########################
### COMPONENT 8 FUNCTIONALITY ####
#########################

  # guidance text
  observe({
    if (input$tabs == 8) {
      if (input$projSel == 'pjArea') gtext$cur <- "www/tab8_pjarea.Rmd"
      if (input$projSel == 'pjTime') gtext$cur <- "www/tab8_pjtime.Rmd"
      if (input$projSel == 'mess') gtext$cur <- "www/tab8_mess.Rmd"
      proxy %>% removeControl('threshLegend')
    }
  })

  # functionality for drawing polygons on map
  observe({
    if (input$tabs == 8) {
      if (is.null(values$df)) return()
      map_plotLocs(values$df, clearImages=FALSE)
      proxy %>% removeControl('r1Legend')
      if (!is.null(values$leg2)) {
        proxy %>% addLegend("topright", pal = values$leg2$pal, title = "Predicted Suitability",
                            values = values$leg2$rasVals, layerId = 'r2Legend')
      }
      proxy %>% showGroup('r2')
      proxy %>% hideGroup('r1')
      if (!is.null(values$poly2)) return()  # if sel pj ext poly selected, don't allow more drawing
      if (is.null(input$map_click)) return()
      lonlat <- c(input$map_click$lng, input$map_click$lat)
     
      if (values$polyErase) {
        if (identical(lonlat, values$mapClick)) return()
        values$polyErase <- FALSE
      }

      values$mapClick <- lonlat
      values$polyPts2 <- isolate(rbind(values$polyPts2, lonlat))
      proxy %>% removeShape("poly2")
      proxy %>% addPolygons(values$polyPts2[,1], values$polyPts2[,2],
                            layerId='poly2', fill=FALSE, weight=4, color='red')
    }
  })

  # erase current projection extent
  observeEvent(input$erasePolyProjExt, {
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    values$poly2 <- NULL  # erase current pj ext polygon
    values$projMsk <- NULL  # erase current proj ext pred clips
    values$pjArea <- NULL
    values$mess <- NULL
    values$polyPts2 <- NULL
    proxy %>% clearShapes()
    proxy %>% removeImage('r2')
    proxy %>% removeControl('r2Legend')

    writeLog('* RESET PROJECTION EXTENT')
  })

  # model selection for component 8
  output$modelSel3 <- renderUI({
    if (is.null(values$evalPreds)) return()
    n <- names(values$evalPreds)
    predNameList <- setNames(as.list(seq(1, length(n))), n)
    selectInput("modelSel3", label = "Choose a model",
                choices = predNameList)
  })

  # select projection extent
  observeEvent(input$poly2Sel, {
    comp8_selProjExt()
  })

  # Module Project to New Area
  observeEvent(input$goPjArea, {
    comp8_pjArea(input$modelSel3, input$predForm, values$enmSel)
  })
  
  # Module Project to New Time
  observeEvent(input$goPjTime, {
    comp8_pjTime(input$modelSel3, input$predForm, values$enmSel,
                 input$bcRes, input$selRCP, bcMod, bcYr)
  })

#   # Module MESS
#   observeEvent(input$goMESS, {
#     comp8_mess()
#   })

  # Download current projected extent
  output$downloadPj <- downloadHandler(
    filename = function() {
      ext <- ifelse(input$pjFileType == 'raster', 'zip',
                    ifelse(input$pjFileType == 'ascii', 'asc',
                           ifelse(input$pjFileType == 'GTiff', 'tif', 'png')))
      paste0(values$rasName, "_", input$predForm, "_", input$predThresh, "_pj.", ext)},
    content = function(file) {
      if (input$pjFileType == 'png') {
        png(file)
        image(values$pjArea)
        dev.off()
      } else if (input$pjFileType == 'raster') {
        fileName <- paste0(values$rasName, "_", input$predForm, "_", input$predThresh, "_pj")
        tmpdir <- tempdir()
        writeRaster(values$pjArea, file.path(tmpdir, fileName), format = input$pjFileType, overwrite = TRUE)
        fs <- file.path(tmpdir, paste0(fileName, c('.grd', '.gri')))
        zip(zipfile=file, files=fs, extras = '-j')
      } else {
        res <- writeRaster(values$pjArea, file, format = input$pjFileType, overwrite = TRUE)
        file.rename(res@file@name, file)
      }
    }
  )


  #########################
  ### MARKDOWN FUNCTIONALITY ####
  #########################

  # handler for R Markdown download
  output$downloadMD <- downloadHandler(
    filename = function() {
      paste0("wallace-session-", Sys.Date(), ".", switch(
        input$mdType, Rmd = 'Rmd', PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))},
    content = function(file) {
      src <- normalizePath('userReport.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'userReport.Rmd')
      exp <- knit_expand('userReport.Rmd', curWD=curWD, gbifName=input$gbifName, occurrences=input$gbifNum, thinDist=input$thinDist,
                         occsCSV=input$userCSV$datapath, occsRemoved=printVecAsis(values$removedAll), occsSel=printVecAsis(values$ptSeln),
                         predsRes=input$bcRes, backgSel=input$backgSel, backgBuf=input$backgBuf, userBGname=input$userBackg$name,
                         userBGpath=input$userBackg$datapath, partSel=values$partSel2, aggFact=input$aggFact, kfoldsSel=input$kfolds,
                         enmSel=input$enmSel, rmsSel1=input$rms[1], rmsSel2=input$rms[2], rmsBy=input$rmsBy, fcsSel=printVecAsis(input$fcs))
      writeLines(exp, 'userReport2.Rmd')

      if (input$mdType == 'Rmd') {
        out <- render('userReport2.Rmd', md_document(variant="markdown_github"))
        writeLines(gsub('``` r', '```{r}', readLines(out)), 'userReport3.Rmd')
        out <- 'userReport3.Rmd'
      } else {
        out <- rmarkdown::render('userReport2.Rmd', switch(
          input$mdType,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
      }
      file.rename(out, file)
    }
  )
})
