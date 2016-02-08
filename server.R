# check package dependencies, and download if necessary
list.of.packages <- c("shiny", "maps", "RColorBrewer", "rmarkdown", "shinyjs", "rgbif", "devtools",
                      "spThin", "colorRamps", "dismo", "rgeos", "XML", "repmis", "Rcpp", "RCurl", "curl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
# use devtools to install leaflet and new unreleased version of ENMeval from github
if (!require('leaflet')) devtools::install_github('rstudio/leaflet')
# for exp version of ENMeval with special updateProgress param for shiny
#install_github("bobmuscarella/ENMeval@ENMeval_v0.1.2")
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
library(shinyjs)
library(RColorBrewer)
library(leaflet)
library(repmis)
library(rmarkdown)

source("functions.R")

# make list to carry data used by multiple reactive functions
brk <- paste(rep('------', 14), collapse='')
values <- reactiveValues(polyID=0, polyErase=FALSE, log=c(paste('***WELCOME TO WALLACE***', brk, 'Please find messages for the user in this log window.', brk, sep='<br>')))

# add text to log
writeLog <- function(x) {
  values$log <- paste(values$log, x, sep='<br>')
}
## functions for text formatting in userReport.Rmd
makeCap <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
getGBIFname <- function() deparse(substitute(input$gbifName))
printVecAsis <- function(x) {
  ifelse(length(x) == 1, x, 
         ifelse(is.character(x), paste0("c(", paste(sapply(x, function(a) paste0("\'",a,"\'")), collapse=", "), ")"),
                paste0("c(", paste(x, collapse=", "), ")")))}

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
  
  #########################
  ### INITIALIZE
  #########################
           
  output$log <- renderUI({tags$div(id='logHeader',
                                   tags$div(id='logContent', HTML(paste0(values$log, "<br>", collapse = ""))))})

  # create map
  map <- leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2)
  output$map <- renderLeaflet(map)

  # make map proxy to make further changes to existing map
  proxy <- leafletProxy("map")
  
  #########################
  ### COMPONENT 1 FUNCTIONALITY
  #########################

  # module GBIF
  observeEvent(input$goName, {
    if (input$gbifName == "") return()
    getGbifOccs(input$gbifName, input$gbifNum)
    shinyjs::enable("downloadOrigOccs")
  })

  # module userOccs
  observe({
    if (is.null(input$userCSV)) return()  # exit if userCSV not specifed
    getUserOccs(input$userCSV$datapath)
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
  ### COMPONENT 2 FUNCTIONALITY
  #########################

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
  output$downloadThincsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$origOccs), "_thinned_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$df[,1:9], file, row.names = FALSE)
    }
  )

  #########################
  ### COMPONENT 3 FUNCTIONALITY
  #########################
  
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
  ### COMPONENT 4 FUNCTIONALITY
  #########################
  
  # module Select Study Region - set buffer, extent shape
  observe({
    if (is.null(values$preds)) return()
    if (input$tabs == 4) comp4_studyReg(input$backgBuf, input$backgSelect)
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
  ### COMPONENT 5 FUNCTIONALITY
  #########################
  
  observe({
    if (input$partSelect == 'nsp') {
      updateSelectInput(session, "partSelect2", choices=list("None selected" = '',
                                                             "Jackknife (k = n)" = "jack", 
                                                             "Random k-fold" = "random"))  
    } else if (input$partSelect == 'sp') {
      updateSelectInput(session, "partSelect2", choices=list("None selected" = '',
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
    comp5_setPartitions(input$partSelect2, input$kfolds, input$aggFact, proxy)
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
  ### COMPONENT 6 FUNCTIONALITY
  #########################
  
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
    
    # Module BIOCLIM
    if (input$modSelect == "BIOCLIM") {
      comp6_bioclimMod()
      # Module BIOCLIM Envelope Plots (for component 7)
      output$bcEnvelPlot <- renderPlot(plot(values$evalMods[[1]], a = input$bc1, b = input$bc2, p = input$bcProb))
    }
    # Module Maxent
     else if (input$modSelect == "Maxent") {
       comp6_maxentMod(input$rms, input$fcs)
       # Module Maxent Evaluation Plots (for component 7): ENMeval graphs
       output$mxEvalPlot <- renderPlot(evalPlot(values$evalTbl, input$mxEvalSel))
       shinyjs::enable("downloadEvalPlots")
     }
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
  ### COMPONENT 7 FUNCTIONALITY
  #########################
  
  observe({
    if (is.null(values$df)) return()
    if (input$tabs == 7) {
      map_plotLocs(values$df, fillColor='black', fillOpacity=0.8)
      proxy %>% addLegend("topright", colors = c('black'),
                          title = "GBIF Records", labels = c('retained'),
                          opacity = 1, layerId = 1)    
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
    values$curMod <- values$evalMods[[which(as.character(values$evalTbl[, 1]) == input$modelSel2)]]
    nonZeroPreds <- mxNonzeroPreds(values$curMod)  # need function from pete's ms
    nonZeroPredNames <- names(values$predsMsk[[nonZeroPreds]])
    nonZeroPredNames <- nonZeroPredNames[order(as.integer(sub('bio', '', nonZeroPredNames)))]  # order by name
    predVarNameList <- setNames(as.list(nonZeroPredNames), nonZeroPredNames)
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
    if (is.null(input$visSelect)) return()
    if (input$visSelect != 'response') return()
    if (is.null(values$curMod)) return()
    output$respCurv <- renderPlot(response(values$curMod, var = input$predVarSel))
  })
  
  # Module Plot Prediction
  observeEvent(input$plotPred, {
    comp7_mapPred(input$modelSel1, input$predForm, input$predThresh, proxy)
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
  ### COMPONENT 8 FUNCTIONALITY
  #########################
  
  # functionality for drawing polygons on map
  observe({
    if (input$tabs == 8) {
      if (is.null(values$df)) return()
      map_plotLocs(values$df, fillColor='black', fillOpacity=0.8, clearShapes=FALSE)
      proxy %>% addLegend("topright", colors = c('black'),
                          title = "GBIF Records", labels = c('retained'),
                          opacity = 1, layerId = 1)   
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
  
  # erase select localities polygon with button click
  observeEvent(input$erasePolyProjExt, {
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    values$poly2 <- NULL
    values$polyPts2 <- NULL
    proxy %>% clearShapes()
    proxy %>% clearImages()
    writeLog('* RESET PROJECTION EXTENT')
  })
  
  # copy for response curve model selection
  output$modelSel3 <- renderUI({
    if (is.null(values$evalPreds)) return()
    n <- names(values$evalPreds)
    predNameList <- setNames(as.list(seq(1, length(n))), n)
    selectInput("modelSel3", label = "Choose a model",
                choices = predNameList)
  })
  
  # Module Select Localities: select points intersecting drawn polygons (replace values$df)
  observeEvent(input$poly2Sel, {
    comp8_selProjExt()
  })
  
  observeEvent(input$goPjCur, {
    comp8_pjModel(input$modelSel3, values$preds)
  })
  
  observeEvent(input$goMESS, {
    comp8_mess(values$preds)
  })
  
  output$downloadPj <- downloadHandler(
    filename = function() {
      ext <- ifelse(input$pjFileType == 'raster', 'zip',
                    ifelse(input$pjFileType == 'ascii', 'asc',
                           ifelse(input$pjFileType == 'GTiff', 'tif', 'png')))
      paste0(values$rasName, "_", input$predForm, "_", input$predThresh, "_pj.", ext)},
    content = function(file) {
      if (input$pjFileType == 'png') {
        png(file)
        image(values$pj)
        dev.off()
      } else if (input$pjFileType == 'raster') {
        fileName <- paste0(values$rasName, "_", input$predForm, "_", input$predThresh, "_pj")
        tmpdir <- tempdir()
        writeRaster(values$pj, file.path(tmpdir, fileName), format = input$pjFileType, overwrite = TRUE)
        fs <- file.path(tmpdir, paste0(fileName, c('.grd', '.gri')))
        zip(zipfile=file, files=fs, extras = '-j')
      } else {
        res <- writeRaster(values$pj, file, format = input$pjFileType, overwrite = TRUE)
        file.rename(res@file@name, file)
      }
    }
  )
  
  #########################
  ### MARKDOWN FUNCTIONALITY
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
      exp <- knit_expand('userReport.Rmd', gbifName=input$gbifName, occurrences=input$gbifNum, thinDist=input$thinDist,
                         occsCSV=input$userCSV$datapath, occsRemoved=printVecAsis(values$removedAll), occsSel=printVecAsis(values$ptSeln),
                         predsRes=input$bcRes, backgSel=input$backgSelect, backgBuf=input$backgBuf, userBGname=input$userBackg$name,
                         userBGpath=input$userBackg$datapath, partSel=values$partSelect2, aggFact=input$aggFact, kfoldsSel=input$kfolds, 
                         modSel=input$modSelect, rmsSel1=input$rms[1], rmsSel2=input$rms[2], rmsBy=input$rmsBy, fcsSel=printVecAsis(input$fcs))
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
