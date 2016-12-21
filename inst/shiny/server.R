
source("funcs/functions.R", local = TRUE)

# make list to carry data used by multiple reactive functions
brk <- paste(rep('------', 14), collapse='')
logInit <- c(paste('***WELCOME TO WALLACE***', brk, 'Please find messages for the user in this log window.', brk, sep='<br>'))
values <- reactiveValues(polyID=0, polyErase=FALSE, log=logInit, mod_db=FALSE)
gtext <- reactiveValues()

# add text to log
writeLog <- function(x) {
  values$log <- paste(values$log, x, sep='<br>')
}

## functions for text formatting in userReport.Rmd
makeCap <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
getSpName <- function() deparse(substitute(input$spName))
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

options(shiny.maxRequestSize=5000*1024^2)

shinyServer(function(input, output, session) {
  # disable download buttons
  shinyjs::disable("dlDbOccs")
  shinyjs::disable("dlProcOccCsv")
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

  # UI for component guidance text
  output$gtext_comp <- renderUI({
    shiny::includeMarkdown(system.file('Rmd', gtext$cur_comp, package='wallace'))
  })

  # UI for module guidance text
  output$gtext_mod <- renderUI({
    shiny::includeMarkdown(system.file('Rmd', gtext$cur_mod, package='wallace'))
  })

#########################
### INITIALIZE ####
#########################

  output$log <- renderUI({tags$div(id='logHeader',
                                   tags$div(id='logContent', HTML(paste0(values$log, "<br>", collapse = ""))))})
  curWD <- getwd()

  # create map
  map <- leaflet() %>% setView(0, 0, zoom = 2) %>% addProviderTiles('Esri.WorldTopoMap')
  output$map <- renderLeaflet(map)

  # make map proxy to make further changes to existing map
  proxy <- leafletProxy("map")
  observe({
    proxy %>% addProviderTiles(input$bmap)
  })


#########################
### COMPONENT 1 ####
#########################

  # guidance text behavior
  observe({
    if (input$tabs == 1) {
      gtext$cur_comp <- 'gtext_comp1.Rmd'
      if (input$occSel == 'db') gtext$cur_mod <- "gtext_comp1_dbOccs.Rmd"
      if (input$occSel == 'user') gtext$cur_mod <- "gtext_comp1_userOccs.Rmd"
      # switch to Map tab
      # updateTabsetPanel(session, 'main', selected = 'Map')
      proxy %>% clearControls()
    }
  })

  # module GBIF
  observeEvent(input$goName, {
    if (input$spName == "") return()
    getDbOccs(input$spName, input$occNum)
    if (!is.null(values$df)) {shinyjs::enable("dlDbOccs")}
  })

  # module userOccs
  observe({
    if (is.null(input$userCSV)) return()  # exit if userCSV not specifed
    isolate({getUserOccs(input$userCSV)})
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
  output$dlDbOccs <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$df.orig), '_', input$occDb, ".csv")},
    content = function(file) {
      write.csv(values$df.orig, file, row.names=FALSE)
    }
  )

#########################
### COMPONENT 2 ####
#########################

  # guidance text
  observe({
    if (input$tabs == 2) {
      gtext$cur_comp <- "gtext_comp2.Rmd"
      # if Module: Select Points, populate guidance text and select legend
      if (input$procOccSel == 'selpts') {
        gtext$cur_mod <- "gtext_comp2_selectLocs.Rmd"
        proxy %>% addLegend("topright", colors = c('red','yellow'),
                            title = "Occ Records", labels = c('original', 'selected'),
                            opacity = 1, layerId = 'selLegend') %>%
          removeControl('thinLegend') %>% clearImages()
        # if points are already selected, plot the original occs in red and the selected ones with yellow fill
        if (!is.null(values$ptsSel)) {
          proxy %>%
            map_plotLocs(values$origOccs, clearShapes=FALSE) %>%
            map_plotLocs(values$ptsSel, fillColor='yellow', fillOpacity=1, clearShapes=FALSE, clearMarkers=FALSE) %>%
            zoom2Occs(values$origOccs)
        }
      }
      if (input$procOccSel == 'spthin') {
        gtext$cur_mod <- "gtext_comp2_spatialThin.Rmd"
        proxy %>% addLegend("topright", colors = c('red', 'blue'),
                            title = "Occ Records", labels = c('retained', 'removed'),
                            opacity = 1, layerId = 'thinLegend') %>%
          removeControl('selLegend') %>% clearImages() %>% clearShapes()
        if (!is.null(values$ptsSel)) {
          proxy %>% map_plotLocs(values$ptsSel)
          if (!is.null(values$prethinned)) {
            proxy %>% addCircleMarkers(data = values$prethinned, lat = ~latitude, lng = ~longitude,
                                       radius = 5, color = 'red', fillColor = 'blue',
                                       fillOpacity = 1, weight = 2, popup = ~pop,
                                       group = 'comp2') %>%
              addCircleMarkers(data = values$df, lat = ~latitude, lng = ~longitude,
                               radius = 5, color = 'red', fillColor = 'red',
                               fillOpacity = 1, weight = 2, popup = ~pop,
                               group = 'comp2') %>%
              zoom2Occs(values$prethinned)
          }
        }
      }
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
      colors <- RColorBrewer::brewer.pal(numPolys, 'Set1')
      for (i in 1:numPolys) {
        curPoly <- curPolys[i][[1]]@Polygons[[1]]@coords
        proxy %>% addPolygons(curPoly[,1], curPoly[,2], weight=3, color=colors[i])
      }
    }
  })

  # Module Select Localities: select points intersecting drawn polygons (replace values$df)
  observeEvent(input$selectPoly, {
    polySelLocs()
    print(values$df, n=50)
    print("BLARG")
    shinyjs::enable("dlProcOccCsv")
  })

  # governs point removal behavior and modifies tables in "values"
  observeEvent(input$remove, {
    if (!is.null(values$ptsSel)) {
      writeLog('NOTICE: Remove localities by ID before selecting with polygons. Press "Reset Localities" to start over.')
      return()
      }
    remSelLocs(input$remLoc)
    print(values$df, n=50)
    shinyjs::enable("dlProcOccCsv")
  })

  # erase select localities polygon with button click
  observeEvent(input$erasePolySelLocs, {
    validate(need(values$origOccs, message = FALSE))
    values$ptsSel <- NULL
    values$polyPts1 <- NULL
    values$poly1 <- NULL
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    proxy %>% clearMarkers() %>% clearShapes() %>% map_plotLocs(values$origOccs)
    if (!is.null(values$origOccs)) {
      values$origOccs <- rbind(values$origOccs, values$removed)
      values$df <- values$origOccs
      x <- paste('* RESET: localities dataset is now back to', nrow(values$origOccs), 'records.')
      isolate(writeLog(x))
    }
    proxy %>% zoom2Occs(values$origOccs) %>% map_plotLocs(values$origOccs)
  })

  # Module Spatial Thin
  observeEvent(input$goThin, {
    thinOccs(input$thinDist)
    shinyjs::enable("dlProcOccCsv")
  })

  # handle download for thinned records csv
  output$dlProcOccCsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$df), "_procOccs.csv")},
    content = function(file) {
      write.csv(values$df[,1:9], file, row.names = FALSE)
    }
  )

#########################
### COMPONENT 3 ####
#########################

  # guidance text
  observe({
    if (input$tabs == 3) {
      gtext$cur_comp <- "gtext_comp3.Rmd"
      if (input$envSel == 'WorldClim') gtext$cur_mod <- "gtext_comp3_worldclim.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
      # plot pts
      if (!is.null(values$df)) proxy %>% map_plotLocs(values$df)
      proxy %>% clearControls() %>% clearShapes()
    }
  })

  # map center coordinates for 30 arcsec download
  observe({
    mapCntr <- mapCenter(input$map_bounds)
    values$mapCntr <- mapCntr
    output$ctrLatLon <- renderText({paste('Using map center', paste(mapCntr, collapse=', '))})
  })

  # enable download button
  observe({if (input$bcRes != "") shinyjs::enable("predDnld")})

  # module WorldClim
  observeEvent(input$predDnld, {
    if (!is.null(values$df)) {
      comp3_bioclim(input$bcRes)
    }
  })

  # observeEvent(input$userPreds, {
  #   validate(need(input$userPreds, message = FALSE))
  #   comp3_userPreds(input$userPreds)
  # })

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
### COMPONENT 4 ####
#########################

  # guidance text
  observe({
    if (input$tabs == 4) {
      gtext$cur_comp <- "gtext_comp4.Rmd"
      if (input$envProcSel == 'backg') gtext$cur_mod <- "gtext_comp4_backg.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
      # plot pts
      if (!is.null(values$df)) proxy %>% map_plotLocs(values$df)
      proxy %>% clearControls()
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
    comp4_mskStudyReg()
    shinyjs::enable("downloadMskPreds")
  })

  # handle download for masked predictors, with file type as user choice
  output$downloadMskPreds <- downloadHandler(
    filename = function() {'mskBioPreds.zip'},
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())

      writeRaster(values$predsMsk, file.path(tmpdir, 'mskBio'), bylayer = TRUE,
                  format = input$mskPredsFileType, overwrite = TRUE)
      nr <- nlayers(values$predsMsk)
      ext <- ifelse(input$mskPredsFileType == 'raster', 'grd',
                    ifelse(input$mskPredsFileType == 'ascii', 'asc',
                           ifelse(input$mskPredsFileType == 'GTiff', 'tif', 'png')))
      fs <- paste0(rep('mskBio_', nr), 1:nr, '.', ext)
      if (ext == 'grd') {
        fs <- c(fs, paste0(rep('mskBio_', nr), 1:nr, '.gri'))
      }
      zip(zipfile=file, files=fs)
      if (file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
    },
    contentType = "application/zip"
  )

#########################
### COMPONENT 5 ####
#########################

  # guidance text
  observe({
    if (input$tabs == 5) {
      gtext$cur_comp <- "gtext_comp5.Rmd"
      if (input$partSel == 'nsp') gtext$cur_mod <- "gtext_comp5_nonspatial.Rmd"
      if (input$partSel == 'sp') gtext$cur_mod <- "gtext_comp5_spatial.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
      # plot pts
      if (!is.null(values$df)) proxy %>% map_plotLocs(values$df)
      proxy  %>% clearControls() %>% clearShapes()
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
      names(bg.bind) <- c('name', 'longitude', 'latitude')
      dfbg.bind <-rbind(values$df[,1:3], bg.bind)
      all.bind <- cbind(dfbg.bind, c(values$modParams$occ.grp, values$modParams$bg.grp))
      names(all.bind)[4] <- "partitionID"
      write.csv(all.bind, file, row.names = FALSE)
    }
  )

#########################
### COMPONENT 6 ####
#########################

  # guidance text
  observe({
    if (input$tabs == 6) {
      gtext$cur_comp <- "gtext_comp6.Rmd"
      if (input$enmSel == 'BIOCLIM') gtext$cur_mod <- "gtext_comp6_bioclim.Rmd"
      if (input$enmSel == 'Maxent') gtext$cur_mod <- "gtext_comp6_maxent.Rmd"
      # plots pts
      if (!is.null(values$df)) proxy %>% map_plotLocs(values$df)
      proxy %>% clearControls() %>% clearShapes()
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
      updateRadioButtons(session, 'visSel', "Modules Available:",
                         choices = list("BIOCLIM Envelope Plots" = 'bcEnvel',
                                        "Map Prediction" = 'map'))
    }
    # Module Maxent
     else if (input$enmSel == "Maxent") {
       comp6_maxentMod(input$rms, input$fcs)
       updateRadioButtons(session, 'visSel', "Modules Available:",
                          choices = list("Maxent Evaluation Plots" = 'mxEval',
                                         "Plot Response Curves" = 'response',
                                         "Map Prediction" = 'map'))
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
### COMPONENT 7 ####
#########################

  # guidance text
  observe({
    if (input$tabs == 7) {
      gtext$cur_comp <- "gtext_comp7.Rmd"
      if (input$visSel == 'map') {
        gtext$cur_mod <- "gtext_comp7_map.Rmd"
        updateTabsetPanel(session, 'main', selected = 'Map')
      } else {
        updateTabsetPanel(session, 'main', selected = 'Results')
      }
      if (input$visSel == 'response') gtext$cur_mod <- "gtext_comp7_respCurves.Rmd"
      if (input$visSel == 'bcEnvel') gtext$cur_mod <- "gtext_comp7_bcPlots.Rmd"
      if (input$visSel == 'mxEval') gtext$cur_mod <- "gtext_comp7_mxEvalPlots.Rmd"
      # plot pts
      if (!is.null(values$df)) proxy %>% map_plotLocs(values$df, clearImages=FALSE)
      proxy %>% clearControls() %>% clearShapes()
    }
  })

  observe({
    if (is.null(values$df)) return()
    if (input$tabs == 7) {
      proxy %>% removeControl('r2Legend')
      # if (!is.null(values$leg1)) {
      #   proxy %>% addLegend("topright", pal = values$leg1$pal, title = "Predicted Suitability",
      #                       values = values$leg1$rasVals, layerId = 'r1Legend', labFormat = reverseLabels(reverse_order=TRUE))
      # }
      proxy %>% showGroup('r1')
      proxy %>% hideGroup('r2')
    }
  })

  # function to make selectable list of model names
  modelSel <- function(inputName, resp=FALSE) {
    renderUI({
      if (is.null(values$evalPreds)) return()
      n <- names(values$evalPreds)
      if (!resp) {
        predNameList <- setNames(as.list(seq(1, length(n))), n)
      } else {
        predNameList <- setNames(as.list(n), n)
      }
      selectInput(inputName, label = "Choose a model",
                  choices = predNameList, selected = predNameList[[1]])
    })
  }

  # generates user selection of rasters to plot dynamically after they are created
  output$modelSelPlotStudyExt <- modelSel("modelSelPlotStudyExt")
  # model list for response curve model selection
  output$modelSelRespCurv <- modelSel("modelSelRespCurv", resp=TRUE)

  # generates list of predictor variables with non-zero coeffs for currently selected model
  output$predVarSel <- renderUI({
    if (is.null(values$evalPreds)) return()

      if(input$enmSel == "Maxent"){
        values$curModNum <- which(as.character(values$evalTbl[, 1]) == input$modelSelRespCurv)
        values$curMod <- values$evalMods[[values$curModNum]]
        nonZeroPreds <- mxNonzeroPreds(values$curMod)
        nonZeroPredNames <- names(values$predsMsk[[nonZeroPreds]])
        nonZeroPredNames <- nonZeroPredNames[order(as.integer(sub('bio', '', nonZeroPredNames)))]  # order by name
        predVarNameList <- setNames(as.list(nonZeroPredNames), nonZeroPredNames)
      } else {
        values$curModNum <- 1
        values$curMod <- values$evalMods[[values$curModNum]]
        predVarNameList <- names(values$curMod@presence)
      }

    radioButtons("predVarSel", "Choose a predictor variable:",
                 choices = predVarNameList, selected = predVarNameList[[1]])

  })

  # Module Plot Prediction
  observeEvent(input$plotPred, {
    comp7_mapPred(input$modelSelPlotStudyExt, input$predForm, input$predThresh, proxy)
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Map')
  })

  # Module Response Curves
  observe({
    if (is.null(input$visSel)) return()
    if (input$visSel != 'response') return()
    if (is.null(values$curMod)) return()
    output$respCurv <- renderPlot(response(values$curMod, var = input$predVarSel))
    values$respCurvParams <- list(mod=values$curModNum, var=input$predVarSel)
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
  })

  # Module BIOCLIM Envelope Plots
  output$bcEnvelPlot <- renderPlot({
    validate(need(values$evalMods[[1]], message = FALSE))
    values$bcEnvelPlot <- TRUE
    bc.plot(values$evalMods[[1]], a = input$bc1, b = input$bc2, p = input$bcProb)
    })

  # Module Maxent Evaluation Plots
  output$mxEvalPlot <- renderPlot({
    if (input$mxEvalSel == '') return()
    values$mxEvalPlot <- TRUE
    evalPlot(values$evalTbl, input$mxEvalSel)
    })

  # handle downloads for BIOCLIM env plots png
  output$downloadEnvPlot <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$origOccs), "_envelope_plot.png")},
    content = function(file) {
      png(file)
      plot(values$evalMods[[1]], a = input$bc1, b = input$bc2, p = input$bcProb)
      dev.off()
    }
  )

  # handle downloads for ENMeval plots png
  output$downloadEvalPlot <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$origOccs), "_enmeval_plot.png")},
    content = function(file) {
      png(file)
      evalPlot(values$evalTbl, input$mxEvalSel)
      dev.off()
    }
  )

  # handle downloads for response curve plot
  output$downloadRespPlot <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$origOccs), "_response_plot.png")},
    content = function(file) {
      png(file)
      response(values$curMod, var = input$predVarSel)
      dev.off()
    }
  )

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
### COMPONENT 8 ####
#########################

  # guidance text
  observe({
    if (input$tabs == 8) {
      gtext$cur_comp <- "gtext_comp8.Rmd"
      if (input$projSel == 'pjArea') gtext$cur_mod <- "gtext_comp8_pjarea.Rmd"
      if (input$projSel == 'pjTime') gtext$cur_mod <- "gtext_comp8_pjtime.Rmd"
      if (input$projSel == 'mess') gtext$cur_mod <- "gtext_comp8_mess.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
      # plot pts
      if (!is.null(values$df)) proxy %>% map_plotLocs(values$df, clearImages=FALSE)
      proxy %>% clearControls() %>% clearShapes()
    }
  })

  # functionality for drawing polygons on map
  observe({
    if (input$tabs == 8) {
      if (is.null(values$df)) return()
      proxy %>% removeControl('r1Legend')
      # if (!is.null(values$leg2)) {
      #   proxy %>% addLegend("topright", pal = values$leg2$pal, title = "Predicted Suitability",
      #                       values = values$leg2$rasVals, layerId = 'r2Legend', labFormat = reverseLabels(reverse_order=TRUE))
      # }
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

  # model list for comp8
  output$modelSelProj <- modelSel("modelSelProj")

  # select projection extent
  observeEvent(input$poly2Sel, {
    comp8_selProjExt()
  })

  # Module Project to New Area
  observeEvent(input$goPjArea, {
    comp8_pjArea(input$modelSelProj, input$predForm, values$enmSel)
  })

  observe({
    GCMnames <- c(AC="ACCESS1-0", BC="BCC-CSM1-1", CC="CCSM4", CE="CESM1-CAM5-1-FV2",
                  CN="CNRM-CM5", GF="GFDL-CM3", GD="GFDL-ESM2G", GS="GISS-E2-R",
                  HD="HadGEM2-AO", HG="HadGEM2-CC", HE="HadGEM2-ES", IN="INMCM4",
                  IP="IPSL-CM5A-LR", ME="MPI-ESM-P", MI="MIROC-ESM-CHEM", MR="MIROC-ESM",
                  MC="MIROC5", MP="MPI-ESM-LR", MG="MRI-CGCM3", NO="NorESM1-M")
    if (input$selTime == 'lgm') {
      selGCMchoices <- c('CC', 'MR', 'MC')
    } else if (input$selTime == 'mid') {
      selGCMchoices <- c("BC", "CC", "CE", "CN", "HG", "IP", "MR", "ME", "MG")
    } else {
      selGCMchoices <- c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD",
                         "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
    }
    names(selGCMchoices) <- GCMnames[selGCMchoices]
    selGCMchoices <- as.list(c("Select GCM" = "", selGCMchoices))
    output$selGCM <- renderUI({
      selectInput("selGCM", label = "Select global circulation model", choices = selGCMchoices)
    })
  })

  # Module Project to New Time
  observeEvent(input$goPjTime, {
    comp8_pjTime(input$modelSelProj, input$predForm, values$enmSel, input$bcRes, input$selRCP,
                 input$selGCM, input$selTime)
  })

  # Module MESS
  observeEvent(input$goMESS, {
    comp8_mess()
  })

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
      csvDataPathFix <- gsub('\\\\', '/', input$userCSV$datapath)
      if (!is.null(values$polyPts2)) {
        projAreaX <- printVecAsis(round(values$polyPts2[,1], digits=4))
        projAreaY <- printVecAsis(round(values$polyPts2[,2], digits=4))
      } else {
        projAreaX <- projAreaY <- NA
      }
      modSel <- as.numeric(input$modelSelProj)
      exp <- knitr::knit_expand(system.file("Rmd", 'userReport.Rmd', package = "wallace"), curWD=curWD, spName=input$spName, dbName=input$occDb, occNum=input$occNum, thinDist=input$thinDist,
                         occsCSV=csvDataPathFix, occsRemoved=printVecAsis(values$removedAll), occsSel=printVecAsis(values$ptSelID),
                         predsRes=input$bcRes, bcLat=values$bcLat, bcLon=values$bcLon, backgSel=input$backgSel, backgBuf=input$backgBuf, userBGname=input$userBackg$name,
                         userBGpath=input$userBackg$datapath, partSel=values$partSel2, aggFact=input$aggFact, kfoldsSel=input$kfolds,
                         enmSel=input$enmSel, rmsSel1=input$rms[1], rmsSel2=input$rms[2], rmsBy=input$rmsBy, fcsSel=printVecAsis(input$fcs),
                         mapPred=values$goMapPred, respCurvParamsMod=values$respCurvParams[[1]], respCurvParamsVar=values$respCurvParams[[2]], bcEnvelPlot=values$bcEnvelPlot,
                         bcPlot1=input$bc1, bcPlot2=input$bc2, bcPlotP=input$bcProb, mxEvalPlot=values$mxEvalPlot, mxEvalPlotSel=input$mxEvalSel,
                         projAreaX=projAreaX, projAreaY=projAreaY, modSel=modSel, selRCP=input$selRCP, selGCM=input$selGCM, selTime=input$selTime)
      writeLines(exp, 'userReport2.Rmd')

      if (input$mdType == 'Rmd') {
        out <- rmarkdown::render('userReport2.Rmd', rmarkdown::md_document(variant="markdown_github"))
        writeLines(gsub('``` r', '```{r}', readLines(out)), 'userReport3.Rmd')
        out <- 'userReport3.Rmd'
      } else {
        out <- rmarkdown::render('userReport2.Rmd', switch(
          input$mdType,
          PDF = rmarkdown::pdf_document(latex_engine='xelatex'), HTML = html_document(), Word = word_document()
        ))
      }
      file.rename(out, file)
    }
  )
})
