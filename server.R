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
values <- reactiveValues(polyID=0, polyErase=FALSE, log=c())

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
  shinyjs::disable("downloadGBIFcsv")
  shinyjs::disable("downloadThincsv")
  shinyjs::disable("predDnld")
  shinyjs::disable("downloadMskPreds")
  shinyjs::disable("downloadPart")
  shinyjs::disable("downloadEvalcsv")
  shinyjs::disable("downloadEvalPlots")
  shinyjs::disable("downloadPred")
  
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
    comp1_gbifOcc(input$gbifName, input$gbifNum)
    shinyjs::enable("downloadGBIFcsv")
  })

  # module userOccs
  observe({
    if (is.null(input$userCSV)) return()  # exit if userCSV not specifed
    comp1_mod_userOcc(input$userCSV$datapath)
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
  
  # handle downloading of GBIF csv
  output$downloadGBIFcsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$df), "_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$gbifOrig$data, file, row.names=FALSE)
    }
  )
  
  #########################
  ### MAPPING FUNCTIONALITY
  #########################
  
  # map gbif occs
  observeEvent(input$goName, {
    if (is.null(values$gbifoccs)) {return()}
    proxy %>% clearShapes()
    lati <- values$gbifoccs[,3]
    longi <- values$gbifoccs[,2]
    z <- smartZoom(longi, lati)
    proxy %>% fitBounds(z[1], z[2], z[3], z[4])

    # this section makes letter icons for occs based on basisOfRecord
    #     occIcons <- makeOccIcons()
    #     iconList <- list(HUMAN_OBSERVATION=1, OBSERVATION=2, PRESERVED_SPECIMEN=3,
    #                      UNKNOWN_EVIDENCE=4, FOSSIL_SPECIMEN=5, MACHINE_OBSERVATION=6,
    #                      LIVING_SPECIMEN=7, LITERATURE_OCCURRENCE=8, MATERIAL_SAMPLE=9)
    #     values$gbifoccs$basisNum <- unlist(iconList[values$gbifoccs$basisOfRecord])
    #     proxy %>% addMarkers(data = values$gbifoccs, lat = ~latitude, lng = ~longitude,
    #                          layerId = as.numeric(rownames(values$gbifoccs)),
    #                          icon = ~icons(occIcons[basisNum]))
  })

  # governs point removal behavior and modifies tables in "values"
  observeEvent(input$remove, {
    if (!is.null(values$ptsSel)) {
      writeLog('NOTICE: Remove localities by ID before selecting with polygons. Press "Reset Localities" to start over.')
      return()}
    comp2_selLocMap_remLocs(input$remLoc)
  })

  # erase select localities polygon with button click
  observeEvent(input$erasePolySelLocs, {
    values$ptsSel <- NULL
    values$drawPolys <- NULL
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    x <- paste('* RESET: localities dataset is now back to', nrow(values$gbifoccs), 'records.')
    isolate(writeLog(x))
    if (!is.null(values$gbifoccs)) {
      values$gbifoccs <- rbind(values$gbifoccs, values$removed)
      values$df <- values$gbifoccs
    }
    lati <- values$df[,3]
    longi <- values$df[,2]
    z <- smartZoom(longi, lati)
    proxy %>% fitBounds(z[1], z[2], z[3], z[4])
  })

  # behavior for plotting points and their colors based on which tab is active
  observe({
    if (is.null(values$df)) return()
    if (input$tabs == 1) {  # if tab1, just plot occurrence localities
      map_plotLocs(values$gbifoccs)
    }

    if (input$tabs == 2) {  # if tab2
      if (is.null(input$procOccSel)) return()
      if (input$procOccSel == "selpts") {  #  and Module Select Localities, make selected pts yellow and add legend
        if (is.null(values$prethinned)) {
          map_plotLocs(values$gbifoccs, clearShapes=FALSE)
          if (!is.null(values$ptsSel)) {
            proxy %>% addCircleMarkers(data = values$ptsSel, lat = ~latitude, lng = ~longitude,
                                       radius = 5, color = 'red',
                                       fill = TRUE, fillColor = 'yellow',
                                       weight = 2, popup = ~pop, fillOpacity=1)
            proxy %>% addLegend("topright", colors = c('red','yellow'),
                                title = "GBIF Records", labels = c('original', 'selected'),
                                opacity = 1, layerId = 1)
          } else {
            map_plotLocs(values$df)
          }
        } else {
          map_plotLocs(values$df, clearShapes=FALSE)
        }

        # draw all user-drawn polygons and color according to colorBrewer
        if (!is.null(values$drawPolys)) {
          curPolys <- values$drawPolys@polygons
          numPolys <- length(curPolys)
          colors <- brewer.pal(numPolys, 'Accent')
          for (i in numPolys) {
            curPoly <- curPolys[i][[1]]@Polygons[[1]]@coords
            proxy %>% addPolygons(curPoly[,1], curPoly[,2],
                                  weight=3, color=colors[i])
          }
        }
      }

      if (input$procOccSel == "spthin") {
        proxy %>% clearMarkers()
        proxy %>% clearShapes()
        proxy %>% clearImages()
        lati <- values$df[,3]
        longi <- values$df[,2]
        z <- smartZoom(longi, lati)
        proxy %>% fitBounds(z[1], z[2], z[3], z[4])
        proxy %>% addCircleMarkers(data = values$df, lat = ~latitude, lng = ~longitude,
                                   radius = 5, color = 'red',
                                   fill = TRUE, fillColor = 'red', weight = 2, popup = ~pop)
        if (!is.null(values$prethinned)) {
          values$drawPolys <- NULL
          lati <- values$prethinned[,3]
          longi <- values$prethinned[,2]
          
          z <- smartZoom(longi, lati)
          proxy %>% fitBounds(z[1], z[2], z[3], z[4])
          proxy %>% addCircleMarkers(data = values$prethinned, lat = ~latitude, lng = ~longitude,
                                     radius = 5, color = 'red', fillOpacity = 1,
                                     fill = TRUE, fillColor = 'blue', weight = 2, popup = ~pop)
          proxy %>% addCircleMarkers(data = values$df, lat = ~latitude, lng = ~longitude,
                                     radius = 5, color = 'red',
                                     fill = TRUE, fillColor = 'red',
                                     fillOpacity = 1, weight = 2, popup = ~pop)
          proxy %>% addLegend("topright", colors = c('red', 'blue'),
                              title = "GBIF Records", labels = c('retained', 'removed'),
                              opacity = 1, layerId = 1)
        }
      }
    }

    if (input$tabs == 3 | input$tabs == 4 | input$tabs == 5) {
      proxy %>% clearMarkers()
      proxy %>% clearShapes()
      proxy %>% clearImages()
      proxy %>% addCircleMarkers(data = values$df, lat = ~latitude, lng = ~longitude,
                                 radius = 5, color = 'red',
                                 fill = TRUE, fillColor = 'red', weight = 2, popup = ~pop)
      proxy %>% addLegend("topright", colors = c('red'),
                          title = "GBIF Records", labels = c('retained'),
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
      proxy %>% addCircleMarkers(data = values$df, lat = ~latitude, lng = ~longitude,
                                 radius = 5, color = 'black',
                                 fill = TRUE, weight = 4, popup = ~pop)
      proxy %>% addLegend("topright", colors = c('black'),
                          title = "GBIF Records", labels = c('retained'),
                          opacity = 1, layerId = 1)
    }
  })
  
  #########################
  ### COMPONENT 2 FUNCTIONALITY
  #########################

  # functionality for drawing polygons on map
  observe({
    if (input$tabs == 2 && input$procOccSel == "selpts") {
      map_drawPolys(input$map_click, component = 2)
    }
  })
  
  # Module Select Localities: select points intersecting drawn polygons (replace values$df)
  observeEvent(input$selectPoly, {
    comp2_selLocMap_selIntLocs()
  })
  
  # Module Spatial Thin
  observeEvent(input$goThin, {
    comp2_spThin(input$thinDist)
    shinyjs::enable("downloadThincsv")
  })

  # handle download for thinned records csv
  output$downloadThincsv <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_thinned_gbifCleaned.csv")},
    content = function(file) {
      write.csv(values$df[,1:9], file, row.names = FALSE)
    }
  )

  #########################
  ### COMPONENT 3 FUNCTIONALITY
  #########################
  
  observe({if (input$bcRes != "") shinyjs::enable("predDnld")})

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
    print(input$backgBuf)
    if (is.null(input$backgSelect) | is.null(values$preds) | is.na(input$backgBuf)) return()
    comp4_studyReg(input$backgBuf, input$backgSelect)
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
  
  # update child radio buttons for selection of either spatial or non-spatial partitions
  observe({
    if (!is.null(input$partSelect)) {
      if (input$partSelect == 'nsp') {
        updateRadioButtons(session, 'partSelect2', choices = list("Jackknife (k = n)" = "jack", "Random k-fold" = "random"))
      } else if (input$partSelect == 'sp') {
        updateRadioButtons(session, 'partSelect2', choices = list("Block (k = 4)" = "block",
                                                                  "Checkerboard 1 (k = 2)" = "cb1",
                                                                  "Checkerboard 2 (k = 4)" = "cb2"))
      }
    }
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
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_partitioned_occs.csv")},
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
      output$bcEnvelPlot <- renderPlot(plot(values$evalMods, a = input$bc1, b = input$bc2, p = input$bcProb))
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
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_enmeval_results.csv")},
    content = function(file) {
      write.csv(values$evalTbl, file, row.names = FALSE)
    }
  )

  #########################
  ### COMPONENT 7 FUNCTIONALITY
  #########################

  # generates user selection of rasters to plot dynamically after they are created
  output$predictionSel1 <- renderUI({
    if (is.null(values$evalPreds)) return()
    n <- names(values$evalPreds)
    predNameList <- setNames(as.list(seq(1, length(n))), n)
    selectInput("predictionSel1", label = "Choose a model",
                choices = predNameList)
  })
  
  # copy of above for response curve model selection
  output$predictionSel2 <- renderUI({
    if (is.null(values$evalPreds)) return()
    n <- names(values$evalPreds)
    predNameList <- setNames(as.list(n), n)
    selectInput("predictionSel2", label = "Choose a model",
                choices = predNameList, selected = predNameList[[1]])
  })
  
  # generates list of predictor variables with non-zero coeffs for currently selected model
  output$predVarSel <- renderUI({
    if (is.null(values$evalPreds)) return()
    values$curMod <- values$evalMods[[which(as.character(values$evalTbl[, 1]) == input$predictionSel2)]]
    nonZeroPreds <- mxNonzeroPreds(values$curMod)  # need function from pete's ms
    nonZeroPredNames <- names(values$predsMsk[[nonZeroPreds]])
    nonZeroPredNames <- nonZeroPredNames[order(as.integer(sub('bio', '', nonZeroPredNames)))]  # order by name
    predVarNameList <- setNames(as.list(nonZeroPredNames), nonZeroPredNames)
    radioButtons("predVarSel", "Choose a predictor variable:",
                 choices = predVarNameList, selected = predVarNameList[[1]])
  })
  
  # handle downloads for ENMeval plots png
  output$downloadEvalPlots <- downloadHandler(
    filename = function() {paste0(nameAbbr(values$gbifoccs), "_enmeval_plots.png")},
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
    comp7_mapPred(input$predictionSel1, input$predForm, input$predThresh, proxy)
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
      map_drawPolys(input$map_click, component = 8)
    }
  })
  
  # erase select localities polygon with button click
  observeEvent(input$erasePolyProjExt, {
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    values$projExtPoly <- NULL
    proxy %>% removeShape("drawPolyProjExt")
    writeLog('* RESET PROJECTION EXTENT')

  })
  
  # Module Select Localities: select points intersecting drawn polygons (replace values$df)
  observeEvent(input$projExtSel, {
    comp8_selProjArea()
  })
  
  observe(print(values$projAreaExt))

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
                         userBGpath=input$userBackg$datapath, partSel=input$partSelect2, aggFact=input$aggFact, kfoldsSel=input$kfolds, 
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
