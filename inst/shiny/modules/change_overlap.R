change_overlap_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Input raster", class = "stepText"), br(), br(),
    selectInput(ns("selSource") , label = "Select raster for calculations",
                choices = list("Wallace SDM" = "wallace",
                               "Projected SDM" = "proj",
                               "User SDM" = "sdm",
                               "Masked SDM" = "masked")),

    actionButton(ns("goInputRaster"), "Select"),
    tags$hr(),
    span("Step 2:", class = "step"),
    span("Choose Input Polygon", class = "stepText"), br(), br(),
    fileInput(ns("changeOverlapShp"), label = "Upload  'Upload polygon as shapefile (.shp, .shx, .dbf)",
              accept = c(".dbf", ".shx", ".shp"), multiple = TRUE),

    actionButton(ns("goInputPoly"), "Load shapefile"),

    tags$hr(),
    span("Step 3:", class = "step"),
    span("Choose field of interest", class = "stepText"), br(),
    #Add a conditional panel showing the fields in the shapefile how?
    uiOutput(ns('selFieldui')),
    uiOutput(ns('selCatdui')),
    actionButton(ns("goSelField"), "Overlap")
    #ADD this to be able to select category




  )
}

change_overlap_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapProj <- common$mapProj
  changeField <- common$changeField
  changeCategory <- common$changeCategory

  observeEvent(input$goInputRaster, {
    if(input$selSource == "wallace"){
      spp[[curSp()]]$change$Plot <- spp[[curSp()]]$visualization$mapPred
    }
    if(input$selSource == "proj"){
      spp[[curSp()]]$change$Plot <-  spp[[curSp()]]$project$mapProj
    }
    if(input$selSource == "sdm"){
      spp[[curSp()]]$change$Plot <- spp[[curSp()]]$postProc$OrigPred
    }
    if(input$selSource == "masked"){
      spp[[curSp()]]$change$Plot <- spp[[curSp()]]$postProc$prediction
    }
  })
 observeEvent(input$goInputPoly, {
   if (is.null(spp[[curSp()]]$postProc$prediction)) {
     logger %>% writeLog(
       type = 'error', hlSpp(curSp()), 'Calculate/Upload a model prediction (**)')
     return()
   }
   pathdir <- dirname(input$changeOverlapShp$datapath)
   pathfile <- basename(input$changeOverlapShp$datapath)
   # get extensions of all input files
   exts <- sapply(strsplit(input$changeOverlapShp$name, '\\.'), FUN = function(x) x[2])
   if ('shp' %in% exts) {
     if (length(exts) < 3) {
       logger %>%
         writeLog(type = 'error',
                  paste0('If entering a shapefile, please select all the ',
                         'following files: .shp, .shx, .dbf.'))
       return()
     }
     smartProgress(
       logger,
       message = "Uploading user provided shapefile ", {
     # get index of .shp
     i <- which(exts == 'shp')
     if (!file.exists(file.path(pathdir, input$changeOverlapShp$name)[i])) {
       file.rename(input$changeOverlapShp$datapath, file.path(pathdir, input$changeOverlapShp$name))
     }
     # read in shapefile and extract coords
     polyOverlap  <- rgdal::readOGR(file.path(pathdir, input$changeOverlapShp$name)[i])
     logger %>% writeLog( "User shapefile loaded ")
})
   } else {
     logger %>%
       writeLog(type = 'error',
                paste0('Please enter a ',
                       'shapefile (.shp, .shx, .dbf).'))
     return()
   }

    spp[[curSp()]]$change$polyOverlap <- polyOverlap

  })
  ###add this if we want to include field selection

  output$selFieldui <- renderUI({
    #add a conditional on providing a file
    req(curSp())
    if(!is.null(spp[[curSp()]]$change$polyOverlap)){
   fields <- colnames(spp[[curSp()]]$change$polyOverlap@data)
    }
    else {fields<-c("load shapefile first")}

  fields <- setNames(as.list(fields), fields)
    shinyWidgets::pickerInput("selField",
                              label = "Select field",
                              choices =   fields ,
                              multiple = FALSE,
                              selected =   fields )
  })

  output$selCatdui <- renderUI({
    #add a conditional on providing a file
    req(curSp(),req(spp[[curSp()]]$change$polyOverlap),req(changeField()))
    if(!is.null(changeField())){
      field <- changeField()
      category <- as.character(unique(spp[[curSp()]]$change$polyOverlap[[field]]))
      category <- c("All",category)
    }
    else {category<-c("load shapefile first")}
    category <- setNames(as.list(category), category)
    shinyWidgets::pickerInput("selCat",
                              label = "Select category",
                              choices = category,
                              multiple = TRUE,
                              selected = "All",
                              options = list(`actions-box` = TRUE))

  })


  observeEvent(input$goSelField,{
    spp[[curSp()]]$change$ShpCat <-   changeCategory()
    spp[[curSp()]]$change$ShpField <-    changeField()

    category<-changeCategory()
    if(input$selSource == "wallace"){
      if (is.null(spp[[curSp()]]$visualization$mapPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Visualize your model before doing overlap calculations')
        return()
      }
      smartProgress(
        logger,
        message = "Calculating range overlap ", {
      r = spp[[curSp()]]$visualization$mapPred
      shp = spp[[curSp()]]$change$polyOverlap
      raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
      raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      ratio.Overlap <- changeRangeR::ratioOverlap(r = r, shp = shp, field=spp[[curSp()]]$change$ShpField, category = category)
        })
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####

      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

    }

    if(input$selSource == "proj"){
      if (is.null(spp[[curSp()]]$project$mapProj)) {
        logger %>%
          writeLog(type = 'error',
                   'Project your model before doing overlap calculations')
        return()
      }
      smartProgress(
        logger,
        message = "Calculating range overlap ", {
      r = spp[[curSp()]]$project$mapProj
      shp = spp[[curSp()]]$change$polyOverlap
      raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
      raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      ratio.Overlap <- changeRangeR::ratioOverlap(r = r, shp = shp, field= spp[[curSp()]]$change$ShpField, category = category)
        })
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of projected range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

      common$update_component(tab = "Map")
    }


    if(input$selSource == "sdm"){
     if (is.null(spp[[curSp()]]$postProc$OrigPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Load you model in component User SDM before doing range calculations')
        return()
     }


      smartProgress(
        logger,
        message = "Calculating range overlap ", {
      r = spp[[curSp()]]$postProc$OrigPred
      shp = spp[[curSp()]]$change$polyOverlap
      raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
      raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp = shp, field = spp[[curSp()]]$change$ShpField, category = category)
      #ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp = shp, category = "All")
      })
    # LOAD INTO SPP ####
    req(ratio.Overlap)
    logger %>% writeLog( "Proportion of user provided range area that is contained by landcover categories calculated ")
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

}
    if(input$selSource == "masked"){
      #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
      #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly

      if (!is.null(spp[[curSp()]]$postProc$prediction)) {
        logger %>%
          writeLog(type = 'error',
                   'Do a maskRangeR analysis before doing range calculations')
        return()
      }

      smartProgress(
        logger,
        message = "Calculating range overlap ", {
      r = spp[[curSp()]]$postProc$prediction
      shp = spp[[curSp()]]$change$polyOverlap
      raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp =  shp,field = spp[[curSp()]]$change$ShpField, category = category)
        })
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of masked range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

    }

  })




  output$result <- renderText({
    # Result
    spp[[curSp()]]$change$overlapvalue

  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      list(
        changeRangeSel = input$changeRangeSel,
        selSource = input$selSource,
        selField = input$selField,
        selCat = input$selCat
      )
    },
    load = function(state) {
      # Load
      updateSelectInput(session, 'changeRangeSel', selected = state$changeRangeSel)
      updateSelectInput(session, ' selSource', selected = state$selSource)
     updateSelectInput(session, ' selField', selected = state$selField)
     updateSelectInput(session, ' selCat', selected = state$selCat)
    }
  ))

}

change_overlap_module_result <- function(id) {
ns <- NS(id)
# Result UI
verbatimTextOutput(ns("result"))
}

change_overlap_module_map <- function(map, common) {
  # Map logic
 spp <- common$spp
  curSp <- common$curSp
  #plot SDM to use
  req(spp[[curSp()]]$change$Plot)
  sdm <-  spp[[curSp()]]$change$Plot
  raster::crs(sdm) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
  SDMVals <- getRasterVals(sdm)
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  legendPal <- colorNumeric(rev(rasCols), SDMVals, na.color = 'transparent')
  rasPal <- colorNumeric(rasCols, SDMVals, na.color = 'transparent')
  zoomExt <- raster::extent(sdm)
  map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                    lat1 = zoomExt[3], lat2 = zoomExt[4])
  if (length(unique(SDMVals)) == 3 |
      length(unique(SDMVals)) == 2) {
    map %>%
      addLegend("bottomright", colors = c('gray', 'red'),
                title = "SDM",
                labels = c("Presence", "Absence"),
                opacity = 1, layerId = 'sdm') %>%
      addRasterImage(sdm, colors = c('gray', 'red'),
                     opacity = 0.7, group = 'change', layerId = 'sdm',
                     method = "ngb")
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), SDMVals, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, SDMVals, na.color = 'transparent')
    map %>%
      addLegend("bottomright", pal = legendPal, title = "SDM",
                values = SDMVals, layerId = "sdm",
                labFormat = reverseLabels(2, reverse_order=TRUE)) %>%
      addRasterImage(sdm, colors = rasPal,
                     opacity = 0.7, group = 'change', layerId = 'sdm',
                     method = "ngb")
  }
  # Add just projection Polygon
  req(spp[[curSp()]]$change$polyOverlap)
  #raster::crs(spp[[curSp()]]$change$polyOverlap) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
  polyOvXY <- spp[[curSp()]]$change$polyOverlap@polygons
  if(length(polyOvXY) == 1) {
    shp <- list(polyOvXY[[1]]@Polygons[[1]]@coords)
  } else {
    shp <- lapply(polyOvXY, function(x) x@Polygons[[1]]@coords)
  }
  bb <- spp[[curSp()]]$change$polyOverlap@bbox
  bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
  map %>%
  fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red", fill=FALSE, group = 'change')
  }


 ##Plot overlap
  req(spp[[curSp()]]$change$overlapRaster)

    Overlap <-  spp[[curSp()]]$change$overlapRaster
    raster::crs(Overlap) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
    OverlapVals <- spp[[curSp()]]$change$overlapvalues
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  legendPal <- colorNumeric(rev(rasCols), OverlapVals, na.color = 'transparent')
  rasPal <- colorNumeric(rasCols, OverlapVals, na.color = 'transparent')
  zoomExt <- raster::extent(Overlap)
  map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                    lat1 = zoomExt[3], lat2 = zoomExt[4])
  # Create legend
  map %>% clearAll()
  if (length(unique(OverlapVals)) == 3 |
      length(unique(OverlapVals)) == 2) {
    map %>%
      addLegend("bottomright", colors = c('gray', 'red'),
                title = "Range Overlap",
                labels = c("Presence", "Absence"),
                opacity = 1, layerId = 'expert') %>%
      addRasterImage(Overlap, colors = c('gray', 'red'),
                     opacity = 0.7, group = 'change', layerId = 'Overlap',
                     method = "ngb")
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), OverlapVals, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, OverlapVals, na.color = 'transparent')
    map %>%
      addLegend("bottomright", pal = legendPal, title = "Range Overlap",
                values = OverlapVals, layerId = "overlap",
                labFormat = reverseLabels(2, reverse_order=TRUE)) %>%
      addRasterImage(Overlap, colors = rasPal,
                     opacity = 0.7, group = 'change', layerId = 'Overlap',
                     method = "ngb")
  }


}
change_overlap_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    change_overlap_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  )
}

