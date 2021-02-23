change_overlap_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Input range map", class = "stepText"), br(), br(),
    selectInput(ns("selSource") , label = "Select raster for calculations",
                choices = list("Wallace SDM" = "wallace",
                               "Projected SDM" = "proj",
                               "User SDM" = "sdm",
                               "Masked SDM" = "masked",
                               "EOO" = "eoo",
                                "AOO" = "aoo"
                               )),

    actionButton(ns("goInputRaster"), "Select"),
    tags$hr(),
    span("Step 2:", class = "step"),
    span("Choose Overlap Map", class = "stepText"), br(), br(),
    selectInput(ns('changeOverlap'), label = "Select type",
                choices = list("Shapefile" = 'shapefile',
                               "Raster" = 'raster')),
    conditionalPanel(sprintf("input['%s'] == 'shapefile'", ns("changeOverlap")),
                     fileInput(ns("changeOverlapShp"),
                               label = 'Upload polygon in shapefile (.shp, .shx, .dbf) format',
                               accept = c(".dbf", ".shx", ".shp"), multiple = TRUE),
                     ),
    conditionalPanel(sprintf("input['%s'] == 'raster'", ns("changeOverlap")),
                     fileInput(ns("changeOverlapRaster"), label = "Upload raster file to overlap",
                               accept = c(".tif", ".asc"))
                     ),
    actionButton(ns("goInputOver"), "Load"), br(),
    tags$hr(),
    span("Step 3:", class = "step"),
    span("Choose field of interest (if input is a shapefile, else go to step 4)", class = "stepText"), br(),


      #Add a conditional panel showing the fields in the shapefile
     uiOutput(ns('selFieldui')),
    uiOutput(ns('selCatdui')),
      checkboxInput(ns("doSubfield"),
                    label = 'Results per subfield',
                    value = FALSE),
    actionButton(ns("goSelField"), "Select"), br(),
    #ADD this to be able to select category
    span("Step 4:", class = "step"),
    span("Do range overlap", class = "stepText"), br(),
    actionButton(ns("goOverlap"), "Overlap"), br(),
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
      if (is.null(spp[[curSp()]]$visualization$mapPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Visualize your model before doing overlap calculations')
        return()
      }
      spp[[curSp()]]$change$Plot <- spp[[curSp()]]$visualization$mapPred
    }
    if(input$selSource == "proj"){
      if (is.null(spp[[curSp()]]$project$mapProj)) {
        logger %>%
          writeLog(type = 'error',
                   'Project your model before doing overlap calculations')
        return()
      }
      spp[[curSp()]]$change$Plot <-  spp[[curSp()]]$project$mapProj
    }
    if(input$selSource == "sdm"){
      if (is.null(spp[[curSp()]]$postProc$OrigPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Load you model in component User SDM before doing range calculations')
        return()
      }
      spp[[curSp()]]$change$Plot <- spp[[curSp()]]$postProc$OrigPred
    }
    if(input$selSource == "masked"){

      if (is.null(    spp[[curSp()]]$mask$prediction)) {
        logger %>%
          writeLog(type = 'error',
                   'Do a maskRangeR analysis before doing range calculations')
        return()
      }
      spp[[curSp()]]$change$Plot <- spp[[curSp()]]$postProc$prediction
    }
    if(input$selSource == "eoo"){

      if (is.null(spp[[curSp()]]$rmm$data$change$EOO)) {
        logger %>%
          writeLog(type = 'error',
                   'Do an EOO calculation in the area module before doing range calculations')
        return()
      }
      spp[[curSp()]]$change$Plot1 <-   spp[[curSp()]]$rmm$data$change$EOO
    }
    if(input$selSource == "aoo"){
      #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
      #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly

      if (is.null(spp[[curSp()]]$rmm$data$change$AOO)) {
        logger %>%
          writeLog(type = 'error',
                   'Do an AOO calculation in the area module before doing range calculations')
        return()
      }
      spp[[curSp()]]$change$Plot <-   spp[[curSp()]]$rmm$data$change$AOO
    }

  })

 observeEvent(input$goInputOver, {
   if (is.null(spp[[curSp()]]$postProc$prediction)) {
     logger %>% writeLog(
       type = 'error', hlSpp(curSp()), 'Calculate/Upload a model prediction (**)')
     return()
   }
   if(input$changeOverlap=='shapefile'){
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


    shpcrop<-rgeos::gBuffer(polyOverlap,byid = TRUE, width=0)
   ##crop polygon for visualization if range is a raster
     if(!is.null(spp[[curSp()]]$change$Plot)){
    shpcrop<-raster::crop(shpcrop,raster::extent(spp[[curSp()]]$change$Plot))}

    spp[[curSp()]]$change$polyOverlap <- polyOverlap
    spp[[curSp()]]$change$polyOverlapCrop <- shpcrop
   }
   if(input$changeOverlap=='raster'){
     userRaster <- change_raster(rasPath = input$changeOverlapRaster$datapath,
                              rasName = input$changeOverlapRaster$name,
                              logger)
     if (!is.null(userRaster)) {
       logger %>% writeLog("User raster file loaded ")
     }
     spp[[curSp()]]$change$RasOverlap <- userRaster$sdm

     }

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


  observeEvent(input$goOverlap,{
    ##condition on which overlap if shp do this if raster then not , stored variables would be different though
    if(input$changeOverlap=='shapefile'){
    spp[[curSp()]]$change$ShpCat <-   changeCategory()
    spp[[curSp()]]$change$ShpField <-    changeField()
    spp[[curSp()]]$change$subfield <- input$doSubfield
    category<-changeCategory()
    shp = spp[[curSp()]]$change$polyOverlap
    }
    else if(input$changeOverlap=='raster'){
      shp = spp[[curSp()]]$change$RasOverlap
      spp[[curSp()]]$change$ShpCat <-   NULL
      spp[[curSp()]]$change$ShpField <-    NULL
      category<-NULL
      spp[[curSp()]]$change$subfield <- FALSE
    }
    if(input$selSource == "wallace"){

      smartProgress(
        logger,
        message = "Calculating range overlap ", {
      r = spp[[curSp()]]$visualization$mapPred
      #shp = spp[[curSp()]]$change$polyOverlap
      raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
      raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      ratio.Overlap <- changeRangeR::ratioOverlap(r = r, shp = shp, field=spp[[curSp()]]$change$ShpField, category = category,subfield=    spp[[curSp()]]$change$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####

      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      if(is.list(ratio.Overlap$maskedRange)){
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE

        ratio.Overlap$maskedRange <- do.call(raster::mosaic, ratio.Overlap$maskedRange)
      }
      spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)


    }

    if(input$selSource == "proj"){

      smartProgress(
        logger,
        message = "Calculating range overlap ", {
      r = spp[[curSp()]]$project$mapProj
     # shp = spp[[curSp()]]$change$polyOverlap
      raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
      raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      ratio.Overlap <- changeRangeR::ratioOverlap(r = r, shp = shp, field= spp[[curSp()]]$change$ShpField, category = category,   subfield= spp[[curSp()]]$change$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of projected range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      if(is.list(ratio.Overlap$maskedRange)){
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE

        ratio.Overlap$maskedRange <- do.call(raster::mosaic, ratio.Overlap$maskedRange)
      }
      spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

      common$update_component(tab = "Map")
    }


    if(input$selSource == "sdm"){

      smartProgress(
        logger,
        message = "Calculating range overlap ", {
      r = spp[[curSp()]]$postProc$OrigPred
    #  shp = spp[[curSp()]]$change$polyOverlap
      raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
      raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp = shp, field = spp[[curSp()]]$change$ShpField, category = category,   subfield= spp[[curSp()]]$change$subfield)
      #ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp = shp, category = "All")
      })
    # LOAD INTO SPP ####
    req(ratio.Overlap)
    logger %>% writeLog( "Proportion of user provided range area that is contained by landcover categories calculated ")
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      if(is.list(ratio.Overlap$maskedRange)){
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE

        ratio.Overlap$maskedRange <- do.call(raster::mosaic, ratio.Overlap$maskedRange)
      }
      spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

}
    if(input$selSource == "masked"){
      #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
      #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly

        smartProgress(
        logger,
        message = "Calculating range overlap ", {
      r =     spp[[curSp()]]$mask$prediction
    #  shp = spp[[curSp()]]$change$polyOverlap
      raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp =  shp,field = spp[[curSp()]]$change$ShpField, category = category,   subfield=  spp[[curSp()]]$change$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of masked range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      if(is.list(ratio.Overlap$maskedRange)){
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE

        ratio.Overlap$maskedRange <- do.call(raster::mosaic, ratio.Overlap$maskedRange)
      }
      spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

    }

    if(input$selSource == "eoo"){

      smartProgress(
        logger,
        message = "Calculating range overlap ", {
          r = spp[[curSp()]]$rmm$data$change$EOO
    #      shp = spp[[curSp()]]$change$polyOverlap
          raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp =  shp,field = spp[[curSp()]]$change$ShpField, category = category,   subfield= spp[[curSp()]]$change$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of EOO that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapPoly <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
     # spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

    }
    if(input$selSource == "aoo"){

      smartProgress(
        logger,
        message = "Calculating range overlap ", {
          r = spp[[curSp()]]$rmm$data$change$AOO
     #     shp = spp[[curSp()]]$change$polyOverlap
          raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp =  shp,field = spp[[curSp()]]$change$ShpField, category = category,subfield=    spp[[curSp()]]$change$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog("Proportion of AOO that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      if(is.list(ratio.Overlap$maskedRange)){
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE

        ratio.Overlap$maskedRange <- do.call(raster::mosaic, ratio.Overlap$maskedRange)
      }
      spp[[curSp()]]$change$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

    }

  })




  output$result <- renderText({
    # Result
 #   spp[[curSp()]]$change$overlapvalue)
  gsub("%","%\n",  spp[[curSp()]]$change$overlapvalue)

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
  map %>% clearAll()
  #if EOO is selected plot the polygon
  if(!is.null(spp[[curSp()]]$change$Plot1)){

    polyEOO <- spp[[curSp()]]$rmm$data$change$EOO@polygons[[1]]@Polygons
    bb <- spp[[curSp()]]$rmm$data$change$EOO@bbox
    bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
    map %>%
      fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
    map %>%
      ##Add legend
      addLegend("bottomright", colors = "gray",
                title = "EOO", labels = "EOO",
                opacity = 1)
    ##ADD polygon
    if (length(polyEOO) == 1) {
      xy <- list(polyEOO[[1]]@coords)
    } else {
      xy <- lapply(polyEOO, function(x) x@coords)
    }
    for (shp in xy) {
      map %>%
        addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "gray",
                    group = 'change')
    }
  }
  #plot SDM to use

  if(is.null(spp[[curSp()]]$change$Plot1)){
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
      addLegend("bottomright", colors = c('red', 'grey'),
                title = "SDM",
                labels = c("Presence", "Absence"),
                opacity = 1, layerId = 'sdm') %>%
      addRasterImage(sdm, colors = c('grey', 'red'),
                     opacity = 0.7, group = 'change', layerId = 'sdm',
                     method = "ngb")
  }
  else if (length(unique(SDMVals)) == 1) {
    map %>%
      addLegend("bottomright", colors = 'red',
                title = "AOO",
                labels = "Presence",
                opacity = 1, layerId = 'expert') %>%
      addRasterImage(sdm, colors = 'red',
                     opacity = 0.7, group = 'change', layerId = 'Overlap',
                     method = "ngb")
  }
  else {
    # if no threshold specified
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
  }
  # Add just projection Polygon
  req(spp[[curSp()]]$change$polyOverlapCrop)
  #raster::crs(spp[[curSp()]]$change$polyOverlap) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
  polyOvXY <- spp[[curSp()]]$change$polyOverlapCrop@polygons
  if(length(polyOvXY) == 1) {
    shp <- list(polyOvXY[[1]]@Polygons[[1]]@coords)
  } else {
    shp <- lapply(polyOvXY, function(x) x@Polygons[[1]]@coords)
  }
  bb <- spp[[curSp()]]$change$polyOverlapCrop@bbox
  bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
  map %>%
  fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red", fill=FALSE, group = 'change')
  }

##Plot overlap of polygons (EOO case)
  if(!is.null(spp[[curSp()]]$change$overlapPoly)){
 req(spp[[curSp()]]$change$overlapPoly)
    polyOver <- as_Spatial(spp[[curSp()]]$change$overlapPoly)
    bb <- polyOver@bbox
 polyOver <- polyOver@polygons[[1]]@Polygons

 bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
 map %>%
   fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
 map %>%
   ##Add legend
   addLegend("bottomright", colors = "red",
             title = "Overlap", labels = "Overlap",
             opacity = 1)
 ##ADD polygon
 if (length(polyOver) == 1) {
   xy <- list(polyOver[[1]]@coords)
 } else {
   xy <- lapply(polyOver, function(x) x@coords)
 }
 for (shp in xy) {
   map %>%
     addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "red",
                 group = 'change')
 }
  }
##Plot overlap of raster vs raster (code to get unclear)
##Plot overlap of poly and raster (SDM vs. polygon case)
  if(!is.null(spp[[curSp()]]$change$overlapRaster)){
 req(spp[[curSp()]]$change$overlapRaster)

    Overlap <-  spp[[curSp()]]$change$overlapRaster
    if(is.list(Overlap)){
    Overlap$fun <- mean
    Overlap$na.rm <- TRUE

    Overlap <- do.call(raster::mosaic, Overlap)
    }

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
      addLegend("bottomright", colors = c('red', 'grey'),
                title = "Range Overlap",
                labels = c("Presence", "Absence"),
                opacity = 1, layerId = 'expert') %>%
      addRasterImage(Overlap, colors = c('gray', 'red'),
                     opacity = 0.7, group = 'change', layerId = 'Overlap',
                     method = "ngb")
  }
  else if (length(unique(OverlapVals)) == 1) {
      map %>%
        addLegend("bottomright", colors = 'red',
                  title = "Range Overlap",
                  labels = "Presence",
                  opacity = 1, layerId = 'expert') %>%
        addRasterImage(Overlap, colors = 'red',
                       opacity = 0.7, group = 'change', layerId = 'Overlap',
                       method = "ngb")
  }
  else {
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

