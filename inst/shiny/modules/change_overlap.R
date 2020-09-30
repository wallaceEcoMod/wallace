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

    ##question for mary add option to do range for sdm that comes from maskRangeR or uploaded?
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
actionButton(ns("goSelField"), "Select"),
  )
}

change_overlap_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapProj <- common$mapProj


  observeEvent(input$goInputPoly, {
    polyOverlap <- proj_userExtent(input$changeOverlapShp$datapath, input$changeOverlapShp$name,
                                 0, logger, spN = curSp())
    spp[[curSp()]]$change$polyOverlap <- polyOverlap
  })


  output$selFieldui <- renderUI({
    ns <- session$ns
    #add a conditional on providing a file
    if(!is.null(spp[[curSp()]]$change$polyOverlap)){
    fields <- colnames(spp[[curSp()]]$change$polyOverlap@data)
    }
    else {fields<-c("load shapefile first")}
    fields <- as.list(c("Select Field" = "", fields))
    selectInput(ns("selField"), label = "Select field of interest",
                choices = fields)
    spp[[curSp()]]$change$FieldSel <- input$selField
  })
  observeEvent(input$goSelField, {
    if(input$selSource == "wallace"){
      if (is.null(spp[[curSp()]]$visualization$mapPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Visualize your model before doing overlap calculations')
        return()
      }
      ratio.Overlap <- changeRangeR::ratioOverlap(r = spp[[curSp()]]$visualization$mapPred, shp =  spp[[curSp()]]$change$polyOverlap, field = spp[[curSp()]]$change$FieldSel, category = "All")
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
    }
    if(input$selSource == "proj"){
      if (is.null(spp[[curSp()]]$project$mapProj)) {
        logger %>%
          writeLog(type = 'error',
                   'Project your model before doing overlap calculations')
        return()
      }
      ratio.Overlap <- changeRangeR::ratioOverlap(r = spp[[curSp()]]$project$mapProj, shp =  spp[[curSp()]]$change$polyOverlap, field = spp[[curSp()]]$change$FieldSel, category = "All")
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of projected range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
      common$update_component(tab = "Map")
    }
    if(input$selSource == "sdm"){
      if (is.null(spp[[curSp()]]$postProc$OrigPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Load you model in component User SDM before doing range calculations')
        return()
      }
      ratio.Overlap <- changeRangeR::ratioOverlap(r = spp[[curSp()]]$postProc$OrigPred, shp =  spp[[curSp()]]$change$polyOverlap, field = spp[[curSp()]]$change$FieldSel, category = "All")
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of user provided range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
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
      ratio.Overlap <- changeRangeR::ratioOverlap(r = spp[[curSp()]]$postProc$prediction, shp =  spp[[curSp()]]$change$polyOverlap, field = spp[[curSp()]]$change$FieldSel, category = "All")
      req(ratio.Overlap)
      logger %>% writeLog( "Proportion of masked range area that is contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$change$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$change$overlapvalue <- ratio.Overlap$ratio
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
        selSource = input$selSource)
    },
    load = function(state) {
      # Load
      updateSelectInput(session, 'changeRangeSel', selected = state$changeRangeSel)
      updateSelectInput(session, ' selSource', selected = state$selSource)
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
  #set map parameters

  if(!is.null( spp[[curSp()]]$change$overlapRaster)){
    Overlap <-  spp[[curSp()]]$change$overlapRaster
  OverlapVals <- getRasterVals(Overlap)
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  legendPal <- colorNumeric(rev(rasCols), OverlapVals, na.color = 'transparent')
  rasPal <- colorNumeric(rasCols, OverlapVals, na.color = 'transparent')
  # Create legend
  map %>% clearAll() %>%
    addLegend("bottomright", pal = legendPal,
              title = "Range Overlap",
              values = OverlapVals, layerId = "train",
              labFormat = reverseLabels(2, reverse_order = TRUE))
  #MAP eoverlap
  map %>% addRasterImage(Overlap, colors = rasPal, opacity = 0.7,
                         layerId = 'Overlap', group = 'change', method = "ngb")
###Add polygon intersect
  # Add just projection Polygon
  req(spp[[curSp()]]$change$polyOverlap)
  polyOvXY <- spp[[curSp()]]$change$polyOverlap@polygons[[1]]@Polygons
  if(length(polyOvXY) == 1) {
    shp <- list(polyOvXY[[1]]@coords)
  } else {
    shp <- lapply(polyOvXY, function(x) x@coords)
  }
  bb <- spp[[curSp()]]$project$pjExt@bbox
  bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
  map %>%
    fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red",group = 'overlap')
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

