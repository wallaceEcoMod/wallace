change_time_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(

    ##inputs must be: SDM so step 1 remains
                      #Environmental variables (e g forest thorugh time as rasters (multiple))
                      #Threshold numeric input
                      #years used (numeric input?)
span("Step 1:", class = "step"),
 span("Choose Input raster", class = "stepText"), br(), br(),
selectInput(ns("selRasterSource") , label = "Select raster for calculations",
 choices = list("Wallace SDM" = "wallace",
"Projected SDM" = "proj",
"User SDM" = "sdm",
"Masked SDM" = "masked")),
 actionButton(ns("goInputRaster"), "Select"),

tags$hr(),
span("Step 2:", class = "step"),
span("Choose environmental variables", class = "stepText"), br(), br(),
fileInput(ns("changeEnvs"), label = "Upload environmental rasters",
          accept = c(".tif", ".asc"), multiple = TRUE),
numericInput(ns("EnvThrVal"), "Set threshold value",
             value = 0, min = 0),
actionButton(ns("goInputEnvs"), "Load"),

tags$hr(),
span("Step 3:", class = "step"),
span("Choose years", class = "stepText"), br(), br(),
textInput(ns("Years"), label = "Enter years to be used",
          placeholder = 'format: 2000, 2002',
          value = ""),
actionButton(ns("goInputYears"), "Calculate")
  )
}

change_time_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapProj <- common$mapProj

  observeEvent(input$goInputRaster, {
    if(input$selRasterSource == "wallace"){
      if (is.null(spp[[curSp()]]$visualization$mapPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Visualize your model before doing overlap calculations')
        return()
      }
      spp[[curSp()]]$change$time <- spp[[curSp()]]$visualization$mapPred
      logger %>% writeLog( "SDM area after masking for environmental variables through time will be calculated based on Wallace SDM ")
    }
    if(input$selRasterSource == "proj"){
      if (is.null(spp[[curSp()]]$project$mapProj)) {
        logger %>%
          writeLog(type = 'error',
                   'Project your model before doing overlap calculations')
        return()
      }
      spp[[curSp()]]$change$time <-  spp[[curSp()]]$project$mapProj
      logger %>% writeLog( "SDM area after masking for environmental variables through time will be calculated based on Projected SDM ")

    }
    if(input$selRasterSource == "sdm"){
      if (is.null(spp[[curSp()]]$postProc$OrigPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Load you model in component User SDM before doing range calculations')
        return()
      }
      spp[[curSp()]]$change$time <- spp[[curSp()]]$postProc$OrigPred
      logger %>% writeLog( "SDM area after masking for environmental variables through time will be calculated based on User provided SDM ")

    }
    if(input$selRasterSource == "masked"){
      #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
      #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly

      if (!is.null(spp[[curSp()]]$postProc$prediction)) {
        logger %>%
          writeLog(type = 'error',
                   'Do a maskRangeR analysis before doing range calculations')
        return()
      }
      spp[[curSp()]]$change$time <- spp[[curSp()]]$postProc$prediction
      logger %>% writeLog( "SDM area after masking for environmental variables through time will be calculated based on Masked SDM ")

    }
  })
  observeEvent(input$goInputEnvs, {
    if (is.null(input$changeEnvs)) {
      logger %>% writeLog(type = 'error', "Raster files not uploaded")
      return()
    }
    if (is.null(input$EnvThrVal)) {
      logger %>% writeLog(type = 'error', "Please enter a threshold for environmental layers")
      return()
    }
    smartProgress(
      logger,
      message = "Loading environmental variables ", {
   rStack <- raster::stack(input$changeEnvs$datapath)
   if (raster::nlayers(rStack)==1) {
     logger %>% writeLog(type = 'error', "Please upload more than one environmental variable")
     return()
   }
   spp[[curSp()]]$change$changeEnvs <- rStack
   spp[[curSp()]]$change$changeEnvsThr <-input$EnvThrVal
      })
  })

  observeEvent(input$goInputYears, {
    req(spp[[curSp()]]$change$changeEnvs)
    years <- trimws(strsplit(input$Years, ",")[[1]])
    if (raster::nlayers(spp[[curSp()]]$change$changeEnvs)!=length(years)) {
      logger %>% writeLog(type = 'error', "Please enter the years for all inputed variables")
      return()
    }
    smartProgress(
      logger,
      message = "Calculating area change through time ", {
    SDM <- spp[[curSp()]]$change$time
    rStack <- raster::projectRaster(spp[[curSp()]]$change$changeEnvs, SDM, method = 'bilinear')
    threshold <- spp[[curSp()]]$change$changeEnvsThr

    ##run function
    SDM.time <- changeRangeR::envChange(rStack = rStack, SDM = SDM, threshold = threshold)
    ### Set up years for plotting
      })


    req(area)
    logger %>% writeLog( "SDM area after masking for environmental variables through time calculation done")
    # LOAD INTO SPP ####
    spp[[curSp()]]$change$AreaTime <-SDM.time$Area
    spp[[curSp()]]$change$Years <- years
    common$update_component(tab = "Results")

 })


  output$TimeAreas <- renderUI({
    # Result
    output$areaMasked <- renderPrint({ paste(
        "SDM area after masking for environmental variables through time",
        spp[[curSp()]]$change$AreaTime) })
    output$timePlot <- renderPlot({
      plot(y = spp[[curSp()]]$change$AreaTime, x = spp[[curSp()]]$change$Years, main = "SDM area change", ylab = "area (square m)",xlab="Time")
      lines(y = spp[[curSp()]]$change$AreaTime, x = spp[[curSp()]]$change$Years)
    })

    tabsetPanel(
      tabPanel("Area through time plot",
               tagList(
                 plotOutput(session$ns('timePlot'))
               )),
      tabPanel("Area through time values",
               tagList(
                 verbatimTextOutput(session$ns('areaMasked'))
               ))
    )
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      list(
        selSource = input$selRasterSource,
        EnvThrVal = input$EnvThrVal,
        Years = input$Years)
    },
    load = function(state) {
      # Load
      updateSelectInput(session, 'selRasterSource', selected = state$selRasterSource)
      updateSelectInput(session, 'EnvThrVal', selected = state$EnvThrVal)
      updateSelectInput(session, 'Years', selected = state$Years)

    }
  ))

}

change_time_module_result <- function(id) {
ns <- NS(id)
# Result UI
uiOutput(ns("TimeAreas"))

}

change_time_module_map <- function(map, common) {
  # Map logic
  spp <- common$spp
  curSp <- common$curSp
  #plot SDM to use
  req(spp[[curSp()]]$change$time)
  sdm <-  spp[[curSp()]]$change$time
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

}

change_time_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    change_time_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  )
}

