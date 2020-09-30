change_range_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("changeRangeSel"), "Options Available:",
                choices = list("None selected" = '',
                               "Range size" = "range",
                               "EOO" = "eoo",
                               "AOO" = "aoo"
                               )), # Check default (no selected)
    conditionalPanel(sprintf("input['%1$s'] == 'range'",
                             ns("changeRangeSel")),
                     selectInput(ns("selSource") , label = "Select source for calculations",
                                 choices = list("Wallace SDM" = "wallace",
                                                "User SDM" = "user",
                                                "Masked SDM" = "mask"))),
    conditionalPanel(sprintf("input['%1$s'] == 'eoo'",
                             ns("changeRangeSel")),
                     selectInput(ns("selSource1") , label = "Select source for calculations",
                                  choices = list("Occurrences" = "occs",
                                                 "SDM" = "sdm"))),

    conditionalPanel(sprintf("input['%1$s'] == 'aoo'",
                             ns("changeRangeSel")),
                     selectInput(ns("selSource2") , label = "Select source for calculations",
                                 choices = list("Occurrences" = "occs",
                                                "SDM" = "sdm",
                                                "Masked SDM" = "mask"))),
##question for mary add option to do range for sdm that comes from maskRangeR or uploaded?
    actionButton(ns("goRange"), "Calculate")

  )
}

change_range_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapProj <- common$mapProj



  observeEvent(input$goRange, {
    # WARNING ####

    #Processing
    if(input$changeRangeSel == "range"){
      if(input$selSource == "wallace"){
      # ERRORS ####
      if (is.null(spp[[curSp()]]$project$mapProj)) {
        logger %>%
          writeLog(type = 'error',
                   'Project your model before doing range calculations')
        return()
      }
      if (is.null(spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet)) {
        logger %>%
          writeLog(type = 'error',
                   'Generate a thresholded prediction before doing range calculations')
        return()
      }
    smartProgress(
      logger,
      message = "Calculating a range size estimate using a South America Albers Equal Area Conic projection", {

        # FUNCTION CALL ####
        ##First project to equal area
        p <- spp[[curSp()]]$project$mapProj

        ##PROJECT
        p<-raster::projectRaster(p,crs="+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m")
        # find the number of cells that are not NA
        pCells <- raster::ncell(p[!is.na(p)])
        # Convert the raster resolution to km^s
        Resolution <- (res(p)/1000)^2
        # Multiply the two
        area <- pCells * Resolution

})

    req(area)
    logger %>% writeLog( "Species range size calculated based on Wallace SDM ")
    # LOAD INTO SPP ####
      spp[[curSp()]]$change$range <- area
    spp[[curSp()]]$change$rangetype <- input$selSource
    common$update_component(tab = "Results")
      }
      if(input$selSource == "user"){
        # ERRORS ####
        if (!is.null(spp[[curSp()]]$postProc$OrigPred)) {
          logger %>%
            writeLog(type = 'error',
                     'Load you model in component User SDM before doing range calculations')
          return()
        }
        p <- spp[[curSp()]]$postProc$OrigPred
        p[p == 0] <- NA
        if (length(unique(values(p)))> 2) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing range calculations')
          return()
        }
        smartProgress(
          logger,
          message = "Calculating a range size estimate using a South America Albers Equal Area Conic projection on a user uploaded SDM", {

            # FUNCTION CALL ####
            ##First project to equal area
            p <- spp[[curSp()]]$postProc$OrigPred
            ##PROJECT
            p<-raster::projectRaster(p,crs="+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m")
            # find the number of cells that are not NA
            pCells <- raster::ncell(p[!is.na(p)])
            # Convert the raster resolution to km^s
            Resolution <- (res(p)/1000)^2
            # Multiply the two
            area <- pCells * Resolution

          })

        req(area)
        logger %>% writeLog( "Species range size calculated based on User SDM ")
        # LOAD INTO SPP ####
        spp[[curSp()]]$change$range <- area
        spp[[curSp()]]$change$rangetype <- input$selSource
        common$update_component(tab = "Results")
      }
      if(input$selSource == "mask"){
        #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
        #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly


        # ERRORS ####
        if (!is.null(spp[[curSp()]]$postProc$prediction)) {
          logger %>%
            writeLog(type = 'error',
                     'Do a maskRangeR analysis before doing range calculations')
          return()
        }
        p <- spp[[curSp()]]$postProc$prediction
        p[p == 0] <- NA
        if (length(unique(values(p)))> 2) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing range calculations')
          return()
        }
        smartProgress(
          logger,
          message = "Calculating a range size estimate using a South America Albers Equal Area Conic projection on a masked SDM", {

            # FUNCTION CALL ####
            ##First project to equal area
            p <- spp[[curSp()]]$postProc$OrigPred
            ##PROJECT
            p<-raster::projectRaster(p,crs="+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m")
            # find the number of cells that are not NA
            pCells <- raster::ncell(p[!is.na(p)])
            # Convert the raster resolution to km^s
            Resolution <- (res(p)/1000)^2
            # Multiply the two
            area <- pCells * Resolution

          })

        req(area)
        logger %>% writeLog( "Species range size calculated based on masked SDM ")
        # LOAD INTO SPP ####
        spp[[curSp()]]$change$range <- area
        spp[[curSp()]]$change$rangetype <- input$selSource
        common$update_component(tab = "Results")

      }
    }
    ##if calculating EOO
    else if(input$changeRangeSel == "eoo"){
      ##Check wether based on sdm or on occurrences
      if(input$selSource1 == "sdm"){
        ##check that the projection exists and that it is thresholded
        if (is.null(spp[[curSp()]]$project$mapProj)) {
          logger %>%
            writeLog(type = 'error',
                     'Project your model before doing EOO calculations')
          return()
        }
        if (is.null(spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet)) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing EOO calculations')
          return()
        }
        smartProgress(
          logger,
          message = "Calculating an EOO estimate based on the thresholded SDM ", {
            #must reclass the sdm to get 0 to be NA
        p <- spp[[curSp()]]$project$mapProj
        p[p == 0] <- NA
        p.pts <- raster::rasterToPoints(p)
        eooSDM <- changeRangeR::mcp(p.pts[,1:2])
        aeoosdm <- raster::area(eooSDM)/1000000

          })
        req(aeoosdm)
        logger %>% writeLog( "Calculated an EOO estimate based on a thresholded SDM. This is an approximation based on a non-projected SDM")
        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$change$EOOval <- aeoosdm
        spp[[curSp()]]$rmm$data$change$EOOtype <- input$selSource1
        spp[[curSp()]]$rmm$data$change$EOO <- eooSDM
        common$update_component(tab = "Map")
      }
      else if (input$selSource1 == "occs"){
        smartProgress(
          logger,
          message = "Calculating an EOO estimate based on occurrence points ", {
          occs <- spp[[curSp()]]$occs
        occs.xy <- occs %>% dplyr::select(longitude, latitude)
        # Create a minimum convex polygon around the occurrences
        eoo <- changeRangeR::mcp(occs.xy)
        # Define the coordinate reference system as unprojected
        crs(eoo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        area <- raster::area(eoo)/1000000

      })
        req(eoo)
        logger %>% writeLog( "Calculated an EOO estimate based on occurrences. This is an approximation based on unprojected coordinates")

        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$change$EOOval <- area
        spp[[curSp()]]$rmm$data$change$EOOtype <- input$selSource1
        spp[[curSp()]]$rmm$data$change$EOO <- eoo
        common$update_component(tab = "Map")
      }
    }
    else if (input$changeRangeSel == "aoo"){
      if(input$selSource2 =="occs"){
        ##check that the projection exists and that it is thresholded
        if (is.null(spp[[curSp()]]$project$mapProj)) {
          logger %>%
            writeLog(type = 'error',
                     'Project your model before doing AOO calculations')
          return()
        }
        if (is.null(spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet)) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing AOO calculations')
          return()
        }
        p <- spp[[curSp()]]$project$mapProj
        p[p == 0] <- NA
        # Using filtered records how to use unfiltered?
        occs <- spp[[curSp()]]$occs
        occs.xy <- occs %>% dplyr::select(longitude, latitude)
      #  p[!is.na(p)] <- 1
        AOOlocs<-changeRangeR::aooArea(r = p, locs = occs.xy)
        req(AOOlocs)
        logger %>% writeLog( "Calculated an AOO estimate based on an SDM and occurrences. This is an approximation based on unprojected coordinates")

        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$change$AOOval <- AOOlocs
        spp[[curSp()]]$rmm$data$change$AOOtype <- input$selSource2
        #spp[[curSp()]]$rmm$data$change$AOO <- aoo #Here the map but I don t have it
        common$update_component(tab = "Results")
      }
      else if (input$selSource2 =="sdm"){
        ##check that the projection exists and that it is thresholded
        if (is.null(spp[[curSp()]]$project$mapProj)) {
          logger %>%
            writeLog(type = 'error',
                     'Project your model before doing AOO calculations')
          return()
        }
        if (is.null(spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet)) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing AOO calculations')
          return()
        }
        p <- spp[[curSp()]]$project$mapProj
        p[p == 0] <- NA
        AOO<-changeRangeR::aooArea(r = p)
        req(AOO)
        logger %>% writeLog( "Calculated an AOO estimate based on an SDM. This is an approximation based on unprojected coordinates")

        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$change$AOOval <- AOO
        spp[[curSp()]]$rmm$data$change$AOOtype <- input$selSource2
        #spp[[curSp()]]$rmm$data$change$AOO <- aoo #Here the map but I don t have it
        common$update_component(tab = "Results")
      }
      else if (input$selSource2 =="mask"){
        #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
        #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly

        p <- spp[[curSp()]]$postProc$prediction
        p[p == 0] <- NA
        if (length(unique(values(p)))> 2) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing AOO calculations')
          return()
        }
        AOO<-aooArea(r = p)
        req(AOO)
        logger %>% writeLog( "Calculated an AOO estimate based on a masked SDM. This is an approximation based on unprojected coordinates")

        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$change$AOOval <- AOO
        spp[[curSp()]]$rmm$data$change$AOOtype <- input$selSource2
        #spp[[curSp()]]$rmm$data$change$AOO <- aoo #Here the map but I don t have it
        common$update_component(tab = "Results")
      }
    }
    })



 # output$result <- renderPrint({
  #  # Result
   # spp[[curSp()]]$change$range[1]
    #spp[[curSp()]]$rmm$data$change$EOOval
  #  spp[[curSp()]]$rmm$data$change$AOOval
#  })
  output$areas <- renderText({
    # Result
    if(is.null(spp[[curSp()]]$change$range[1])){
      spp[[curSp()]]$change$range[1]<-"Not calculated"
      spp[[curSp()]]$change$rangetype<-NA}
    if(is.null(spp[[curSp()]]$rmm$data$change$EOOval)){
      spp[[curSp()]]$rmm$data$change$EOOval<-"Not calculated"
      spp[[curSp()]]$rmm$data$change$EOOtype<-NA}
    if(is.null(spp[[curSp()]]$rmm$data$change$AOOval)){
      spp[[curSp()]]$rmm$data$change$AOOval<-"Not calculated"
      spp[[curSp()]]$rmm$data$change$AOOtype<-NA}
    # define contents for both evaluation tables
    #all_areas<-c(spp[[curSp()]]$change$range[1],spp[[curSp()]]$rmm$data$change$EOOval,spp[[curSp()]]$rmm$data$change$AOOval)
    #rangetype<-paste0("Range based on ", spp[[curSp()]]$change$rangetype)
    #eootype<-paste0("EOO based on ", spp[[curSp()]]$rmm$data$change$EOOtype)
    #aootype<-paste0("AOO based on ", spp[[curSp()]]$rmm$data$change$AOOtype)
    #names(all_areas)<-c(rangetype,eootype,aootype)
      paste(
        "Range based on", spp[[curSp()]]$change$rangetype, spp[[curSp()]]$change$range[1], "\n",
        "EOO based on ",  spp[[curSp()]]$rmm$data$change$EOOtype, spp[[curSp()]]$rmm$data$change$EOOval, "\n",
        "AOO based on ", spp[[curSp()]]$rmm$data$change$AOOtype, spp[[curSp()]]$rmm$data$change$AOOval)


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

change_range_module_result <- function(id) {
ns <- NS(id)
# Result UI
verbatimTextOutput(ns("areas"))


}

change_range_module_map <- function(map, common) {
  # Map logic
 spp <- common$spp
  curSp <- common$curSp
  map %>% clearAll()
if(!is.null(spp[[curSp()]]$rmm$data$change$EOO)){

 polyEOO <- spp[[curSp()]]$rmm$data$change$EOO@polygons[[1]]@Polygons
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
  if(!is.null(spp[[curSp()]]$rmm$data$change$AOO)){
    polyAOO <- spp[[curSp()]]$rmm$data$change$AOO@polygons[[1]]@Polygons
    map %%
      ##Add legend
      addLegend("bottomright", colors = "red",
                title = "AOO", labels = "AOO",
                opacity = 1)
    ##ADD polygon
    if (length(polyAOO) == 1) {
      xy <- list(polyAOO[[1]]@coords)
    } else {
      xy <- lapply(polyAOO, function(x) x@coords)
    }
    for (shp in xy) {
      map %>%
        addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "red",
                    group = 'change')
    }
  }

}

change_range_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    change_range_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  )
}

