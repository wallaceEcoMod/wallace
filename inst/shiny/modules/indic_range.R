indic_range_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("indicRangeSel"), "Options Available:",
                choices = list("None selected" = '',
                               "Range size" = "range",
                               "EOO" = "eoo",
                               "AOO" = "aoo")), # Check default (no selected)
    conditionalPanel(sprintf("input['%1$s'] == 'range'",
                             ns("indicRangeSel")),
                     selectInput(ns("selSource") ,
                                 label = "Select source for calculations",
                                 choices = list("Wallace SDM" = "wallace",
                                                "Transferred SDM" = "xfer",
                                                "User SDM" = "user",
                                                "Masked SDM" = "mask"))),
    conditionalPanel(sprintf("input['%1$s'] == 'eoo'",
                             ns("indicRangeSel")),
                     selectInput(ns("selSource1") ,
                                 label = "Select source for calculations",
                                 choices = list("Occurrences" = "occs",
                                                "Wallace SDM" = "wallace",
                                                "Transferred SDM" = "xfer",
                                                "User SDM" = "user" #,
                                                #"Masked SDM" = "mask"
                                                ))),

    conditionalPanel(sprintf("input['%1$s'] == 'aoo'",
                             ns("indicRangeSel")),
                     selectInput(ns("selSource2") ,
                                 label = "Select source for calculations",
                                 choices = list("Occurrences & Wallace SDM" = "occs",
                                                "Wallace SDM" = "wallace",
                                                "Transferred SDM" = "xfer",
                                                "User SDM" = "user",
                                                "Masked SDM" = "mask"))),
    ##question for mary add option to do range for sdm that comes from maskRangeR or uploaded?
    actionButton(ns("goRange"), "Calculate")
  )
}

indic_range_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapXfer <- common$mapXfer



  observeEvent(input$goRange, {
    # WARNING ####

    #Processing
    if (input$indicRangeSel == "range") {
      if (input$selSource == "wallace") {
        # ERRORS ####
        if (is.null(spp[[curSp()]]$visualization$mapPred)) {
          logger %>%
            writeLog(type = 'error',
                     'Visualize your model before doing range calculations')
          return()
        }
        if (is.null( spp[[curSp()]]$visualization$thresholds)) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded model before doing range calculations')
          return()
        }
        smartProgress(
          logger,
          message = paste0("Calculating a range size estimate using a ",
                           "South America Albers Equal Area Conic projection"), {
            # FUNCTION CALL ####
            ##First project to equal area
            p <- spp[[curSp()]]$visualization$mapPred
            ##PROJECT
            p <- raster::projectRaster(
              p,
              crs = "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m")
            # find the number of cells that are not NA
            pCells <- raster::ncell(p[!is.na(p)])
            # Convert the raster resolution to km^s
            Resolution <- (raster::res(p)/1000)^2
            # Multiply the two
            area <- pCells * Resolution

          })

        req(area)
        logger %>% writeLog("Species range size calculated based on Wallace SDM.")
        # LOAD INTO SPP ####
        spp[[curSp()]]$indic$range <- area
        spp[[curSp()]]$indic$rangetype <- input$selSource
        common$update_component(tab = "Results")
      }
      if (input$selSource == "xfer") {
        # ERRORS ####
        if (is.null(spp[[curSp()]]$transfer$mapXfer)) {
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
          message = paste0("Calculating a range size estimate using a ",
                           "South America Albers Equal Area Conic projection"), {
            # FUNCTION CALL ####
            ## First project to equal area
            p <- spp[[curSp()]]$transfer$mapXfer

            ## PROJECT
            p <- raster::projectRaster(
              p,
              crs = "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m")
            # find the number of cells that are not NA
            pCells <- raster::ncell(p[!is.na(p)])
            # Convert the raster resolution to km^s
            Resolution <- (raster::res(p)/1000)^2
            # Multiply the two
            area <- pCells * Resolution
          })

        req(area)
        logger %>% writeLog("Species range size calculated based on  projected SDM")
        # LOAD INTO SPP ####
        spp[[curSp()]]$indic$range <- area
        spp[[curSp()]]$indic$rangetype <- input$selSource
        common$update_component(tab = "Results")
      }
      if (input$selSource == "user") {
        # ERRORS ####
        if (is.null(spp[[curSp()]]$mask$userSDM)) {
          logger %>%
            writeLog(type = 'error',
                     'Load you model in component User SDM before doing range calculations')
          return()
        }
        p <- spp[[curSp()]]$mask$userSDM
        p[p == 0] <- NA
        if (length(unique(raster::values(p))) > 2) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing range calculations')
          return()
        }
        smartProgress(
          logger,
          message = paste0("Calculating a range size estimate using a South ",
                           "America Albers Equal Area Conic projection on a ",
                           "user uploaded SDM"), {

            # FUNCTION CALL ####
            ##First project to equal area
            p <- spp[[curSp()]]$mask$userSDM
            ##PROJECT
            p <- raster::projectRaster(
              p,
              crs = "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m")
            # find the number of cells that are not NA
            pCells <- raster::ncell(p[!is.na(p)])
            # Convert the raster resolution to km^s
            Resolution <- (raster::res(p)/1000)^2
            # Multiply the two
            area <- pCells * Resolution

          })
        req(area)
        logger %>% writeLog("Species range size calculated based on User SDM")
        # LOAD INTO SPP ####
        spp[[curSp()]]$indic$range <- area
        spp[[curSp()]]$indic$rangetype <- input$selSource
        common$update_component(tab = "Results")
      }
      if (input$selSource == "mask") {
        # ERRORS ####
        if (is.null(spp[[curSp()]]$mask$prediction)) {
          logger %>%
            writeLog(type = 'error',
                     'Do a maskRangeR analysis before doing range calculations')
          return()
        }
        p <- spp[[curSp()]]$mask$prediction
        p[p == 0] <- NA
        if (length(unique(raster::values(p)))> 2) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing range calculations')
          return()
        }
        smartProgress(
          logger,
          message = paste0("Calculating a range size estimate using a South ",
                           "America Albers Equal Area Conic projection on ",
                           "a masked SDM"), {

            # FUNCTION CALL ####
            ##First project to equal area
            p <- spp[[curSp()]]$mask$prediction
            ##PROJECT
            p <- raster::projectRaster(
              p,
              crs = "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m")
            # find the number of cells that are not NA
            pCells <- raster::ncell(p[!is.na(p)])
            # Convert the raster resolution to km^s
            Resolution <- (raster::res(p)/1000)^2
            # Multiply the two
            area <- pCells * Resolution
          })
        req(area)
        logger %>% writeLog("Species range size calculated based on masked SDM")
        # LOAD INTO SPP ####
        spp[[curSp()]]$indic$range <- area
        spp[[curSp()]]$indic$rangetype <- input$selSource
        common$update_component(tab = "Results")
      }
    ## if calculating EOO
    } else if (input$indicRangeSel == "eoo") {
      ## Check whether based on sdm or on occurrences
      if (input$selSource1 == "wallace") {
        ##check that the transfer exists and that it is thresholded
        if (is.null(spp[[curSp()]]$visualization$mapPred)) {
          logger %>%
            writeLog(type = 'error',
                     'Transfer your model before doing EOO calculations')
          return()
        }
        if (is.null(spp[[curSp()]]$visualization$thresholds)) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing EOO calculations')
          return()
        }
        smartProgress(
          logger,
          message = "Calculating an EOO estimate based on the thresholded SDM ", {
            #must reclass the sdm to get 0 to be NA
            p <- spp[[curSp()]]$visualization$mapPred
            p[p == 0] <- NA
            p.pts <- raster::rasterToPoints(p)
            eooSDM <- changeRangeR::mcp(p.pts[,1:2])
            aeoosdm <- raster::area(eooSDM)/1000000
          })
        req(aeoosdm)
        logger %>% writeLog("Calculated an EOO estimate based on a ",
                            "thresholded SDM. This is an approximation ",
                            "based on a non-transferred SDM")
        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$indic$EOOval <- aeoosdm
        spp[[curSp()]]$rmm$data$indic$EOOtype <- input$selSource1
        spp[[curSp()]]$rmm$data$indic$EOO <- eooSDM
        common$update_component(tab = "Map")
      }
      if (input$selSource1 == "user") {
        ##check that the transfer exists and that it is thresholded
        if (is.null(spp[[curSp()]]$mask$userSDM)) {
          logger %>%
            writeLog(type = 'error',
                     'Load a model before doing EOO calculations')
          return()
        }
        p <- spp[[curSp()]]$mask$userSDM
        p[p == 0] <- NA
        if (length(unique(raster::values(p)))> 2) {
          logger %>%
            writeLog(
              type = 'error',
              'Generate a thresholded prediction before doing range calculations')
          return()
        }
        smartProgress(
          logger,
          message = "Calculating an EOO estimate based on the user provided SDM ", {
            #must reclass the sdm to get 0 to be NA
            p <- spp[[curSp()]]$mask$userSDM
            p[p == 0] <- NA
            p.pts <- raster::rasterToPoints(p)
            eooSDM <- changeRangeR::mcp(p.pts[,1:2])
            aeoosdm <- raster::area(eooSDM)/1000000

          })
        req(aeoosdm)
        logger %>% writeLog("Calculated an EOO estimate based on a user ",
                            "provided SDM. This is an approximation based ",
                            "on a non-transferred SDM")
        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$indic$EOOval <- aeoosdm
        spp[[curSp()]]$rmm$data$indic$EOOtype <- input$selSource1
        spp[[curSp()]]$rmm$data$indic$EOO <- eooSDM
        common$update_component(tab = "Map")
      }
      if (input$selSource1 == "xfer") {
        ##check that the transfer exists and that it is thresholded
        if (is.null(spp[[curSp()]]$transfer$mapXfer)) {
          logger %>%
            writeLog(type = 'error',
                     'Transfer your model before doing EOO calculations')
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
          message = "Calculating an EOO estimate based on the transferred thresholded SDM ", {
            #must reclass the sdm to get 0 to be NA
            p <- spp[[curSp()]]$transfer$mapXfer
            p[p == 0] <- NA
            p.pts <- raster::rasterToPoints(p)
            eooSDM <- changeRangeR::mcp(p.pts[,1:2])
            aeoosdm <- raster::area(eooSDM)/1000000
          })
        req(aeoosdm)
        logger %>% writeLog("Calculated an EOO estimate based on a transferred ",
                            "thresholded SDM. This is an approximation based ",
                            "on a non-transferred SDM")
        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$indic$EOOval <- aeoosdm
        spp[[curSp()]]$rmm$data$indic$EOOtype <- input$selSource1
        spp[[curSp()]]$rmm$data$indic$EOO <- eooSDM
        common$update_component(tab = "Map")
      }
      if (input$selSource1 == "occs") {
        if (is.null(spp[[curSp()]]$occs)) {
          logger %>%
            writeLog(type = 'error',
                     'Get or upload occurrence data for this species before ",
                     "doing EOO calculations (names must match)')
          return()
        }
        smartProgress(
          logger,
          message = "Calculating an EOO estimate based on occurrence points ", {
            occs <- spp[[curSp()]]$occs
            occs.xy <- occs %>% dplyr::select(longitude, latitude)
            # Create a minimum convex polygon around the occurrences
            eoo <- changeRangeR::mcp(occs.xy)
            # Define the coordinate reference system as unprojected
            raster::crs(eoo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
            area <- raster::area(eoo)/1000000

          })
        req(eoo)
        logger %>% writeLog("Calculated an EOO estimate based on occurrences. ",
                            "This is an approximation based on unprojected coordinates")

        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$indic$EOOval <- area
        spp[[curSp()]]$rmm$data$indic$EOOtype <- input$selSource1
        spp[[curSp()]]$rmm$data$indic$EOO <- eoo
        common$update_component(tab = "Map")
      }
      ## if calculating AOO
    } else if (input$indicRangeSel == "aoo") {
      if (input$selSource2 == "occs") {
        ##check that the transfer exists and that it is thresholded
        if (is.null(spp[[curSp()]]$visualization$mapPred)) {
          logger %>%
            writeLog(type = 'error',
                     'Visualize your model before doing AOO calculations')
          return()
        }
        if (is.null(spp[[curSp()]]$visualization$thresholds)) {
          logger %>%
            writeLog(type = 'error',
                     'Visualize a thresholded prediction before doing AOO calculations')
          return()
        }
        p <- spp[[curSp()]]$visualization$mapPred
        p[p == 0] <- NA
        # Using filtered records how to use unfiltered?
        occs <- spp[[curSp()]]$occs
        occs.xy <- occs %>% dplyr::select(longitude, latitude)
      #  p[!is.na(p)] <- 1
        AOOlocs<-changeRangeR::AOOarea(r = p, locs = occs.xy)
        req(AOOlocs)
        logger %>% writeLog("Calculated an AOO estimate based on an SDM ",
                            "and occurrences. This is an approximation based ",
                            "on unprojected coordinates")

        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$indic$AOOval <- AOOlocs$area
        spp[[curSp()]]$rmm$data$indic$AOOtype <- input$selSource2
        spp[[curSp()]]$rmm$data$indic$AOO <- AOOlocs$aooRaster
        common$update_component(tab = "Map")
      }
      if (input$selSource2 == "wallace") {
        ##check that the transfer exists and that it is thresholded
        if (is.null(spp[[curSp()]]$visualization$mapPred)) {
          logger %>%
            writeLog(type = 'error',
                     'Visualize your model before doing AOO calculations')
          return()
        }
        if (is.null(spp[[curSp()]]$visualization$thresholds)) {
          logger %>%
            writeLog(type = 'error',
                     'Visualize a thresholded prediction before doing AOO calculations')
          return()
        }
        p <- spp[[curSp()]]$visualization$mapPred
        p[p == 0] <- NA
        AOO<-changeRangeR::AOOarea(r = p)
        req(AOO)
        logger %>% writeLog("Calculated an AOO estimate based on an SDM. This ",
                            "is an approximation based on unprojected coordinates")

        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$indic$AOOval <- AOO$area
        spp[[curSp()]]$rmm$data$indic$AOOtype <- input$selSource2
        spp[[curSp()]]$rmm$data$indic$AOO <- AOO$aooRaster
        common$update_component(tab = "Map")
      }
      if (input$selSource2 == "xfer") {
        ##check that the transfer exists and that it is thresholded
        if (is.null(spp[[curSp()]]$transfer$mapXfer)) {
          logger %>%
            writeLog(type = 'error',
                     'Transfer your model before doing AOO calculations')
          return()
        }
        if (is.null(spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet)) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing AOO calculations')
          return()
        }
        p <- spp[[curSp()]]$transfer$mapXfer
        p[p == 0] <- NA
        AOO<-changeRangeR::AOOarea(r = p)
        req(AOO)
        logger %>% writeLog("Calculated an AOO estimate based on an SDM. This ",
                            "is an approximation based on unprojected coordinates")
        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$indic$AOOval <- AOO$area
        spp[[curSp()]]$rmm$data$indic$AOOtype <- input$selSource2
        spp[[curSp()]]$rmm$data$indic$AOO <- AOO$aooRaster
        common$update_component(tab = "Map")
      }
      if (input$selSource2 == "user") {
        ##check that the transfer exists and that it is thresholded
        if (is.null(spp[[curSp()]]$mask$userSDM)) {
          logger %>%
            writeLog(type = 'error',
                     'Upload your model before doing AOO calculations')
          return()
        }
        p <- spp[[curSp()]]$mask$userSDM
        p[p == 0] <- NA
        if (length(unique(raster::values(p)))> 2) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing range calculations')
          return()
        }
        p <- spp[[curSp()]]$mask$userSDM
        p[p == 0] <- NA
        AOO<-changeRangeR::AOOarea(r = p)
        req(AOO)
        logger %>% writeLog("Calculated an AOO estimate based on an user ",
                            "uploaded SDM. This is an approximation based ",
                            "on unprojected coordinates")
        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$indic$AOOval <- AOO$area
        spp[[curSp()]]$rmm$data$indic$AOOtype <- input$selSource2
        spp[[curSp()]]$rmm$data$indic$AOO <- AOO$aooRaster
        common$update_component(tab = "Map")
      }
      if (input$selSource2 == "mask") {
        p <- spp[[curSp()]]$mask$prediction
        p[p == 0] <- NA
        if (length(unique(raster::values(p))) > 2) {
          logger %>%
            writeLog(type = 'error',
                     'Generate a thresholded prediction before doing AOO calculations')
          return()
        }
        AOO<-AOOarea(r = p)
        req(AOO)
        logger %>% writeLog("Calculated an AOO estimate based on a masked SDM. ",
                            "This is an approximation based on unprojected coordinates")
        # LOAD INTO SPP ####
        spp[[curSp()]]$rmm$data$indic$AOOval <- AOO$area
        spp[[curSp()]]$rmm$data$indic$AOOtype <- input$selSource2
        spp[[curSp()]]$rmm$data$indic$AOO <- AOO$aooRaster
        common$update_component(tab = "Map")
      }
    }
  })
  # output$result <- renderPrint({
  #  # Result
  # spp[[curSp()]]$indic$range[1]
  #spp[[curSp()]]$rmm$data$indic$EOOval
  #  spp[[curSp()]]$rmm$data$indic$AOOval
  #  })
  output$areas <- renderText({
    # Result
    if (is.null(spp[[curSp()]]$indic$range[1])) {
      spp[[curSp()]]$indic$range[1] <- "Not calculated"
      spp[[curSp()]]$indic$rangetype <- NA
    }
    if (is.null(spp[[curSp()]]$rmm$data$indic$EOOval)) {
      spp[[curSp()]]$rmm$data$indic$EOOval <- "Not calculated"
      spp[[curSp()]]$rmm$data$indic$EOOtype <- NA
    }
    if (is.null(spp[[curSp()]]$rmm$data$indic$AOOval)) {
      spp[[curSp()]]$rmm$data$indic$AOOval <- "Not calculated"
      spp[[curSp()]]$rmm$data$indic$AOOtype <- NA
    }
    # define contents for both evaluation tables
    #all_areas<-c(spp[[curSp()]]$indic$range[1],spp[[curSp()]]$rmm$data$indic$EOOval,
    #spp[[curSp()]]$rmm$data$indic$AOOval)
    #rangetype<-paste0("Range based on ", spp[[curSp()]]$indic$rangetype)
    #eootype<-paste0("EOO based on ", spp[[curSp()]]$rmm$data$indic$EOOtype)
    #aootype<-paste0("AOO based on ", spp[[curSp()]]$rmm$data$indic$AOOtype)
    #names(all_areas)<-c(rangetype,eootype,aootype)
    paste(
      "Range based on", spp[[curSp()]]$indic$rangetype,
      spp[[curSp()]]$indic$range[1],"Km^2", "\n",
      "EOO based on ",  spp[[curSp()]]$rmm$data$indic$EOOtype,
      spp[[curSp()]]$rmm$data$indic$EOOval,"Km^2","\n",
      "AOO based on ", spp[[curSp()]]$rmm$data$indic$AOOtype,
      spp[[curSp()]]$rmm$data$indic$AOOval)


  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      list(
        indicRangeSel = input$indicRangeSel,
        selSource = input$selSource,
        selSource1 = input$selSource1,
        selSource2 = input$selSource2)
    },
    load = function(state) {
      # Load
      updateSelectInput(session, 'indicRangeSel', selected = state$indicRangeSel)
      updateSelectInput(session, ' selSource', selected = state$selSource)
      updateSelectInput(session, ' selSource1', selected = state$selSource1)
      updateSelectInput(session, ' selSource2', selected = state$selSource2)
    }
  ))

}

indic_range_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  verbatimTextOutput(ns("areas"))


}

indic_range_module_map <- function(map, common) {
  # Map logic
  spp <- common$spp
  curSp <- common$curSp
  map %>% clearAll()
  if(!is.null(spp[[curSp()]]$rmm$data$indic$EOO)){

    polyEOO <- spp[[curSp()]]$rmm$data$indic$EOO@polygons[[1]]@Polygons
    bb <- spp[[curSp()]]$rmm$data$indic$EOO@bbox
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
                    group = 'indic')
    }
  }
  if(!is.null(spp[[curSp()]]$rmm$data$indic$AOO)){

    AOOras <- spp[[curSp()]]$rmm$data$indic$AOO
    zoomExt <- raster::extent(AOOras)
    map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                      lat1 = zoomExt[3], lat2 = zoomExt[4])
    map %>%
      ##Add legend
      addLegend("bottomright", colors = c('red', 'grey'),
                title = "AOO",
                labels = c("Presence", "Absence"),
                opacity = 1, layerId = 'expert') %>%
      ##ADD polygon
      addRasterImage(AOOras, colors = c('red', 'grey'),
                     opacity = 0.7, group = 'indic', layerId = 'AOO',
                     method = "ngb")
  }

}

indic_range_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  # list(
  #   indic_range_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  # )
}

