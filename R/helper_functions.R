# add text to log
writeLog <- function(logs, ..., type = 'default') {
  if (is.null(logs)) {
    if (type == "default") {
      pre <- "> "
    } else if (type == 'error') {
      pre <- 'ERROR: '
    } else if (type == 'warning') {
      pre <- 'WARNING: '
    }  
    newEntries <- paste(pre, ..., collapse = "")
    message(newEntries)
    return()
  }
  
  if (type == "default") {
    pre <- "> "
  } else if (type == 'error') {
    pre <- '<font color="red"><b>! ERROR</b></font> : '
  } else if (type == 'warning') {
    pre <- '<font color="orange"><b>! WARNING</b></font> : '
  }
  newEntries <- paste(pre, ..., collapse = "")
  logs(paste(logs(), newEntries, sep = '<br>'))
}

popUpContent <- function(x) {
  lat <- round(as.numeric(x['latitude']), digits = 2)
  lon <- round(as.numeric(x['longitude']), digits = 2)
  as.character(tagList(
    tags$strong(paste("occID:", x['occID'])),
    tags$br(),
    tags$strong(paste("Latitude:", lat)),
    tags$br(),
    tags$strong(paste("Longitude:", lon)),
    tags$br(),
    tags$strong(paste("Year:", x['year'])),
    tags$br(),
    tags$strong(paste("Inst. Code:", x['institutionCode'])),
    tags$br(),
    tags$strong(paste("Country:", x['country'])),
    tags$br(),
    tags$strong(paste("State/Prov.:", x['stateProvince'])),
    tags$br(),
    tags$strong(paste("Locality:", x['locality'])),
    tags$br(),
    tags$strong(paste("Elevation:", x['elevation'])),
    tags$br(),
    tags$strong(paste("Basis of Record:", x['basisOfRecord']))
  ))
}

smartProgress <- function(logs, message, expr) {
  if(!is.null(logs)) {
    withProgress(message = message, expr)
  } else {
    message(message)
    expr
  }
}

mapBgPolys <- function(bgShpXY) {
  for (shp in bgShpXY) {
    map %>%
      addPolygons(lng = shp[,1], lat = shp[,2], fill = FALSE,
                  weight = 4, color="red", group='proj')
  }
}

# retrieve the value range for a prediction raster for plotting
getRasterVals <- function(r, type='raw') {
  v <- raster::values(r)
  # remove NAs
  v <- v[!is.na(v)]
  if (type == 'logistic' | type == 'cloglog') v <- c(v, 0, 1)  # set to 0-1 scale
  return(v)
}

maxentPredTransform <- function(results, curModel, bgMask, predType, shinyLogs = NULL) {
  pargs <- paste0("outputformat=", predType) 
  smartProgress(shinyLogs, message = paste0("Generating ", predType, " prediction for model", curModel, "..."), {
    transPred <- dismo::predict(results$models[[curModel]], bgMask, args=pargs)
  })  
  return(transPred)
}

getThresh <- function(occPredVals, thresh) {
  # remove all NA
  occPredVals <- na.omit(occPredVals)
  if (thresh == 'mtp') {
    # apply minimum training presence threshold
    x <- min(occPredVals)
  } else if (thresh == 'p10') {
    # Define 10% training presence threshold
    if (length(occPredVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
      n90 <- floor(length(occPredVals) * 0.9)
    } else {  # if greater than or equal to 10 occ values, round up
      n90 <- ceiling(length(occPredVals) * 0.9)
    }
    x <- rev(sort(occPredVals))[n90]  # apply 10% training presence threshold over all models
  }
  return(x)
}

threshPred  <- function(occs, predSel, thresh, rasName) {
  if (thresh != 'noThresh') {
    # find predicted values for occurrences for selected model
    # extract the suitability values for all occurrences
    occs.xy <- occs[c('longitude', 'latitude')]
    
    # determine the threshold based on the current, not projected, prediction
    occPredVals <- raster::extract(predSel, occs.xy)
    # get the chosen threshold value
    x <- getThresh(occPredVals, thresh)  
    # threshold model prediction
    threshPred <- predSel > x
    # rename
    names(threshPred) <- rasName
  } else {
    threshPred <- predSel
  }
  
  return(threshPred)
}

