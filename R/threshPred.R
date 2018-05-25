#' @export

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

#' @export

threshPred  <- function(occs, predSel, curModel, thresh, predType, shinyLogs = NULL) {
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
    names(threshPred) <- paste0(curModel, '_thresh_', thresh)
    shinyLogs %>% writeLog(thresh, 'threshold selected for', predType, ': ', round(x, digits = 3), '.')
  } else {
    threshPred <- predSel
  }
  
  return(threshPred)
}