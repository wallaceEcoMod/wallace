comp3_bioclim <- function(bcRes) {
  if (bcRes == "") return()
  # getData() downloads bioclim variables unless they exist in directory, then just loads them
  withProgress(message = "Retrieving WorldClim data...", {
    values$preds <- raster::getData(name = "worldclim", var = "bio", res = bcRes)
  })
  # proxy %>% addLegend("topleft", colors = c(),
  #                     title = "Predictors: WorldClim bio 1-19", labels = c(),
  #                     opacity = 1, layerId = 2)
  isolate(writeLog(paste("* Environmental predictors: WorldClim bio1-19 at", bcRes, " arcmin resolution.")))
  withProgress(message = "Processing...", {
    locs.vals <- extract(values$preds[[1]], values$df[,2:3])
    
    if (sum(is.na(locs.vals)) > 0) {
      isolate(writeLog(paste0("* Removed records without environmental values with IDs: ",
                              paste(row.names(values$df[is.na(locs.vals),]), collapse=', '), ".")))
    }
    values$df <- values$df[!is.na(locs.vals),]  # remove locs without environmental values
    
    if (!is.null(values$inFile)) {
      values$inFile <- values$inFile[!is.na(locs.vals), ]
    }
  })
}