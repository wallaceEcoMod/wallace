comp3_bioclim <- function(bcRes) {
  if (bcRes == "") return()
  # getData() downloads bioclim variables unless they exist in directory, then just loads them
  withProgress(message = "Retrieving WorldClim data...", {
    if (bcRes == 0.5) {
      values$bcLat <- values$mapCntr[2]
      values$bcLon <- values$mapCntr[1]
      values$preds <- raster::getData(name = "worldclim", var = "bio", res = bcRes, lon = values$bcLon, lat = values$bcLat)
    } else {
      values$bcLat <- NULL
      values$bcLon <- NULL
      values$preds <- raster::getData(name = "worldclim", var = "bio", res = bcRes)
    }
  })
  # proxy %>% addLegend("topleft", colors = c(),
  #                     title = "Predictors: WorldClim bio 1-19", labels = c(),
  #                     opacity = 1, layerId = 2)
  isolate(writeLog(paste("> Environmental predictors: WorldClim bio1-19 at", bcRes, " arcmin resolution.")))
  withProgress(message = "Processing...", {
    locs.vals <- raster::extract(values$preds[[1]], values$df[c('longitude', 'latitude')])

    if (sum(is.na(locs.vals)) == length(locs.vals)) {
      writeLog(paste0('<font color="red"><b>! ERROR</b></font> : No localities overlay with environmental predictors. All localities may be marine -- please redo with terrestrial occurrences.'))
      return()
    }

    if (sum(is.na(locs.vals)) > 0) {
      isolate(writeLog(paste0("! WARNING: Removed records without environmental values with IDs: ",
                              paste(row.names(values$df[is.na(locs.vals),]), collapse=', '), ".")))
    }
    values$df <- values$df[!is.na(locs.vals),]  # remove locs without environmental values

    if (!is.null(values$inFile)) {
      values$inFile <- values$inFile[!is.na(locs.vals), ]
    }
  })
}

# comp3_userPreds <- function(userIn) {
#   if (is.null(values$df)) return()
#   withProgress(message = "Reading in user predictor rasters...", {
#     print(userIn)
#     userIn.noHDR <- userIn[grepl(userIn, pattern='[.]hdr'),]
#     values$preds <- stack()
#     for (i in 1:length(userIn[,1])) {
#       values$preds <- isolate({stack(values$preds, raster(userIn.noHDR[[i, 'datapath']]))})
#     }
#   })
#
#   withProgress(message = "Processing...", {
#     locs.vals <- extract(values$preds[[1]], values$df[,2:3])
#
#     if (sum(is.na(locs.vals)) > 0) {
#       isolate(writeLog(paste0("> Removed records without environmental values with IDs: ",
#                               paste(row.names(values$df[is.na(locs.vals),]), collapse=', '), ".")))
#     }
#     values$dfOrig <- values$df
#     values$df <- values$df[!is.na(locs.vals),]  # remove locs without environmental values
#     if (nrow(values$df) == 0) {
#       isolate({writeLog('All records removed. Please check extent of the input predictor rasters.')})
#       values$df <- values$dfOrig
#     }
#
#     if (!is.null(values$inFile)) {
#       values$inFile <- values$inFile[!is.na(locs.vals), ]
#     }
#   })
# }
