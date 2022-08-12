
#' @title mask_expPoly
#' @description Remove or add polygon to prediction
#' @param polyMask Polygon in shapefile
#' @param prediction Raster prediction
#' @param polyExt Polygon of background extent
#' @param rem Remove areas of polygon from prediction
#' @param logger logger
#' @param spN species name
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'

mask_expPoly <- function(polyMask, prediction, polyExt, rem = FALSE,
                         logger = NULL, spN = NULL) {
  # Check for NAs
  v <- raster::extract(prediction, polyMask)
  v <- is.na(unlist(v))

  if (sum(v) == length(v)) {
    logger %>% writeLog(
      type = 'error', hlSpp(spN),
      paste("The polygon just included NA values.",
            "Please, select a polygon that intersects model prediction.(**)")
    )
    return()
  }

  if (sum(v) > 0) {
    logger %>% writeLog(
      type = 'warning', hlSpp(spN),
      paste("The polygon selected included some cells with NA values.",
            "You cannot changes the predictions (suitable or unsuitable),",
            "in these cells.(**)")
    )
  }

  polyRaster <- raster::rasterize(polyMask, prediction, 1)
  polyRaster[is.na(polyRaster)] <- 0

  ## Add shiny logs
  if (rem == FALSE) {
    newPred <- prediction + polyRaster
    newPred[newPred > 1] <- 1
    extPoly <- polyExt
    logger %>% writeLog(
      hlSpp(spN), "The polygon was added (**)")
  } else {
    rastValues <- raster::values(prediction)
    if (length(unique(rastValues)) == 3 | length(unique(rastValues)) == 2) {
      newPred <- prediction - polyRaster
      newPred[newPred < 0] <- 0
      extPoly <- polyExt
    } else {
      smartProgress(logger, message = "Removing area..", {
        newPred <- prediction - polyRaster
        newPred[newPred <= 0] <- NA
        newPred <- raster::trim(newPred)
        extPoly <- raster::extent(newPred)
        extPoly <- methods::as(extPoly, 'SpatialPolygons')
      })
    }
    logger %>% writeLog(
      hlSpp(spN), "The polygon was removed (**)")
  }

  return(list(pred = newPred, ext = extPoly))
}
