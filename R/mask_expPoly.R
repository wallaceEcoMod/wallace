
#' @title mask_expPoly
#' @description Remove or add polygon to prediction
#' @param polyMask Polygon in shapefile
#' @param prediction Raster prediction
#' @param bgExt Polygon of background extent
#' @param rem Remove areas of polygon from prediction
#' @param logger logger
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'

mask_expPoly <- function(polyMask, prediction, bgExt, rem = FALSE,
                         logger = NULL) {
  ## Add shiny logs
  if (rem == FALSE) {
    addRaster <- raster::rasterize(polyMask, prediction, 1)
    addRaster[is.na(addRaster)] <- 0
    newPred <- prediction + addRaster
    newPred[newPred > 1] <- 1
    extPoly <- bgExt
  } else {
    remRaster <- raster::rasterize(polyMask, prediction, 1)
    remRaster[is.na(remRaster)] <- 0
    rastValues <- raster::values(prediction)
    if (length(unique(rastValues)) == 3 | length(unique(rastValues)) == 2) {
      newPred <- prediction - remRaster
      newPred[newPred < 0] <- 0
      extPoly <- bgExt
    } else {
      smartProgress(logger, message = "Removing area..", {
        newPred <- prediction - remRaster
        newPred[newPred <= 0] <- NA
        newPred <- raster::trim(newPred)
        extPoly <- raster::extent(newPred)
        extPoly <- methods::as(extPoly, 'SpatialPolygons')
      })
    }
  }

  return(list(pred = newPred, ext = extPoly))
}
