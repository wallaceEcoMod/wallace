
#' @title mask_expPoly
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param polyAddRem x
#' @param prediction x
#' @param rem x
#' @param bgExt x
#' @param logger x
#' @param spN Character. Used to obtain species name for logger messages
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

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
        newPred[newPred < 0] <- NA
        newPred <- raster::trim(newPred)
        extPoly <- raster::extent(newPred)
        extPoly <- as(extPoly, 'SpatialPolygons')
      })
    }
  }

  return(list(pred = newPred, ext = extPoly))
}
