# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# mask_expPoly.R
# File author: Wallace EcoMod Dev Team. 2023.
# --------------------------------------------------------------------------
# This file is part of the Wallace EcoMod application
# (hereafter “Wallace”).
#
# Wallace is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# Wallace is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Wallace. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
#' @title mask_expPoly
#' @description Remove or add polygon to prediction
#' @details Mask a range prediction by removing or adding a polygon. The polygon
#' must fall partly within the extent of the prediction. If the prediction is
#' continuous, only remove polygon will work.
#' @param polyMask Polygon in shapefile
#' @param prediction Raster prediction, of which to add/remove polyMask.
#' @param polyExt Polygon of the prediction background extent
#' @param rem Remove areas of polygon from prediction or add them to prediction. (TRUE = remove, FALSE = add)
#' @param logger stores all notification messages to be displayed in the Log
#'  Window of Wallace GUI. insert the logger reactive list here for running in
#'  shiny, otherwise leave the default NULL
#' @param spN species name
#' @examples
#' \dontrun{
#' ### Set parameters
#' # range prediction
#' prediction <- raster::raster(system.file("extdata/Bassaricyon_neblina.tif", package = "wallace"))
#' # bg extent
#' polyExt <- terra::ext(prediction)
#' polyExt <- terra::as.polygons(polyExt)
#' polyExt <-sf::st_as_sf(polyExt)
#' #poly for masking
#' polyMask <- sf::st_read(system.file("extdata/wdpa/WDPA_COL_olinguito.shp", package = "wallace"))
#' ### Run function
#' expertRast <- mask_expPoly(polyMask, prediction, polyExt, rem = TRUE, logger = NULL, spN = NULL)
#' }
#' @return A list of two elements: the masked prediction and the extent
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
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
      paste("The polygon only included NA values.",
            "Please select a polygon that intersects the model prediction. ")
    )
    return()
  }

  if (sum(v) > 0) {
    logger %>% writeLog(
      type = 'warning', hlSpp(spN),
      "The polygon selected includes some cells with NA values. You cannot change the prediction (suitable or unsuitable) in these cells. "
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
      hlSpp(spN), "The polygon was added. ")
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
      hlSpp(spN), "The polygon was removed. ")
  }

  return(list(pred = newPred, ext = extPoly))
}
