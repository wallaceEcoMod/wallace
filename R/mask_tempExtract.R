# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# mask_tempExtract.R
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
#' @title mask_tempExtract
#' @description To mask prediction by temporal thresholds.
#' @details Mask a range prediction (e.g., an SDM prediction) with an
#' environmental raster. Set the upper and lower bounds to remove areas
#' above/below those values in the masking raster from the prediction. The upper
#' and lower bounds can be determined from tempAnnotate. Anything area in the
#' masking raster with values below the lower bound and above the upper bound
#' will be removed from the range prediction.
#' @param lowerInp numeric. Lower bound value
#' @param upperInp numeric. Upper bound value
#' @param maskRaster RasterLayer. The masking raster
#' @param pred RasterLayer. The range prediction to be masked
#' @param logger stores all notification messages to be displayed in the Log
#'  Window of Wallace GUI. insert the logger reactive list here for running in
#'  shiny, otherwise leave the default NULL
#' @param spN character. Used to obtain species name for logger messages
#' @examples
#' \dontrun{
#' ### Set parameters
#' lowerInp <- 50
#' upperInp <- 100
#' maskRaster <- raster::raster(system.file("extdata/MODIS/2010_olinguito_Modis.tif", package = "wallace"))
#' pred <- raster::raster(system.file("extdata/Bassaricyon_neblina.tif",  package = "wallace"))
#' ### Run function
#' postPred <- mask_tempExtract(lowerInp, upperInp, maskRaster, pred, logger = NULL, spN = NULL)
#' }
#' @return Returns a RasterLayer of the masked range prediction
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @seealso \code{\link{mask_tempAnnotate}} #mask_tempAnnotate?
#' @export
#'

mask_tempExtract <- function(lowerInp, upperInp, maskRaster, pred,
                             logger = NULL, spN = NULL) {
  if (is.na(lowerInp) & is.na(upperInp)) {
    logger %>% writeLog(type = 'error', hlSpp(spN), "Please provide bounds for masking. ")
    return()
  }
  # compare prediction and mask Raster
  smartProgress(logger, message = "Masking ...", {
    pred <- terra::rast(pred)
    maskRaster <- terra::rast(maskRaster)
    pred <- terra::resample(pred, maskRaster, "near")
    sameExt <- tryCatch(expr =terra::compareGeom(maskRaster, pred, ext = FALSE,
                                  rowcol = FALSE, crs = TRUE, res = TRUE),
                        error = function(e) NULL)
    if (is.null(sameExt)) {
      logger %>%
        writeLog(type = 'warning', hlSpp(spN),
                 "Rasters don't have the same resolution, CRS, and/or origin. ")
      return()
    }
    maskRaster <- terra::crop(maskRaster, pred)
    postPred <- pred * (maskRaster >= lowerInp) * (maskRaster <= upperInp)
    predValues <- terra::spatSample(x = postPred,
                                    size = 100, na.rm = TRUE)[, 1]
    if (any(predValues > 0 & predValues < 1)) {
      postPred[postPred <= 0] <- NA
    }
    postPred <- postPred %>% terra::trim() %>% raster::raster()
  })
  return(postPred)
}
