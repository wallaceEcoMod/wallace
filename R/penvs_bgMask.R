# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# penvs_bgMask.R
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

#' @title penvs_bgMask Mask environmental data
#' @description This functions crops and masks the environmental data to the
#'   provided background area.
#'
#' @details
#'  This function is used in the select study region component. Here, the
#'    environmental layers to be used in the modeling are cropped and masked
#'    to the provided background area. The background area is determined in
#'    the function penvs_bgExtent from the same component. The function returns
#'    the provided environmental layers cropped and masked in the provided
#'    format (either a rasterBrick or a rasterStack).
#'
#' @param occs data frame of cleaned or processed occurrences obtained from
#'   components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param envs a RasterStack or RasterBrick of environmental layers to be
#'   processed. This determines the output type.
#' @param bgExt a SpatialPolygonsDataFrame with the background area to be used
#'   for processing.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL.
#' @param spN species name to be used for all logger messages
#' @examples
#' \dontrun{
#' occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
#'                  package = "wallace"))[, 2:3]
#' occs$occID <- 1:nrow(occs)
#' envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = TRUE),
#'                       rasName = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = FALSE))
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMask <- penvs_bgMask(occs, envs, bgExt)
#' }
#'
#' @return A RasterStack or a RasterBrick of environmental layers cropped and
#'   masked to match the provided background extent.
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
#' @seealso   \code{\link{penvs_userBgExtent}},
#'   \code{\link{penvs_drawBgExtent}}, \code{\link{penvs_bgExtent}},
#'   \code{\link{penvs_bgSample}}
#' @export

penvs_bgMask <- function(occs, envs, bgExt, logger = NULL, spN = NULL) {
  if (is.null(bgExt)) {
    logger %>% writeLog(
      type = 'error',
      hlSpp(spN),
      "Before sampling background points, define the background extent.")
    return()
  }
  # mask envs by background extent
  smartProgress(logger,
                       message = paste0("Masking rasters for ",
                                        spName(spN), "..."), {

    bgCrop <- raster::crop(envs, bgExt)
    bgMask <- raster::mask(bgCrop, bgExt)
    # GEPB: Workaround when raster alignment is changed after crop, which makes appears
    # new duplicated occs in the same grid cells.
    occsEnvsVals <- as.data.frame(raster::extract(bgMask,
                                                  occs[, c('longitude', 'latitude')],
                                                  cellnumbers = TRUE))
    occs.dups <- duplicated(occsEnvsVals[, 1])
    if (sum(occs.dups) > 0) {
      bgMask <- terra::project(terra::rast(bgMask),
                               terra::rast(envs), method = 'near')
      bgMask <- methods::as(bgMask, "Raster")
    }
  })

  logger %>% writeLog(hlSpp(spN), 'Environmental data masked.')

  return(bgMask)
}
