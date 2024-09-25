# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# indic_time.R
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
#' @title indic_time
#' @description Calculate change in range area through time
#' @details This function calls on the changeRangeR::envchange() to calculate
#' the change in range area over time. It calculates the area of polygon of a
#' binary range, a masked range, EOO, or AOO, over time based on environmental
#' variables uploaded by the user and a selected threshold. The bounds of the
#' threshold are selected: "upper" removes values above the threshold (e.g.,
#' maximum human footprint), "lower" removes values below the threshold (e.g.,
#' minimum forest cover), "neither" does not threshold at any point, & finally
#' "both" thresholds at both threshold values (if provided; e.g., minimum and
#' maximum temperature).
#' @param range sf polygon. A polygon with the range map. Extent must match envs.
#' @param envs rasterStack. Environmental variables. Extent must match range.
#' @param thrh integer. Threshold of where envs layers
#'  should be thresholded
#' @param bound character. character string characterizing the way the threshold
#'  should happen. "upper" removes values above the threshold (e.g., maximum
#'  human footprint). "lower" removes values below the threshold (e.g., minimum
#'  forest cover). "neither" does not threshold at any point. "both" thresholds
#'  at both threshold values (if provided; e.g., minimum and maximum temperature).
#' @param logger logger
#' @param spN species name
#' @examples
#' \dontrun{
#' ### Set parameters
#' # rasterstack of envs
#' pathRast <- list.files(system.file("extdata/MODIS", package = "wallace"), pattern = ".tif$", full.names = TRUE)
#' envs <- raster::stack(pathRast)
#' # range map
#' range <- raster::raster(system.file("extdata/Bassaricyon_neblina.tif",package = "wallace"))
#' # match projections
#' projected_range <- raster::projectRaster(range, envs, method = 'bilinear')
#' # threshold & bounds
#' thrh <- 20
#' bound <- "lower"
#' ### Run function
#' envChangeArea <- indic_time(projected_range, envs, thrh, bound, logger = NULL, spN = NULL)
#' }
#' @return a matrix array of the area calculations through time
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @seealso \code{\link[changeRangeR]{envChange}}
#' @export
#'

indic_time <- function(range, envs, thrh, bound, logger = NULL, spN = NULL) {
  smartProgress(
    logger,
    message = "Calculating area indic through time ", {
      ##run function
      rangeTime <- changeRangeR::envChange(
        rStack = envs,
        binaryRange = range,
        threshold = thrh,
        bound = bound)
    })
  logger %>% writeLog(hlSpp(spN),
                      "Range area after masking for environmental variables ",
                      "through time calculation done. ")
  return(rangeTime$Area)
}

