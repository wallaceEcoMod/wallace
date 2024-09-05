# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# indic_raster.R
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
#' @title  indic_raster
#' @description Upload user-specified SDM prediction
#' @details This function uploads a user-specified raster to be used in the calculate ratio
#' overlap module. It checks to make sure the raster intersects the overlapping object(an sf object;
#' a range map, EOO, AOO, masked map, etc) for calculating ratio overlap
#'
#' @param rasPath character of path to raster, must be the full path including
#'  file name and extension.
#' @param rasName character vector of raster name to be assigned to loaded
#'  raster.
#' @param overlapArea x. An sf object.
#' @param logger stores all notification messages to be displayed in the
#'  Log Window of Wallace GUI. insert the logger reactive list here for
#'  running in shiny, otherwise leave the default NULL.
#' @param spN Species name
#'
#' @example
#' \dontrun{
#' #raster
#' rasPath <- system.file("extdata/Bassaricyon_neblina.tif",package = "wallace")
#' rasName <- "Bassaricyon_neblina.tif"
#' #overlapArea
#' rangeMap <- raster::raster(nrows=108, ncols=108, xmn=-79, xmx=-73)
#' raster::values(rangeMap)<- runif(n = (108*108))
#' ##convert to sf
#' rangeMap <- terra::rast(rangeMap)
#' rangeMap[rangeMap == 0] <- NA
#' rangeMap <- terra::as.polygons(rangeMap)
#' rangeMap <- sf::st_as_sf(rangeMap)
#'
#' # Run function
#' r <- indic_raster(rasPath, rasName, overlapArea = rangeMap, logger = NULL, spN = NULL)
#' }
#'
#' @return A rasterlayer
#'
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @seealso \code{\link{indic_overlap}}
#' @export

indic_raster <- function(rasPath, rasName, overlapArea,
                         logger = NULL, spN = NULL) {
  rasterName <- fileNameNoExt(rasName)
  smartProgress(logger, message = "Uploading user-specified raster...", {
    r <- raster::raster(rasPath)
    r <- raster::trim(r)
    names(r) <- "overlapRaster"
    extPoly <- raster::extent(r)
    if (extPoly@xmin < -180 | extPoly@xmax > 180 |
        extPoly@ymin < -90 | extPoly@ymax > 90) {
      logger %>%
        writeLog(
          type = "error", hlSpp(spN),
          "Wrong extent projection. '", rasterName,
          "' cannot be uploaded.")
      return()
    }
    if (sum(lengths(sf::st_intersects(sf::st_as_sfc(sf::st_bbox(r)),
                                      overlapArea))) == 0) {
      logger %>% writeLog(
        type = 'error', hlSpp(spN),
        "Overlap raster does not match with range map extent. ",
        "Please specify a new raster."
      )
      return()
    }
  })
  logger %>% writeLog(hlSpp(spN), "User raster file loaded.")
  return(r)
}
