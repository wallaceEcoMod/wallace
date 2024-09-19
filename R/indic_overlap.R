# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# indic_overlap.R
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
#' @title indic_overlap
#' @description Overlap a binary range map with shapefile or raster
#' @details This function calculates the overlap between a range map and a
#' supplied shape or continuous raster. If the inputOverlap is a continuous raster, the
#' function returns a polygon of the overlap and the proportion of range:
#' below 25%, 25-50%, 50-75%, and 75-100%. If the inputOverlap is a polygon, the
#' function returns a polygon of the overlap and the percentage of the range
#' contained within the shape.
#' @param rangeMap sf object representing a binary range map.
#' @param inputOverlap sf polygon or continuous raster. A feature to apply overlap.
#' @param field character. Name of the field to select categories/attributes.
#' NULL if inputOverlap is a raster.
#' @param category character. String with names of the categories/attributes
#' selected to overlap with range map. NULL if inputOverlap is a raster.
#' @param logger logger
#' @param spN species name
#'
#' @examples
#' \dontrun{
#' ### Set parameters
#' # range map
#' rangeMap <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif", package = "wallace"))
#' rangeMap[rangeMap == 0] <- NA
#' rangeMap <- terra::as.polygons(rangeMap)
#' rangeMap <- sf::st_as_sf(rangeMap)
#' # input overlap raster
#' inputRaster <- raster::raster(nrows=108, ncols=108, xmn=-80, xmx=-75)
#' raster::values(inputRaster)<- runif(n = (108*108))
#'
#' ### Run function
#' overlap_r <- indic_overlap(rangeMap, inputOverlap = inputRaster, field = NULL, category = NULL, logger = NULL, spN = NULL)
#' }
#'
#' @return A list of 2: a polygon of the overlap and the ratio overlap statistics.
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @seealso \code{\link{indic_raster}}, \code{\link{indic_inputPoly}}, \code{\link[changeRangeR]{ratioOverlap}}
#' @export
#'

indic_overlap <- function(rangeMap, inputOverlap, field = NULL,
                          category = NULL, logger = NULL, spN = NULL) {
  if ("sf" %in% class(inputOverlap)) {
    # GEPB: Uncomment when subfield in changeRangeR::ratioOverlap is working
    # catAv <- unique(inputOverlap[[field]])
    # if (length(catAv) == length(category)) {
    #   categoryUse <- "All"
    # } else {
    #   categoryUse <- category
    # }
    smartProgress(
      logger,
      message = "Calculating range overlap ", {
        sf::sf_use_s2(FALSE)
        ratioOverlap <- changeRangeR::ratioOverlap(
          r = rangeMap,
          shp = inputOverlap,
          field = field,
          category = category)
        simpRangeMap <- sf::st_union(rangeMap)
        simpInputOverlap <- sf::st_union(
          subset(inputOverlap, inputOverlap[[field]] %in% category))
        overlapPolygon <- sf::st_intersection(simpRangeMap, simpInputOverlap)
      }
    )
  } else if ("RasterLayer" %in% class(inputOverlap)) {
    smartProgress(
      logger,
      message = "Calculating range overlap ", {
        ratioOverlap <- changeRangeR::ratioOverlap(
          r = sf::as_Spatial(rangeMap),
          shp = inputOverlap)
        # GEPB: Not sure when this scenario occurs.
        # if (is.list(maskedRange) & length(maskedRange) > 1) {
        #   names(maskedRange) <- NULL
        #   maskedRange$fun <- mean
        #   maskedRange$na.rm <- TRUE
        #   maskedRange <- do.call(raster::mosaic, maskedRange)
        # }
        simpRangeMap <- sf::st_union(rangeMap)
        simpInputOverlap <- terra::rast(inputOverlap)
        simpInputOverlap[!is.na(simpInputOverlap)] <- 1
        simpInputOverlap <- terra::as.polygons(simpInputOverlap) %>% sf::st_as_sf()
        overlapPolygon <- sf::st_intersection(simpRangeMap, simpInputOverlap)
      }
    )
  }
  return(list(overlapPolygon = overlapPolygon,
              overlapRatio = ratioOverlap$ratio))
}
