# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# indic_aoo.R
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

#' @title  indic_aoo
#' @description Calculate AOO.
#'
#' @details
#' The function calculates the area of occupancy (AOO) from a thresholded
#' prediction raster or a data.frame of occurrence records.
#'
#' @param r raster. Thresholded prediction raster. It could be NULL if occs provided.
#' @param occs data.frame. Table with occurrences. It could be NULL if raster
#' provided. If occs provided raster is ignored.
#' @param lon character. Column name of longitude.
#' @param lat character. Column name of latitude.
#' @param wktFrom character. Well-known text representation of coordinate reference systems
#'  of the provided data.
#' @param wktTo character. Well-known text representation of coordinate reference systems
#'  to calculate area.
#' @param logger stores all notification messages to be displayed in the Log
#'  Window of Wallace GUI. Insert the logger reactive list here for running in
#'  shiny, otherwise leave the default NULL
#'
#' @examples
#'  \dontrun{
#'  ### Set parameters
#'  # binary raster
#'  r <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif",
#'  package = "wallace"))
#'  # occurrences
#'
#'  ### Run function
#'  # aoo from raster
#'  aoo_r <- indic_aoo(r,
#'                     occs = NULL,
#'                     lon = NULL,
#'                     lat = NULL,
#'                     wktFrom = getWKT("wgs84"),
#'                     wktTo = getWKT("wcea"),
#'                     logger = NULL)
#'  # aoo from occs
#'  aoo_occs <- indic_aoo(r <- NULL,
#'                        occs,
#'                        lon = "longitude",
#'                        lat = "latitude",
#'                        wktFrom = getWKT("wgs84"),
#'                        wktTo = getWKT("wcea"),
#'                        logger = NULL)
#'  }
#'
#' @return A list of two elements: area and AOOraster. The first is the numeric
#' value of the area (in km^2). The second is a RasterLayer of the extent
#' (in WCEA & 2x2 km resolution).
#'
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @export
#'

indic_aoo <- function(r = NULL, occs = NULL, lon = NULL, lat = NULL,
                      wktFrom, wktTo, logger = NULL) {
  if (!is.null(occs)) {
    if (is.null(lon) | is.null(lat)) {
      logger %>% writeLog(type = 'error', "No longitude and/or latitude name provided.")
      return()
    } else {
      p.pts <- occs
      if (sum(c(lon, lat) %in% names(p.pts)) != 2) {
        logger %>% writeLog(type = 'error', "Longitude and/or latitude names not found.")
        return()
      } else {
        p.pts <- p.pts %>%
          dplyr::select(all_of(c(lon, lat))) %>% #BAJ
          terra::vect(geom = c(lon, lat))
        terra::crs(p.pts) <- wktFrom
        rast_temp <- terra::rast(terra::ext(terra::project(p.pts, wktTo)) + 10000,
                                 resolution = 2000, crs = wktTo)
        terra::origin(rast_temp) <- c(0,0)
        AOOraster <- terra::rasterize(terra::project(p.pts, wktTo), rast_temp,
                                       field = 1, update = TRUE) %>%
          terra::trim()
        terra::origin(AOOraster) <- c(0,0)
        AOOarea <- terra::freq(AOOraster, value = 1)$count * 4
      }
    }
  } else if (!is.null(r)) {
    if ("RasterLayer" %in% class(r)) {
    r <- terra::rast(r)
    }
    ## Unsuitable for NAs
    r[r == 0] <- NA
    p.poly <- terra::as.polygons(r)
    rast_temp <- terra::rast(terra::ext(terra::project(p.poly, wktTo)) + 10000,
                             resolution = 2000, crs = wktTo,
                             vals = 1)
    terra::origin(rast_temp) <- c(0,0)
    AOOraster <- terra::mask(rast_temp, terra::project(p.poly, wktTo)) %>%
      terra::trim()
    AOOarea <- terra::freq(AOOraster, value = 1)$count * 4
  } else {
    logger %>% writeLog(type = 'error', "Provide occurrences or raster.")
    return()
  }
  return(list(area = AOOarea, AOOraster = raster::raster(AOOraster)))
}
