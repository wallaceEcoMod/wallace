# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# indic_eoo.R
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
#' @title  indic_eoo
#' @description Calculate EOO.
#' @details Calculate extent of occurrence (EOO) based on a binary raster or occurrence points.
#'
#' @param r raster. Thresholded prediction raster. It could be NULL if occs provided.
#' @param occs data.frame. Table with occurrences. It could be NULL if raster
#' provided. If occs provided raster is ignored.
#' @param lon character. Column name of longitude.
#' @param lat character. Column name of latitude.
#' @param wkt character. Well-known text representation of coordinate reference systems
#'  to calculate area.
#' @param logger stores all notification messages to be displayed in the Log
#'  Window of Wallace GUI. Insert the logger reactive list here for running in
#'  shiny, otherwise leave the default NULL
#'
#' @examples
#' \dontrun{
#' # binary raster
#' r <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif",package = "wallace"))
#' # occurrences
#' occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",package = "wallace"))
#' # wcea
#' wkt <- getWKT("wcea")
#'
#' eoo_r <- indic_eoo(r, occs = NULL, lon = NULL, lat = NULL, wkt, logger = NULL)
#' eoo_o <- indic_eoo(r = NULL, occs, lon = "longitude", lat = "latitude", wkt, logger = NULL)
#' }
#'
#' @return A list of two elements: area and eooPoly. area is a numeric value and eooPoly is a SpatialPolygon.
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @export
#'

indic_eoo <- function(r = NULL, occs = NULL, lon = NULL, lat = NULL,
                      wkt, logger=NULL) {
  if (!is.null(occs)) {
    if (is.null(lon) | is.null(lat)) {
      logger %>% writeLog(type = 'error',
                          "No longitude and/or latitude name provided.")
      return()
    } else {
      p.pts <- occs
      if (sum(c(lon, lat) %in% names(p.pts)) != 2) {
        logger %>% writeLog(type = 'error',
                            "Longitude and/or latitude names not found.")
        return()
      } else {
        p.pts <- p.pts %>% dplyr::select(all_of(c(lon, lat)))
      }
    }
  } else if (!is.null(r)) {
    if ("RasterLayer" %in% class(r)) {
    r <- terra::rast(r)
    }
    r[r == 0] <- NA
    p.pts <- terra::as.points(r) %>%
      terra::geom() %>% data.frame() %>%
      dplyr::select(tidyselect::all_of(c("x", "y")))
  } else {
    logger %>% writeLog(type = 'error',
      "Provide occurrences or raster.")
    return()
  }
  wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  # Create a minimum convex polygon around the occurrences
  eoo <- changeRangeR::mcp(p.pts, wgs84)
  # Project to WCEA (IUCN recommendation)
  eooPr <- terra::project(terra::vect(eoo), y = wkt)
  areaEOO <- terra::expanse(eooPr, unit = "km")
  return(list(area = areaEOO, eooPoly = eoo))
}
