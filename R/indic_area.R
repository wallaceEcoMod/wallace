# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# indic_area.R
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
#' @title  indic_area
#' @description Calculate Range size in square kilometers.
#' @details Calculate the geographic range size of a thresholded raster. The units are in square kilometers.
#'
#' @param r raster. Thresholded prediction raster. It could be NULL if occs provided.
#' @param wkt character. Well-known text representation of coordinate reference systems
#'  to calculate area.
#' @param logger stores all notification messages to be displayed in the Log
#'  Window of Wallace GUI. insert the logger reactive list here for running in
#'  shiny, otherwise leave the default NULL
#'
#' @examples
#' \dontrun{
#' r <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif",package = "wallace"))
#' wkt <- getWKT("wcea")
#' areaRange <- indic_area(r, wkt, logger = NULL)
#' }
#' @return Numeric value
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @export
#'

indic_area <- function(r, wkt, logger = NULL) {
  if ("RasterLayer" %in% class(r)) {
  r <- terra::rast(r)
  }
  ## Unsuitable for NAs
  r[r == 0] <- NA
  ## Raster to polygon
  p <- terra::as.polygons(r)
  ## Project to SR-ORG 8287
  p <- terra::project(p, y = wkt)
  areaRange <- terra::expanse(p, unit = "km")
  return(areaRange)
}
