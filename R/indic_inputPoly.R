# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# indic_inputPoly.R
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
#' @title indic_inputPoly user-provided shapefile for ratio overlap calculations
#' @description This function allows the user to upload a shapefile to be used in ratio overlap calculations
#'
#' @details
#' This function is used in the Calculate Indicators component. Here, the user
#'   provides the path and name of a shapefile to be used with the source polygon
#'   to calculate the ratio of overlap between the polygons. The source polygon
#'   is a polygonized raster of a range map from: a Wallace SDM (prediction,
#'   transfer, or masked), EOO, or AOO. The user shapefile and source polygon must
#'   intercept. The function returns an sf object to be used in the indic_overlap()
#'   function.
#'
#' @param bgShp_path Path to the user provided shapefile
#' @param bgShp_name Name of the user provided shapefile
#' @param overlapArea sf object. The source polygon for ratio overlap calculations
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#' otherwise leave the default NULL
#' @param spN character. Used to obtain species name for logger messages
#' @example
#' \dontrun{
#' # Overlap polygon
#' bgShp_path <- list.files(path = system.file("extdata/shp", package = "wallace"), full.names = TRUE)
#' bgShp_name <- list.files(path = system.file("extdata/shp", package = "wallace"), full.names = FALSE)
#'
#' # Source polygon
#' r <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif", package = "wallace"))
#' r[r == 0] <- NA
#' r <- terra::as.polygons(r)
#' overlapArea <- sf::st_as_sf(r)
#'
#' # Run function
#' spatialPoly <- indic_inputPoly(bgShp_path, bgShp_name, overlapArea, logger = NULL, spN = NULL)
#' }
#'
#' @return An sf object of two variables: a dummy integer and an sfc multipolygon of the overlap
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
# @note

#' @seealso \code{\link{indic_overlap}}
#' @export

indic_inputPoly <- function(bgShp_path, bgShp_name, overlapArea,
                             logger = NULL, spN = NULL) {
  pathdir <- dirname(bgShp_path)
  pathfile <- basename(bgShp_path)
  # get extensions of all input files
  exts <- sapply(strsplit(bgShp_name, '\\.'), FUN = function(x) x[2])
  if ('shp' %in% exts) {
    if (length(exts) < 3) {
      logger %>%
        writeLog(type = 'error', hlSpp(spN),
                 paste0('If entering a shapefile, please select all the ',
                        'following files: .shp, .shx, .dbf.'))
      return()
    }
    # get index of .shp
    i <- which(exts == 'shp')
    if (!file.exists(file.path(pathdir, bgShp_name)[i])) {
      file.rename(bgShp_path, file.path(pathdir, bgShp_name))
    }
    smartProgress(logger, message = "Uploading shapefile ...", {
      polyData <- sf::read_sf(file.path(pathdir, bgShp_name)[i])
      polyData <- replace(polyData, is.na(polyData), values = "NA")
    })
  } else {
    logger %>%
      writeLog(type = 'error', hlSpp(spN),
               'Please enter shapefile (.shp, .shx, .dbf).')
    return()
  }
  if (is.na(sf::st_crs(polyData))) {
    logger %>% writeLog(
      type = 'warning', hlSpp(spN),
      "Projection not found for shapefile. It is assumed that shapefile datum is WGS84."
    )
    sf::st_crs(polyData) <- 4326
  }
  if (sf::st_crs(polyData)$input != "EPSG:4326") {
    polyData <- sf::st_transform(polyData, 4326)
    logger %>% writeLog(
      type = 'warning', hlSpp(spN),
      "Original coordinate reference system (CRS) is not WGS84 (EPSG:4326). ",
      "Shapefile was reprojected to this CRS."
    )
  }
  sf::sf_use_s2(FALSE)
  if (sum(lengths(sf::st_intersects(polyData, overlapArea))) == 0) {
    logger %>% writeLog(
      type = 'error', hlSpp(spN),
      "Shapefile does not intersect the area to overlap. Please specify a new polygon."
    )
    return()
  }
  smartProgress(logger, message = "Intersecting spatial data ...", {
    nmOverlap <- names(overlapArea)
    nmOverlap <- nmOverlap[!(nmOverlap %in% "geometry")]
    spatialPoly <- sf::st_intersection(polyData, overlapArea) %>%
      dplyr::select(all_of(nmOverlap))
  })
  logger %>% writeLog(hlSpp(spN), "Spatial data uploaded.")
  return(spatialPoly)
}
