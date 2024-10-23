# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# mask_spatialPoly.R
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
#' @title mask_spatialPoly
#' @description Upload user-provided shapefile for masking
#'
#' @details This function provides a spatialpolygon from a shapefile for masking
#'  a continuous or binary range prediction (sdm) based on the path and name of
#'  a user-provided shapefile. The shapefile must fall within the extent of the
#'  range prediction. Any part of the range prediction (sdm) outside of the polygon
#'  will be removed ("masked" out).
#'
#' @param bgShp_path Path to the user-provided shapefile for masking
#' @param bgShp_name Name of the user-provided shapefile for masking
#' @param sdm A rasterlayer of a continuous or binary range prediction to be masked by shape
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#' otherwise leave the default NULL
#' @param spN character. Used to obtain species name for logger messages
#'
#' @examples
#' \dontrun{
#' ### Set parameters
#' bgShp_path <- list.files(path = system.file("extdata/shp", package = "wallace"),full.names = TRUE)
#' bgShp_name <- list.files(path = system.file("extdata/shp", package = "wallace"),full.names = FALSE)
#' sdm <- raster::raster(system.file("extdata/Bassaricyon_neblina.tif",package = "wallace"))
#' ### Run function
#' spatialMask <- mask_spatialPoly(bgShp_path,bgShp_name,sdm,logger = NULL,spN = NULL)
#' }
#' @return A SpatialPolygonsDataFrame for masking
#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @export

mask_spatialPoly <- function(bgShp_path, bgShp_name, sdm,
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
      polyData <- sf::st_read(file.path(pathdir, bgShp_name)[i])
      polyData <- sf::as_Spatial(polyData)
    })

  } else {
    logger %>%
      writeLog(type = 'error',
               paste0('Please enter shapefile: .shp, .shx, & .dbf.'))
    return()
  }

  if (is.na(raster::crs(polyData, asText = TRUE))) {
    logger %>% writeLog(
      type = 'warning', hlSpp(spN),
      "Projection not found for shapefile. It is assumed that shapefile datum ",
      "is WGS84. "
    )
  }

  sdm_ext <- methods::as(raster::extent(sdm), 'SpatialPolygons')
  sdm_sfc <- sf::st_as_sfc(sdm_ext) #sdm extent to sfc
  polyData_sfc <- sf::st_as_sfc(polyData) #convert polyData to sfc
  if (!sf::st_intersects(sdm_sfc, polyData_sfc, sparse = FALSE)[1,1]) {
    logger %>% writeLog(
      type = 'error', hlSpp(spN),
      "Shapefile must fall within the SDM extent. Please specify a new polygon. "
    )
    return()
  }

  smartProgress(logger, message = "Intersecting spatial data ...", {
    sdm <- terra::rast(sdm)
    sdm <- sdm >= 0

    polR <- sf::st_as_sf(terra::as.polygons(sdm, trunc = TRUE, dissolve = TRUE,
                                            values = TRUE),
                         as_points = FALSE, merge = TRUE)
   # polyData <- rgeos::gBuffer(polyData, byid = TRUE, width = 0)
    polyData <- sf::st_as_sf(polyData)
    polyData <- sf::st_buffer(polyData, dist = 0) # BAJ not sure why buffer of 0 is needed?
    polyData <- replace(polyData, is.na(polyData), values = "NA")
    # Check for NAs
    v <- terra::extract(sdm, polyData)
    v <- is.na(unlist(v))

    if (sum(v) == length(v)) {
      logger %>% writeLog(
        type = 'error', hlSpp(spN),
        paste("The polygon just included NA values.",
              "Please, select a polygon that intersects model prediction.(**)")
      )
      return()
    }

    if (sum(v) > 0) {
      logger %>% writeLog(
        type = 'warning', hlSpp(spN),
        paste("The polygon selected included some cells with NA values.",
              "You cannot change the predictions (suitable or unsuitable),",
              "in these cells. ")
      )
    }
    sf::st_crs(polyData) <- sf::st_crs(polR)
    spatialPoly <- sf::as_Spatial(sf::st_intersection(sf::st_make_valid(polyData),
                                                      sf::st_make_valid(polR)))
  })
  logger %>% writeLog(hlSpp(spN), "Spatial data uploaded.")
  return(spatialPoly)
}
