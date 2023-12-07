# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# xfer_userExtent.R
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
#' @title xfer_userExtent: user provided extent of transfer
#' @description This function generates an area of transfer according to a user
#'   provided polygon and buffer.
#'
#' @details
#' This function is used in the transfer component. Here, the user provides
#'   either a shapefile or a csv with vertex coordinates with the desired shape
#'   for the extent of transfer, the user may include a buffer to the given
#'   polygon. The function returns a SpatialPolygons object of the desired
#'   extent (+ buffer).
#'
#' @param bgShp_path path to the user provided shapefile or csv with
#'   vertex coordinates.
#' @param bgShp_name name of the user provided shapefile or csv with
#'   vertex coordinates.
#' @param userBgBuf numeric. Buffer to be used in creating the background
#'   extent must be >= 0.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL.
#' @param spN data frame of cleaned occurrences obtained from component
#'   occs: Obtain occurrence data. Used to obtain species name for logger
#'   messages.
#' @examples
#' pathShp <- list.files(system.file("extdata/shp", package = "wallace"),
#'                       full.names = TRUE)
#' nameShp <- list.files(system.file("extdata/shp", package = "wallace"),
#'                       full.names = FALSE)
#' xferUser <- xfer_userExtent(bgShp_path = pathShp, bgShp_name = nameShp,
#'                             userBgBuf = 1)
#' @return This function returns a SpatialPolygons object with the user
#'   provided shape (+ a buffer is userBgBuf >0).
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @seealso \code{\link{penvs_drawBgExtent}}, \code{\link{penvs_bgExtent}},
#'   \code{\link{penvs_bgMask}} , \code{\link{penvs_bgSample}}
#' @export

xfer_userExtent <- function(bgShp_path, bgShp_name, userBgBuf,
                            logger = NULL, spN = NULL) {
  pathdir <- dirname(bgShp_path)
  pathfile <- basename(bgShp_path)
  # get extensions of all input files
  exts <- sapply(strsplit(bgShp_name, '\\.'), FUN = function(x) x[2])
  if (length(exts) == 1 & exts[1] == 'csv') {
    f <- utils::read.csv(bgShp_path, header = TRUE)
    bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(f)), 1)))
  } else if ('shp' %in% exts) {
    if (length(exts) < 3) {
      logger %>%
        writeLog(type = 'error',
                 paste0('If entering a shapefile, please select all the ',
                        'following files: .shp, .shx, .dbf.'))
      return()
    }
    # get index of .shp
    i <- which(exts == 'shp')
    if (!file.exists(file.path(pathdir, bgShp_name)[i])) {
      file.rename(bgShp_path, file.path(pathdir, bgShp_name))
    }
    # read in shapefile and extract coords
    bgExt <- sf::st_read(file.path(pathdir, bgShp_name)[i])
    bgExt <- sf::as_Spatial(bgExt)

  } else {
    logger %>%
      writeLog(type = 'error',
               paste0('Please enter either a CSV file of vertex coordinates ',
                      'or shapefile (.shp, .shx, .dbf).'))
    return()
  }

  if (userBgBuf > 0) {
    bgExt <- sf::st_as_sf(bgExt)
    bgExt <- sf::st_buffer(bgExt, dist = userBgBuf)
    bgExt <- sf::as_Spatial(bgExt)
    logger %>% writeLog(
      hlSpp(spN),
      'Transferring extent user-defined polygon buffered by ',
      userBgBuf, ' degrees.')
  } else if (userBgBuf < 0) {
    logger %>%
      writeLog(type = 'error',
               'Change buffer distance to a positive value.')
    return()
  } else {
    logger %>% writeLog(
      hlSpp(spN),
      "Transferring extent: user-defined polygon.")
  }
  return(bgExt)
}
