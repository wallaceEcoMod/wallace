# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# xfer_draw.R
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

#' @title xfer_draw Draw extent of transfer
#' @description This function creates a polygon object from coordinates of user
#'   drawn poylgon in the GUI.
#'
#' @details
#' This function is used in the transfer model component. In the GUI, the user
#'   draws a polygon to be used as the extent of transfer and may include a
#'   buffer to the given polygon. The function returns a
#'   SpatialPolygonsDataFrame object of the desired extent (+ buffer).
#' @param polyXfXY coordinates of polygon endpoints obtained from user
#'   drawn polygon
#' @param polyXfID numeric .ID to be used in the generation of the polygon
#' @param drawXfBuf the buffer to be used in generating the
#'   SpatialPolygonsDataFrame, must be >=0 . A number must be specified.
#' @param logger Stores all notification messages to be displayed in the
#'   Log Window of Wallace GUI. Insert the logger reactive list here for
#'   running in shiny, otherwise leave the default NULL
#' @param spN character. Used to obtain species name for logger messages
#' @examples
#' longitude <- c(-27.78641, -74.09170, -84.01930, -129.74867,
#'                -142.19085, -45.55045, -28.56050)
#' latitude <- c(-40.40539, -37.02010, 2.28455, 40.75350,
#'               56.35954, 54.55045, -7.11861)
#' userDrawPoly <- matrix(c(longitude, latitude), byrow = FALSE,
#'                        ncol = 2)
#' drawXfBuf <- 0.5
#' polyXfID <- 1
#' polygonTest <- xfer_draw(polyXfXY = userDrawPoly, polyXfID,
#'                          drawXfBuf)
#'
#' @return This functions returns a SpatialPolygons object based on the user
#'   specified coordinates (drawn on map). This SpatialPolygonsDataFrame may be
#'   larger than specified if drawBgBuf > 0.

#' @author Gonzalo Pinilla <gepinillab@@gmail.com>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
# @note
#' @seealso  \code{\link{xfer_userEnvs}}
#' @export

xfer_draw <- function(polyXfXY, polyXfID, drawXfBuf, logger = NULL, spN = NULL) {
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyXfXY)),
                                                   ID = polyXfID)))
  newPoly.sf <- sf::st_as_sf(newPoly)
  bgExt <- sf::st_buffer(newPoly.sf, dist = drawXfBuf)
  bgExt <- sf::as_Spatial(bgExt)
  if (drawXfBuf == 0) {
    logger %>% writeLog(hlSpp(spN), 'Draw polygon without buffer.')
  } else {
    logger %>% writeLog(hlSpp(spN), 'Draw polygon with buffer of ',
                        drawXfBuf, ' degrees.')
  }
  return(bgExt)
}
