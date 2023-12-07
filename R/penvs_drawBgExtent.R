# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# penvs_drawBgExtent.R
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
#' @title penvs_drawBgExtent: Draw background extent
#' @description This function generates a background area according to a user
#'   drawn polygon and provided buffer.
#'
#' @details
#' This function is used in the select study region component. Here, in the GUI,
#'   the user draws a polygon to be used as the background extent and may
#'   include a buffer to the given polygon. The buffered poylgon must include
#'   all occurrences (occs) or function will return an error. The function
#'   returns a SpatialPolygonsDataFrame object of the desired extent (+ buffer).
#'
#' @param polyExtXY coordinates of polygon endpoints obtained from user drawn
#'   polygon in GUI.
#' @param polyExtID numeric. ID to be used in the generation of the polygon.
#' @param drawBgBuf the buffer to be used in generating the
#'   SpatialPolygonsDataFrame, maybe be 0 or >0. A number must be specified.
#' @param occs data frame of cleaned or processed occurrences obtained from
#'   components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param logger Stores all notification messages to be displayed in the
#'   Log Window of Wallace GUI. Insert the logger reactive list here for
#'   running in shiny, otherwise leave the default NULL.
#' @param spN data frame of cleaned occurrences obtained from component
#'   occs: Obtain occurrence data. Used to obtain species name for logger
#'   messages.
#'
#' @examples
#' occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
#'                  package = "wallace"))[, 2:3]
#' occs$occID <- 1:nrow(occs)
#' longitude <- c(-27.78641, -74.09170, -84.01930, -129.74867,
#'                -142.19085, -45.55045, -28.56050)
#' latitude <- c(-40.40539, -37.02010, 2.28455, 40.75350,
#'               56.35954, 54.55045, -7.11861)
#' expertDrawPoly <- matrix(c(longitude, latitude), byrow = FALSE,
#'                          ncol = 2)
#' drawBgBf <- penvs_drawBgExtent(polyExtXY = expertDrawPoly, polyExtID = 1,
#'                                drawBgBuf = 0.5, occs)
#' @return This functions returns a SpatialPolygons object based on the user
#' specified coordinates (drawn on map). This SpatialPolygons object may be
#' larger than specified if drawBgBuf > 0. The SpatialPolygons object will
#' include all occurrences.

#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
# @note
#' @seealso \code{\link{penvs_userBgExtent}}, \code{\link{penvs_bgExtent}},
#' \code{\link{penvs_bgMask}} , \code{\link{penvs_bgSample}}
#' @export

penvs_drawBgExtent <- function(polyExtXY, polyExtID, drawBgBuf, occs,
                               logger = NULL, spN = NULL) {
  ptRem <- NULL
  occs.xy <- occs[c('longitude', 'latitude')]
  # make spatial pts object of original occs and preserve origID
  pts <- sp::SpatialPointsDataFrame(occs.xy, data = occs['occID'])
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyExtXY)),
                                                   ID = polyExtID)))
  intersect <- sp::over(pts, newPoly)
  newPoly.sf <- sf::st_as_sf(newPoly)

  ptRem <- ifelse(all(!is.na(intersect)), 0, as.numeric(which(is.na(intersect))))
  if (ptRem == 0) {
    bgExt <- sf::st_buffer(newPoly.sf, dist = drawBgBuf)
    bgExt <- sf::as_Spatial(bgExt)
    if (drawBgBuf == 0) {
      logger %>% writeLog(hlSpp(spN), 'Draw polygon without buffer.')
    } else {
      logger %>% writeLog(hlSpp(spN), 'Draw polygon with buffer of ',
                          drawBgBuf, ' degrees.')
    }
    return(bgExt)
  } else if (ptRem > 0) {
    logger %>%
      writeLog(type = 'error', hlSpp(spN),
               "The drawn polygon did not include all localities. ",
               "Remove the polygon before drawing a new one.")
    return()
  }
}
