# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# penvs_bgExtent.R
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

#' @title penvs_bgExtent Generate background extent
#' @description This function generates a background area according to a user-
#'   provided method.
#'
#' @details This function is used in the select study region component. Here,
#'   the user can select between three methods ('bounding box', 'point buffers'
#'   or ' minimum convex polygon') to determine the background extent based on the
#'   observed occurrences. The function returns a SpatialPolygonsDataFrame
#'   object of the desired extent.
#'
#' @param occs data frame of cleaned or processed occurrences obtained from
#'   components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param bgSel character. Method of background building. Must be one of three
#'   options: 'bounding box' , 'point buffers' or ' minimum convex polygon'.
#' @param bgBuf numeric. Buffer distance in degrees to be used in the building
#'   of the background area.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL.
#' @param spN data frame of cleaned occurrences obtained from component occs:
#'   Obtain occurrence data. Used to obtain species name for logger messages.
#' @examples
#' occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
#'                  package = "wallace"))[, 2:3]
#' occs$occID <- 1:nrow(occs)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#'
#' @return A SpatialPolygons object that contains all occurrences from occs
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
# @note

#' @seealso \code{\link{penvs_userBgExtent}}, \code{\link{penvs_drawBgExtent}},
#'   \code{\link{penvs_bgMask}} , \code{\link{penvs_bgSample}}
#' @export
#'

penvs_bgExtent <- function(occs, bgSel, bgBuf, logger = NULL, spN = NULL) {

  if (nrow(occs) <= 2) {
    logger %>%
      writeLog(type = 'error',
               'Too few localities (<2) to create a background polygon.')
    return()
  }

  # extract just coordinates
  occs.xy <- occs[c('longitude', 'latitude')]

  # make spatial pts object of original occs and preserve origID
  occs.sp <- sp::SpatialPointsDataFrame(occs.xy, data = occs['occID'])

  # make an sf obj
  occs.sf <- sf::st_as_sf(occs.xy, coords = c("longitude", "latitude"))
  occs.sf <- sf::st_union(occs.sf, by_feature = FALSE)

  # generate background extent - one grid cell is added to perimeter of each shape
  # to ensure cells of points on border are included
  if (bgSel == 'bounding box') {
    xmin <- occs.sp@bbox[1]
    xmax <- occs.sp@bbox[3]
    ymin <- occs.sp@bbox[2]
    ymax <- occs.sp@bbox[4]
    bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin),
                 ncol = 2)
    bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(bb)), 1)))
    msg <- "Study extent: bounding box."
  } else if (bgSel == "minimum convex polygon") {
    mcp.xy <- as.data.frame(sp::coordinates(occs.xy))
    coords.t <- grDevices::chull(mcp.xy[, 1], mcp.xy[, 2])
    xy.bord <- mcp.xy[coords.t, ]
    xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
    bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(as.matrix(xy.bord))), 1)))
    msg <- "Study extent: minimum convex polygon."
  } else if (bgSel == 'point buffers') {
    if (bgBuf <= 0) {
      logger %>%
      writeLog(type = 'error',
               'Change buffer distance to a positive value.')
      return()
    }
    bgExt <- sf::st_buffer(occs.sf, dist = bgBuf)
    msg <- paste0("Study extent: buffered points.  Buffered by ", bgBuf, " degrees.")
  }

  if (bgBuf >= 0 & bgSel != 'point buffers') {
    bgExt <- sf::st_as_sf(bgExt)
    bgExt <- sf::st_buffer(bgExt, dist = bgBuf)
    logger %>% writeLog(hlSpp(spN), msg, ' Buffered by ', bgBuf, ' degrees.')
  } else if (bgBuf < 0 & bgSel != 'point buffers') {
    logger %>%
      writeLog(type = 'error',
               'All localities must be included within extent.
               Change buffer distance to a positive value.')
    return()
  } else {
    logger %>% writeLog(hlSpp(spN), msg)
  }
  bgExt <- sf::as_Spatial(bgExt)
  return(bgExt)
}
