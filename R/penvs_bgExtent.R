
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
#' @return A SpatialPolygonsDataFrame object that contains all occurrences from occs
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
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
    if (bgBuf == 0) {
      logger %>%
      writeLog(type = 'error',
               'Change buffer distance to positive or negative value.')
      return()
    }
    bgExt <- rgeos::gBuffer(occs.sp, width = bgBuf)
    msg <- paste0("Study extent: buffered points.  Buffered by ", bgBuf, " degrees.")
  }

  if (bgBuf > 0 & bgSel != 'point buffers') {
    bgExt <- rgeos::gBuffer(bgExt, width = bgBuf)
    logger %>% writeLog(hlSpp(spN), msg, ' Buffered by ', bgBuf, ' degrees.')
  } else {
    logger %>% writeLog(hlSpp(spN), msg)
  }
  bgExt <- methods::as(bgExt, "SpatialPolygonsDataFrame")
  return(bgExt)
}
