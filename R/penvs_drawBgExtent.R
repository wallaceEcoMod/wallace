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
#' @return This functions returns a SpatialPolygonsDataFrame based on the user
#' specified coordinates (drawn on map). This SpatialPolygonsDataFrame may be
#' larger than specified if drawBgBuf > 0. The SpatialPolygonsDataFrame will
#' include all occurrences.

#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
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
  ptRem <- ifelse(all(!is.na(intersect)), 0, as.numeric(which(is.na(intersect))))
  if (ptRem == 0) {
    bgExt <- rgeos::gBuffer(newPoly, width = drawBgBuf)
    bgExt <- methods::as(bgExt, "SpatialPolygonsDataFrame")
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
