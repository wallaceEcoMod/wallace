
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
#' @return This functions returns a SpatialPolygonsDataFrame based on the user
#'   specified coordinates (drawn on map). This SpatialPolygonsDataFrame may be
#'   larger than specified if drawBgBuf > 0.

#' @author Gonzalo Pinilla <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
#' @seealso  \code{\link{xfer_userEnvs}}
#' @export

xfer_draw <- function(polyXfXY, polyXfID, drawXfBuf, logger = NULL, spN = NULL) {
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyXfXY)),
                                                   ID = polyXfID)))
  bgExt <- rgeos::gBuffer(newPoly, width = drawXfBuf)
  bgExt <- methods::as(bgExt, "SpatialPolygonsDataFrame")
  if (drawXfBuf == 0) {
    logger %>% writeLog(hlSpp(spN), 'Draw polygon without buffer.')
  } else {
    logger %>% writeLog(hlSpp(spN), 'Draw polygon with buffer of ',
                        drawXfBuf, ' degrees.')
  }
  return(bgExt)
}
