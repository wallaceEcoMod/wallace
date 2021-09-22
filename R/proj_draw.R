
#' @title proj_draw Draw projection extent
#' @description This function creates a polygon object from coordinates of user drawn poylgon in the GUI.
#'
#' @details
#' This function is used in the project model component. In the GUI, the user draws a polygon to be used
#' as the projection extent and may include a buffer to the given polygon.
#' The function returns a SpatialPolygonsDataFrame object of the desired extent (+ buffer).

#' @param polyPjXY coordinates of polygon endpoints obtained from user drawn polygon
#' @param polyPjID numeric .ID to be used in the generation of the polygon
#' @param drawBgBuf the buffer to be used in generating the SpatialPolygonsDataFrame, must be >=0 . A number must be specified.
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny, otherwise leave the default NULL
#' @param spN character. Used to obtain species name for logger messages
# @keywords
#'
#' @examples
#'longitude <- c(-27.78641, -74.09170, -84.01930, -129.74867, -142.19085, -45.55045, -28.56050)
#'latitude <- c(-40.40539, -37.02010, 2.28455, 40.75350, 56.35954, 54.55045, -7.11861)
#'userDrawPoly <- matrix(c(longitude, latitude), byrow = F, ncol = 2)
#'drawPjBuf = 0.5
#' polyPjID=1
#' polygonTest<-proj_draw(polyPjXY=userDrawPoly, polyPjID, drawPjBuf, logger = NULL)
#'
#' @return This functions returns a SpatialPolygonsDataFrame based on the user specified coordinates (drawn on map).
#' This SpatialPolygonsDataFrame may be larger than specified if drawBgBuf > 0.

#' @author Gonzalo Pinilla <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
#' @seealso  \code{\link{proj_userEnvs}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

#' @export

proj_draw <- function(polyPjXY, polyPjID, drawPjBuf, logger = NULL, spN = NULL) {
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyPjXY)),
                                                   ID = polyPjID)))
  bgExt <- rgeos::gBuffer(newPoly, width = drawPjBuf)
  bgExt <- as(bgExt, "SpatialPolygonsDataFrame")
  if (drawPjBuf == 0) {
    logger %>% writeLog(hlSpp(spN), 'Draw polygon without buffer.')
  } else {
    logger %>% writeLog(hlSpp(spN), 'Draw polygon with buffer of ',
                        drawPjBuf, ' degrees.')
  }
  return(bgExt)
}
