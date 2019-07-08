
#' @title c8_projectDraw
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param polyPjXY x
#' @param polyPjID x
#' @param drawBgBuf x
#' @param shinyLogs x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Gonzalo Pinilla gpinillabuitrago@@gradcenter.cuny.edu
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

#' @export

c8_projectDraw <- function(polyPjXY, polyPjID, drawPjBuf, shinyLogs = NULL) {
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyPjXY)),
                                                   ID = polyPjID)))
  bgExt <- rgeos::gBuffer(newPoly, width = drawPjBuf)
  bgExt <- sp::SpatialPolygonsDataFrame(bgExt, data = data.frame(x = 1),
                                        match.ID = FALSE)
  return(bgExt)
}
