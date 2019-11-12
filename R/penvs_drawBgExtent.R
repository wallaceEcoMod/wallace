
#' @title penvs_drawBgExtent
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param polyExtXY x
#' @param polyExtID x
#' @param drawBgBuf x
#' @param occs x
#' @param logger x
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

penvs_drawBgExtent <- function(polyExtXY, polyExtID, drawBgBuf, occs,
                               logger = NULL) {
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
    if (drawBgBuf == 0 ) {
      logger %>% writeLog(em(spName(occs)), ' : Draw polygon without buffer(**).')
    } else {
      logger %>% writeLog(em(spName(occs)), ' : Draw polygon with buffer of ',
                          drawBgBuf, ' degrees (**).')
    }
    bgExt <- sp::SpatialPolygonsDataFrame(bgExt,
                                          data = data.frame(x = 1),
                                          match.ID = FALSE)
    return(bgExt)
  } else if (ptRem > 0) {
    logger %>%
      writeLog(type = 'error',
               paste0("The draw polygon did not include all localities(**). ",
                      "Remove the polygon before to draw a new one."))
    return()
  }
}
