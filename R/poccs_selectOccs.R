
#' @title poccs_selectOccs
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs x
#' @param polySelXY x
#' @param polySelID x
#' @param logger x
#' @param spN x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

#' @export

poccs_selectOccs <- function(occs, polySelXY, polySelID = 1, logger = NULL, spN = NULL) {
  if (is.null(occs)) {
    logger %>% writeLog(type = 'error',
      "Before processing occurrences, obtain the data in component 1.")
    return()
  }
  if (is.null(polySelXY)) {
    logger %>% writeLog(type = 'error',
      'The polygon has not been finished. Please press "Finish" on the map toolbar then the "Select Occurrences" button.')
      return()
    }

    occs.xy <- occs[c('longitude', 'latitude')]

    # make spatial pts object of original occs and preserve origID
    pts <- sp::SpatialPointsDataFrame(occs.xy, data=occs['occID'])

    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polySelXY)), ID=polySelID)))  # create new polygon from coords

    intersect <- sp::over(pts, newPoly)
    ptRemIndex <- as.numeric(which(is.na(intersect)))

    remIDs <- printVecAsis(pts[ptRemIndex,]$occID)
    # need code here to format the string better

    occs.sel <- occs[-ptRemIndex,]

    logger %>% writeLog(
      em(spName(spN)), ": Removing occurrences with occID = ", remIDs, ". Updated data has n = ", nrow(occs.sel), " records.")
    return(occs.sel)
}