#' @title poccs_selectOccs
#' @description This function removes occurrences outside of a user created polygon
#'
#' @details
#' See Examples.
#'
#' @param occs data frame of cleaned occurrences obtained from component occs: Obtain occurrence data.
#' @param polySelXY matrix of longitude and latitude describing the expert drawn polygon.
#' @param polySelID Polygon ID to be used in SpatialPolygons creation, defaults to 1.
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN data frame of cleaned occurrences obtained from component occs: Obtain occurrence data. Used to obtain species name for logger messages
# @keywords
#'
# @examples
#'
#'
#' @return A new occurence dataframe including only occurences inside the provided polygon and mantaining all
#' columns from original dataframe for further analyses.
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
