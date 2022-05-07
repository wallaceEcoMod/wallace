#' @title poccs_selectOccs Remove occurrences outside of polygon
#' @description This function removes occurrences outside of a user created
#'   polygon.
#'
#' @details
#' This function is called by the select occurrences on map module. It
#'   allows for removal of occurrences outside the user drawn polygon in
#'   the map. The function will return a data frame of occurrences with all
#'   relevant columns for further analyses and without the occurrences outside
#'   of the polygon.
#'
#' @param occs data frame of cleaned occurrences obtained from component
#'   occs: Obtain occurrence data.
#' @param polySelXY matrix of longitude and latitude describing the expert
#'   drawn polygon.
#' @param polySelID numeric. Polygon ID to be used in SpatialPolygons creation,
#'   defaults to 1.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL.
#' @param spN data frame of cleaned occurrences obtained from component
#'   occs: Obtain occurrence data. Used to obtain species name for logger
#'   messages.
#' @examples
#' occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
#'                              package = "wallace"))[, 2:3]
#' occs$occID <- 1:nrow(occs)
#' longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331,
#'                -66.47149, -66.71319, -71.11931)
#' latitude <- c(13.18379, 7.52315, 0.93105, -1.70167,
#'               0.98391, 6.09208, 12.74980)
#' expertAddedPoly <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
#' out.occs <- poccs_selectOccs(occs, polySelXY = expertAddedPoly,
#'                              polySelID = 1)

#' @return A new occurence dataframe including only occurences inside the
#' provided polygon and mantaining all columns from original dataframe for
#' further analyses.
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export

poccs_selectOccs <- function(occs, polySelXY, polySelID = 1, logger = NULL,
                             spN = NULL) {
  if (is.null(occs)) {
    logger %>% writeLog(type = 'error',
      "Before processing occurrences, obtain the data in component 1.")
    return()
  }
  if (is.null(polySelXY)) {
    logger %>% writeLog(
      type = 'error',
      'The polygon has not been finished. Please press "Finish" on the map ',
      'toolbar then the "Select Occurrences" button.')
      return()
    }

    occs.xy <- occs[c('longitude', 'latitude')]

    # make spatial pts object of original occs and preserve origID
    pts <- sp::SpatialPointsDataFrame(occs.xy, data=occs['occID'])
    # create new polygon from coords
    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polySelXY)),
                                                     ID = polySelID)))

    intersect <- sp::over(pts, newPoly)

    ptRemIndex <- as.numeric(which(is.na(intersect)))

    remIDs <- printVecAsis(pts[ptRemIndex,]$occID)
    # need code here to format the string better
    if (is.na(ptRemIndex[1])){
      logger %>% writeLog(
        type = 'warning',
        hlSpp(spN),
        "Your polygon is selecting all occurrences. None will be removed.")
      occs.sel <- occs
      return()
    }
    occs.sel <- occs[-ptRemIndex,]

    logger %>% writeLog(
      hlSpp(spN), "Removing occurrence(s) with occID = ", remIDs,
      ". Updated data has n = ", nrow(occs.sel), " records.")

    if (nrow(occs.sel) < 4) {
      logger %>% writeLog(
        type = 'error',
        hlSpp(spN),
        "After removing occurrences, there are three or less points. ",
        "You need more occurrences to continue the analysis."
      )
      return()
    }
    return(occs.sel)
}
