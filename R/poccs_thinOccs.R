
#' @title poocs_thinOccs Thin occurrences
#' @description The function thins the observed occurrences by a user provided
#'   distance.
#'
#' @details
#' This function is called by the component poccs: process occurrence data to
#'   thin the occurrence data to a user specified distance. Providing an output
#'   with preserved columns appropriate for further analyses and a maximized
#'   number of occurrences that are separated by at least the provided distance.
#'
#' @param occs data frame of cleaned occurrences obtained from component occs:
#'   Obtain occurrence data
#' @param thinDist distance in kilometers to be used for thinning. Number must be
#'   positive.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL.
#' @param spN data frame of cleaned occurrences obtained from component occs:
#'   Obtain occurrence data. Used to obtain species name for logger messages.
#'
#' @examples
#' occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
#'                              package = "wallace"))
#' occs$occID <- 1:nrow(occs)
#' out.thin <- poccs_thinOccs(occs = occs, thinDist = 30)
#'
#'
#' @return Output is a data frame of thinned occurences (all occurences at a
#'   distance >thinDist) with the same columns as occs
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @seealso \code{\link[spThin]{thin}}
#' @export

poccs_thinOccs <- function(occs, thinDist, logger = NULL, spN = NULL) {
  if (is.null(occs)) {
    logger %>% writeLog(type = 'error',
      "Before processing occurrences, obtain the data in component 1.")
    return()
  }

  if (thinDist <= 0) {
    logger %>% writeLog(type = "error",
      'Assign positive distance to thinning parameter.')
    return()
  }
  # query database
  smartProgress(logger,
                       message = paste0("Spatially thinning for ",
                                        spName(spN), "..."), {
    output <- spThin::thin(loc.data = occs, lat.col = 'latitude',
                           long.col = 'longitude', spec.col = 'scientific_name',
                           thin.par = thinDist, reps = 100,
                           locs.thinned.list.return = TRUE, write.files = FALSE,
                           write.log.file = FALSE, verbose = FALSE)

    # pull thinned dataset with max records, not just the first in the list
    maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
    # if more than one max, pick first
    maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]
    occs.thin <- occs[as.numeric(rownames(maxThin)),]
    # if (!is.null(values$inFile)) {
    #   thinned.inFile <- values$inFile[as.numeric(rownames(output[[1]])),]
    # }
  })

  logger %>% writeLog(
    hlSpp(spN), 'Total records thinned (', thinDist, ' km) to ',
    nrow(occs.thin), ' localities')

  if (nrow(occs.thin) < 4) {
    logger %>% writeLog(type = 'error',
      hlSpp(spN),
      "After removing occurrences, there are three or less points. ",
      "You need more occurrences to continue the analysis."
    )
    return()
  }

  return(occs.thin)
}
