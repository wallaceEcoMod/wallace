
#' @title poocs_thinOccs Thin occurrences
#' @description The function thins the observed occurrences by a user provided distance.
#'
#' @details
#' This function is called by the component poccs: process occurrence data to thin the occurence data
#' to a user specified distance. Providing an output with preserved columns appropriate for further analyses
#' and a maximized number of occurrences that are separated by at least the provided distance.
#'
#' @param occs data frame of cleaned occurrences obtained from component occs: Obtain occurrence data
#' @param thinDist distance in meters to be used for thinning. Number must be positive.
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN data frame of cleaned occurrences obtained from component occs: Obtain occurrence data. Used to obtain species name for logger messages
# @keywords
#'
#' @examples
#' spN <- "Panthera onca"
#' out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
#' occs <- as.data.frame(out.gbif[[1]]$cleaned)
#' out.thin <- poccs_thinOccs(occs=occs, thinDist = 30,
#'                            spN = spN, logger = NULL)
#'
#'
#' @return Output is a data frame of thinned occurences (all occurences at a distance >thinDist) with the same columns as occs
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note

#' @seealso \code{\link[spThin]{thin}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

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
  alfred.smartProgress(logger, message = paste0("Spatially thinning for ", alfred.spName(spN), "..."), {  # start progress bar
    output <- spThin::thin(occs, 'latitude', 'longitude', 'scientific_name', thin.par = thinDist,
                           reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE,
                           verbose = FALSE)

    # pull thinned dataset with max records, not just the first in the list
    maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
    maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]  # if more than one max, pick first
    occs.thin <- occs[as.numeric(rownames(maxThin)),]
    # if (!is.null(values$inFile)) {
    #   thinned.inFile <- values$inFile[as.numeric(rownames(output[[1]])),]
    # }
  })

  logger %>% writeLog(
    alfred.hlSpp(spN), 'Total records thinned (', thinDist, ' km) to ',
    nrow(occs.thin), ' localities')

  if (nrow(occs.thin) < 4) {
    logger %>% writeLog(type = 'error',
      alfred.hlSpp(spN), "After removing occurrences, there are three or less points. ",
      "You need more occurrences to continue the analysis."
    )
    return()
  }

  return(occs.thin)
}
