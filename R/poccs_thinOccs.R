
#' @title poocs_thinOccs
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs x
#' @param thinDist x
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
  smartProgress(logger, message = paste0("Spatially thinning for ", spName(spN), "..."), {  # start progress bar
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
    'Total records for ', em(spName(spN)), ' thinned to [', nrow(occs.thin), '] localities.')

  return(occs.thin)
}