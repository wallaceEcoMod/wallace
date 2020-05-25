
#' @title poccs_removeByID
#' @description This function removes user selected occurrences by ID
#'
#' @details
#' #' @details
#' This function is called by the select occurrences by ID module. It allows for removal of
#' occurrences flagged by the user on the map. The function will return
#' a data frame of occurrences with all relevant columns for futher analyses and without the
#' occurrences selected by the user
#'
#' @param occs data frame of cleaned occurrences obtained from component occs: Obtain occurrence data.
#' @param removeID x
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN data frame of cleaned occurrences obtained from component occs: Obtain occurrence data. Used to obtain species name for logger messages

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

poccs_removeByID <- function(occs, removeID, logger = NULL, spN = NULL) {
  if (is.null(occs)) {
    logger %>% writeLog(type = 'error',
      "Before processing occurrences, obtain the data in component 1.")
    return()
  }

  if (!(removeID %in% occs$occID)) {
    logger %>% writeLog(type = 'error','Entered occID not found.')
    return()
  }

  # find which occID corresponds to row for removal
  i <- which(removeID == occs$occID)
  # remove the row
  occs.remID <- occs[-i,]

  logger %>% writeLog(
    "Removed occurrence from ", em(spName(spN)), "with occID = ", removeID,". Updated data has n = ", nrow(occs.remID), " records.")
  return(occs.remID)
}
