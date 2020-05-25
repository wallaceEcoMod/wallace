
#' @title poccs_removeByID
#' @description This function removes user selected occurrences by ID
#'
#' @details
#' #' @details
#' This function is called by the select occurrences by ID module. It allows for removal of a single
#' occurrence flagged by the user on the map. The function will return
#' a data frame of occurrences with all relevant columns for futher analyses and without the
#' occurrence selected by the user
#'
#' @param occs data frame of cleaned occurrences obtained from component occs: Obtain occurrence data.
#' @param removeID The ID of the occurrence to be removed from the occurrences dataframe.
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN data frame of cleaned occurrences obtained from component occs: Obtain occurrence data. Used to obtain species name for logger messages

# @keywords
#'
#' @examples
#'out.gbif <- occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
#'occs <- as.data.frame(out.gbif[[1]]$cleaned)
#'removeID <- 81
#'out.ID <- poccs_removeByID(occs, removeID,spN=occs)
#'
#' @return A new occurence dataframe without the user selected occurrence mantaining all
#' columns from original dataframe for further analyses.

#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references

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
