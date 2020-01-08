
#' @title poccs_removeByID
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs x
#' @param removeID x
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
