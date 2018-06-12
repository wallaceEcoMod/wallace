#' @title c2_removeByID
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs
#' @param removeID
#' @param shinyLogs = NULL
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

c2_removeByID <- function(occs, removeID, shinyLogs = NULL) {
  if (is.null(occs)) {
    shinyLogs %>% writeLog(type = 'error', "Before processing occurrences, 
                      obtain the data in component 1.")
    return()
  }
  
  if (!(removeID %in% occs$occID)) {
    shinyLogs %>% writeLog(type = 'error','Entered occID not found.')
    return()
  }
  
  # find which occID corresponds to row for removal
  i <- which(removeID == occs$occID)  
  # remove the row
  occs.remID <- occs[-i,]
  
  shinyLogs %>% writeLog("Removed occurrence from", em(spName(occs)), "with occID = ", removeID, 
                    ". Updated data has n = ", nrow(occs.remID), " records.")
  return(occs.remID)
}