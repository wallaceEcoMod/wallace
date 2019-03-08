
#' @title c2_thinOccs
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs x
#' @param thinDist x
#' @param shinyLogs x
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

c2_profileOccs <- function(sp.name, sp.table, x.field, y.field, t.field, l.field,
                           # c.field = "country",
                           # e.field = "elevation",
                           r.env, shinyLogs = NULL) {
  
  # query database
  occs.prof <- occProfileR::occurrenceProfile(sp.name = sp.name, sp.table = sp.table, 
                                              x.field = x.field, y.field = y.field, 
                                              t.field = t.field, l.field = l.field,
                                              # c.field = c.field, e.field = e.field,
                                              doCentroidDetection = FALSE,
                                              doRangeAnalysis = FALSE,
                                              doCountryRecordAnalysis = FALSE,
                                              doHyperHumanDetection = FALSE,
                                              r.env = r.env)
    
    # SPACE for processing outputs
    
    
  shinyLogs %>% writeLog('Occurrences profiled for ', em(sp.name), '.')
  
  return(occs.prof)
}
  