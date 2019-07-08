#' 
#' #' @title c2_thinOccs
#' #' @description ..
#' #'
#' #' @details
#' #' See Examples.
#' #'
#' #' @param occs x
#' #' @param thinDist x
#' #' @param shinyLogs x
#' # @keywords
#' #'
#' # @examples
#' #'
#' #'
#' # @return 
#' #' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' # @note
#' 
#' # @seealso
#' # @references
#' # @aliases - a list of additional topic names that will be mapped to
#' # this documentation when the user looks them up from the command
#' # line.
#' # @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' 
#' #' @export
#' 
#' c2_profileOccs <- function(sp.name, 
#'                            sp.table, 
#'                            x.field, 
#'                            y.field, 
#'                            t.field, 
#'                            l.field,
#'                            # c.field = "country",
#'                            # e.field = "elevation",
#'                            r.env, 
#'                            shinyLogs = NULL) {
#'   
#'   # query database
#'   occs.prof <- suppressMessages(
#'                occProfileR::occurrenceProfile(sp.name = sp.name, 
#'                                               sp.table = sp.table, 
#'                                               x.field = x.field, 
#'                                               y.field = y.field, 
#'                                               t.field = t.field, 
#'                                               l.field = l.field,
#'                                               # c.field = c.field, 
#'                                               # e.field = e.field,
#'                                               doCentroidDetection = TRUE, 
#'                                               doRangeAnalysis = TRUE,
#'                                               doCountryRecordAnalysis = FALSE,
#'                                               doHyperHumanDetection = FALSE,
#'                                               doInstitutionLocality=TRUE,
#'                                               doGeoOutliers=TRUE,
#'                                               doEnvOutliers=TRUE,
#'                                               r.env = r.env,
#'                                               verbose = F))
#'     
#'   shinyLogs %>% writeLog('Occurrences profiled for ', em(sp.name), '.')
#'   
#'   return(occs.prof)
#' }
#' 
#' ##############################################################################
#' # CM: probably not needed
#' #' @title c2_selectCleanedOccs
#' #' @description After profiling occurrences, select a subset based on user selected tests or grades
#' #'
#' #' @details
#' #' See Examples.
#' #'
#' #' @param occs x
#' #' @param grades x
#' #' @param shinyLogs x
#' # @keywords
#' #'
#' # @examples
#' #'
#' #'
#' # @return 
#' #' @author Cory Merow <jkass@@gradcenter.cuny.edu>
#' # @note
#' 
#' # @seealso
#' # @references
#' # @aliases - a list of additional topic names that will be mapped to
#' # this documentation when the user looks them up from the command
#' # line.
#' # @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' 
#' #' @export
#' # 
#' # c2_selectCleanedOccs <- function(occs, grades, shinyLogs = NULL) {
#' #   if (is.null(occs)) {
#' #     shinyLogs %>% writeLog(type = 'error', 
#' #                            "Before processing occurrences, obtain the data in component 1.")
#' #     return()
#' #   }
#' #   # make sure some have been selected
#' #   # also add a test that at least some presences pass the specified tests
#' #   if (is.null(grades)) {
#' #     shinyLogs %>% writeLog(type = 'error', 
#' #                            'You must select some grades to keep.')
#' #     return()
#' #   }
#' #   
#' #   # CM: i think occs should have all the grades with it
#' #   # check this uses the right formats
#' #   keep=occs$quality.grade %in% unlist(grades) 
#' #   occsClean=occs[keep,]
#' #   
#' #   shinyLogs %>% writeLog(
#' #     em(spName(occs)), ": Removing dirty occurrences")
#' #   return(occsClean)
#' # }
#'   