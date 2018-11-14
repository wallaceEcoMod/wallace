
#' @title c8_mess
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs
#' @param bg
#' @param bgMsk
#' @param projExtRas
#' @param time
#' @param shinyLogs
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

c8_mess <- function(occs, bg, bgMsk, projExtRas, time, shinyLogs = NULL) {
  
  occsVals <- occs[,names(bgMsk)]
  bgVals <- bg[,names(bgMsk)]
  allVals <- rbind(occsVals, bgVals)
  
  # rename rasters to match originals
  projExtRas2 <- projExtRas
  names(projExtRas2) <- names(bgMsk)
  
  smartProgress(shinyLogs, message = "Generating MESS map...", {
    mss <- suppressWarnings(dismo::mess(projExtRas2, allVals))
    # for mapping purposes, set all infinite values to NA
    mss[is.infinite(mss)] <- NA
    shinyLogs %>% writeLog("Generated MESS map for ", time, ".")
  })
  
  return(mss)
}