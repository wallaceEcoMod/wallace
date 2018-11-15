
#' @title c4_bgSample
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs
#' @param bgMask
#' @param bgPtsNum
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

c4_bgSample <- function(occs, bgMask, bgPtsNum, shinyLogs=NULL) {
  # sample random background points
  smartProgress(shinyLogs, message = "Generating background points...", {
    # rvals <- raster::getValues(bgMask)
    # num.vals <- sum(!is.na(rvals))
    # pct <- round((bgPtsNum / num.vals) * 100, digits = 2)
    bgXY <- dismo::randomPoints(bgMask, bgPtsNum)
    bgXY <- bgXY %>% as.data.frame() %>% dplyr::select(longitude = x, latitude = y)
  })
  bg.prop <- round(nrow(bgXY)/bgPtsNum, digits = 2)
  if(bg.prop == 1) {
    shinyLogs %>% writeLog(em(spName(occs)), ": ", bgPtsNum, " random background points sampled.")
  } else {
    shinyLogs %>% writeLog(em(spName(occs)), ": ", bgPtsNum, " random background
                           points requested, but only ", 100*bg.prop, "% of 
                           points (n = ", nrow(bgXY), ") were able to be sampled.")
  }
  return(bgXY)
}
