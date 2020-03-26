
#' @title penvs_bgSample
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs x
#' @param bgMask x
#' @param bgPtsNum x
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

penvs_bgSample <- function(occs, bgMask, bgPtsNum, logger = NULL, spN = NULL) {
  # sample random background points
  smartProgress(logger, message = "Generating background points...", {
    # rvals <- raster::getValues(bgMask)
    # num.vals <- sum(!is.na(rvals))
    # pct <- round((bgPtsNum / num.vals) * 100, digits = 2)
    bgXY <- dismo::randomPoints(bgMask, bgPtsNum)
    bgXY <- bgXY %>% as.data.frame() %>% dplyr::select(longitude = x, latitude = y)
    bgNonNA <- raster::ncell(bgMask) - raster::freq(bgMask, value = NA)[[1]]
  })
  bg.prop <- round(nrow(bgXY)/bgPtsNum, digits = 2)
  if(bg.prop == 1) {
    logger %>%
      writeLog(
        em(spName(spN)), ": ", bgPtsNum, " random background points sampled out of ",
        bgNonNA, " total points. (**)")
  } else {
    logger %>%
      writeLog(
        em(spName(spN)), ": ", bgPtsNum, " random background points requested, but only ",
        100 * bg.prop, "% of points (n = ", nrow(bgXY), ") were able to be sampled.")
  }
  return(bgXY)
}
