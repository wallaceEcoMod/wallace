
#' @title penvs_bgMask
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs x
#' @param envs x
#' @param bgExt x
#' @param logger x
#'
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

penvs_bgMask <- function(occs, envs, bgExt, logger=NULL) {
  if (is.null(bgExt)) {
    logger %>% writeLog(type = 'error', "Before sampling background points, define the background extent.")
    return()
  }
  # mask envs by background extent
  smartProgress(logger, message = paste0("Masking rasters for ", spName(occs), "..."), {
    bgCrop <- raster::crop(envs, bgExt)
    bgMask <- raster::mask(bgCrop, bgExt)
  })

  logger %>% writeLog(em(spName(occs)), ': Environmental data masked.')

  return(bgMask)
}
