
#' @title proj_user
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param evalOut x
#' @param curModel x
#' @param envs x
#' @param outputType x
#' @param pjExt x
#' @param logger x
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

proj_user <- function(evalOut, curModel, envs, outputType, alg, clamp, pjExt,
                      logger = NULL) {
  newPoly <- pjExt

  if (alg == 'bioclim') {
    logger %>% writeLog('Projection for BIOCLIM model.')
  } else if (alg == 'maxent') {
    if (clamp == TRUE | alg == "maxent.jar") {
      logger %>% writeLog('Projection for clamped model', curModel(), '.')
    } else if (clamp == FALSE) {
      logger %>% writeLog('Projection for unclamped', curModel(), '.')
    }
  }

  smartProgress(logger,
                message = "Masking environmental grids to projection extent...", {
    projMsk <- raster::crop(envs, newPoly)
    projMsk <- raster::mask(projMsk, newPoly)
  })

  smartProgress(logger,
                message = 'Projecting model to user-specified files (**)...', {
    if (alg == 'BIOCLIM') {
      modProjUser <- dismo::predict(evalOut@models[[curModel]], projMsk)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") {pargs <- "exponential"} else {pargs <- outputType}
      modProjUser <- ENMeval::maxnet.predictRaster(evalOut@models[[curModel]],
                                                   projMsk, type = pargs,
                                                   doClamp = clamp)
    } else if (alg == "maxent.jar") {
      pargs <- paste0("outputformat=", outputType)
      modProjUser <- dismo::predict(evalOut@models[[curModel]], projMsk,
                                    args = pargs)
    }
  })

  return(list(projExt = projMsk, projUser = modProjUser))
}
