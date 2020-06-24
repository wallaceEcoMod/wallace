#' @title proj_user Project model to user specified area and/or time
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
    logger %>% writeLog('User specified projection for BIOCLIM model.')
  }
  else if (alg == 'maxent.jar'|clamp==TRUE) {
    logger %>% writeLog('User specified projection for clamped model ', curModel, '.')
    }
  else if (clamp == FALSE) {
      logger %>% writeLog('User specified projection for unclamped model', curModel, '.')
    }

  smartProgress(logger,
                message = "Masking environmental grids to projection extent...", {
    projMsk <- raster::crop(envs, newPoly)
  })

  smartProgress(logger,
                message = 'Projecting model to user-specified time and/or area', {
    if (alg == 'bioclim') {
      modProjUser <- dismo::predict(evalOut@models[[curModel]], projMsk)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") {pargs <- "exponential"} else {pargs <- outputType}
      modProjUser <- ENMeval::enm.maxnet@pred(mod = evalOut@models[[curModel]],
                                                   envs = projMsk, doClamp = clamp,
                                              pred.type = pargs)


    } else if (alg == "maxent.jar") {
      pargs <- paste0("outputformat=", outputType)
      modProjUser <- dismo::predict(evalOut@models[[curModel]], projMsk,
                                    args = pargs)
    }
  })

  return(list(projExt = projMsk, projUser = modProjUser))
}
