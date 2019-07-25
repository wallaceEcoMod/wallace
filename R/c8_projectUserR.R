
#' @title c8_projectUser
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

c8_projectUser <- function(evalOut, curModel, envs, outputType, alg, clamp, pjExt,
                           shinyLogs = NULL) {
  newPoly <- pjExt

  if (alg == 'bioclim') {
    shinyLogs %>% writeLog('Projection for BIOCLIM model.')
  } else if (alg == 'maxent') {
    if (clamp == TRUE | alg == "maxent.jar") {
      shinyLogs %>% writeLog('Projection for clamped model', curModel(), '.')
    } else if (clamp == FALSE) {
      shinyLogs %>% writeLog('Projection for unclamped', curModel(), '.')
    }
  }

  smartProgress(shinyLogs,
                message = "Masking environmental grids to projection extent...", {
    projMsk <- raster::crop(envs, newPoly)
    projMsk <- raster::mask(projMsk, newPoly)
  })

  smartProgress(shinyLogs,
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
