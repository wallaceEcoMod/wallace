
#' @title proj_time Project model to a new time
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
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

proj_time <- function(evalOut, curModel, envs, outputType, alg, clamp,
                      pjExt, logger = NULL) {
  newPoly <- pjExt
  if (alg == 'bioclim') {
    logger %>% writeLog('Projection in time for BIOCLIM model.')
  } else if (alg == 'maxent.jar'|clamp==TRUE) {

    logger %>% writeLog('Projection in time for clamped model ', curModel, '.')

  } else if (clamp == FALSE) {
    logger %>% writeLog('New time projection for unclamped ', curModel, '.')
  }


  smartProgress(logger, message = "Clipping environmental data to current extent...", {
    pjtMsk <- raster::crop(envs, newPoly)
  })

  smartProgress(logger, message = ("Projecting to new time..."), {
    if (alg == 'bioclim') {
      modProjTime <- dismo::predict(evalOut@models[[curModel]], pjtMsk)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") {pargs <- "exponential"} else {pargs <- outputType}
      modProjTime <- ENMeval::enm.maxnet@pred(mod = evalOut@models[[curModel]],
                                                  envs = pjtMsk, doClamp = clamp,
                                                  pred.type = pargs)
    } else if (alg == "maxent.jar") {
      pargs <- paste0("outputformat=", outputType)
      modProjTime <- dismo::predict(evalOut@models[[curModel]], pjtMsk,
                                    args = pargs)
    }
  })

  return(list(projExt = pjtMsk, projTime = modProjTime))
}
