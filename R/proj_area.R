
#' @title proj_area Project model to a new area
#' @description Function projects the model generated in previous components to a new user drawn area
#'
#' @details
#' See Examples.
#'
#' @param evalOut ENMevaluate output from previous module and using any of the available algorithms
#' @param curModel If algorithm is maxent, model selected by user as best or optimal, in terms of feature class and regularization multiplier (e.g 'L_1'). Else must be 1
#' @param envs Environmental layers to be used for projecting the model. They must matchthe layers used for generating the model in the model component
#' @param outputType Output type to be used when algorithm is maxnet or maxent.jar.
#' @param alg Modeling algorithm used in the model component. Can be one of : "bioclim", "maxent.jar" or "maxnet"
#' @param clamp logical, wether projection will be of clamped or uncalmped model.
#' @param pjExt Extent of the area to project the model to. This is defined by the user in the map of the GUI.
#' @param logger logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
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

proj_area <- function(evalOut, curModel, envs, outputType, alg, clamp, pjExt,
                      logger = NULL) {
  newPoly <- pjExt

  if (alg == 'bioclim') {
    logger %>% writeLog('Projection for BIOCLIM model.')
  } else if (alg == 'maxent.jar'|clamp==TRUE) {

     logger %>% writeLog('Projection for clamped model ', curModel, '.')

       } else if (clamp == FALSE) {
       logger %>% writeLog('New area projection for unclamped ', curModel, '.')
    }


  smartProgress(logger,
                message = "Masking environmental grids to projection extent...", {
    projMsk <- raster::crop(envs, newPoly)
  })

  smartProgress(logger, message = 'Projecting model to new area...', {
    if (alg == 'bioclim') {
      modProjArea <- dismo::predict(evalOut@models[[curModel]], projMsk)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") {pargs <- "exponential"} else {pargs <- outputType}
      modProjArea <- ENMeval::maxnet.predictRaster(evalOut@models[[curModel]],
                                                   projMsk, type = pargs,
                                                   doClamp = clamp)
    } else if (alg == 'maxent.jar') {
      pargs <- paste0("outputformat=", outputType)
      modProjArea <- dismo::predict(evalOut@models[[curModel]], projMsk,
                                    args = pargs)
    }
  })

  return(list(projExt = projMsk, projArea = modProjArea))
}
