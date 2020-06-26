
#' @title proj_time Project model to a new time
#' @description Function projects the model generated in previous components to a new time and area using provided layers

#' @details
#' This functions allows for the projection of the model created in previous components to a new time and area.
#' The projection area is user provided in the map of the GUI and the projection time user selected. The model will be projected to the new area and time as long as the environmental variables are available for the area.
#' This function returns a list including the cropped environmental variables used for projecting and the projected model.

#'
#' @param evalOut ENMevaluate output from previous module and using any of the available algorithms
#' @param curModel If algorithm is maxent, model selected by user as best or optimal, in terms of feature class and regularization multiplier (e.g 'L_1'). Else must be 1
#' @param envs Environmental layers of different time to be used for projecting the model. They must match the layers used for generating the model in the model component
#' @param outputType Output type to be used when algorithm is maxnet or maxent.jar.
#' @param alg Modeling algorithm used in the model component. Can be one of : 'bioclim', 'maxent.jar' or 'maxnet'
#' @param clamp logical, whether projection will be of clamped or unclamped model.
#' @param pjExt Extent of the area to project the model to. This is defined by the user in the map of the GUI and is provided as a SpatialPolygons object.
#' @param logger logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
# @keywords
#'
#' @examples
#' out.gbif <- occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
#' occs <- as.data.frame(out.gbif[[1]]$cleaned)
#' envs <- envs_worldclim(bcRes = 10, bcSel = c('bio01','bio19'), doBrick = FALSE)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=occs)
#' bgMask <- penvs_bgMask(occs, envs, bgExt,spN=occs)
#' bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000,spN=occs)
#' partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,aggFact = NULL,spN=occs)
#' ## extent to project
#' longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319, -71.11931)
#' latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
#'selCoords <- matrix(c(longitude, latitude), byrow = F, ncol = 2)
#'expertAddedPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)), ID=1)))
#' ##projection time layers, using worldclim 2.1 Future 2021-2040 MIROC6 ssp126 bioclims as example
#' envsFut<-list.files(path='./tests/testthat/wc10/Future', pattern = ".tif$", full.names = TRUE)
#' envsFut<-raster::stack(envsFut)
#' modAlg <- model_bioclim(occs, bg, partblock$occ.grp, bgGrp = partblock$bg.grp, bgMask,spN=occs)
#' modProj <- proj_time(evalOut = modAlg, curModel=1, envs=envsFut,alg='bioclim',clamp=FALSE, pjExt = expertAddedPoly )

#' @return A list of two elements: projExt and projTime.
#' The first is a RasterBrick or RasterStack of the environmental variables cropped to the projection area.
#' The second element is a raster of the projected model with the specified output type.
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
# @note
#' @seealso \code{\link[dismo]{predict}}, \code{\link[ENMeval]{maxnet.predictRaster}}, \code{\link{proj_area}} \code{\link{proj_user}}

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
    projMsk <- raster::mask(projMsk, newPoly)
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
