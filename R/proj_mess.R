
#' @title proj_mess generate MESS map for projection time
#' @description This function generates a MESS map for the new variables for projection based on variables and points used for modeling in previous components.
#'
#' @details
#' This functions allows for the creation of a MESS map for the new provided variables for projection.
#' These variables are either user uploaded or selected from WorldClim database.
#' MESS map is based on occurrence and background points used for generating the model and the environmental values at those points.
#'
#' @param occs a data frame of occurrences used for modeling and values of environmental variables for each point
#' @param bg a data frame of points used as background for modeling and values of environmental variables for each point
#' @param bgMsk a rasterBrick or rasterStack of environmental variables used for modeling. They must be cropped and masked to extent used in model training
#' @param projExtRas a rasterStack or rasterBrick of environmental variables to be used for projecting
#' @param time a string of the time used for projecting (e.g. "2040 MIROC6 ssp126)
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN character. Used to obtain species name for logger messages
# @keywords
#'
#' @examples
#' out.gbif <- occs_queryDb(spName = "Panthera onca", occDb = "gbif",
#'                          occNum = 100)
#' occs <- as.data.frame(out.gbif[[1]]$cleaned)
#' envs <- envs_worldclim(bcRes = 10, bcSel = c('bio01','bio19'),
#'                        doBrick = FALSE)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMsk <- penvs_bgMsk(occs, envs, bgExt)
#' bg <- penvs_bgSample(occs, bgMsk, bgPtsNum = 10000)
#' partblock <- part_partitionOccs(occs, bg, method = 'block',
#'                                 kfolds = NULL, bgMask = NULL,
#'                                 aggFact = NULL)
#' bioclimAlg <- model_bioclim(occs, bg, partblock$occ.grp,
#'                             partblock$bg.grp, bgMsk)
#' modelOccs <- bioclimAlg@@occs
#' modelBg <- bioclimAlg@@bg
#' longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331,
#'                -66.47149, -66.71319, -71.11931)
#' latitude <- c(13.18379, 7.52315, 0.93105, -1.70167,
#'               0.98391, 6.09208, 12.74980)
#' selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
#' expertAddedPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)), ID=1)))
#' # projection time layers, using worldclim 2.1 Future 2021-2040
#' # MIROC6 ssp126 bioclims as example
#' envsFut <- list.files(path='./wc10/Future', pattern = ".tif$",
#'                       full.names = TRUE)
#' envsFut <- raster::stack(envsFut)
#' projExtRas <- raster::crop(envsFut, expertAddedPoly)
#' projExtRas <- raster::mask(projExtRas, expertAddedPoly)
#' time <- "2021-2040 MIROC6 ssp126"
#' ## run function
#' projMess <- proj_mess(occs = modelOccs, bg = modelBg, bgMsk = bgMsk,
#'                       projExtRas = projExtRas, time = time)

#'
# @return
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>

# @note
#' @seealso \code{\link[dismo]{mess}}, \code{\link{proj_time}} \code{\link{proj_userEnvs}}

# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

proj_mess <- function(occs, bg, bgMsk, projExtRas, time, logger = NULL, spN = NULL) {

  occsVals <- occs[,names(bgMsk)]
  bgVals <- bg[,names(bgMsk)]
  allVals <- rbind(occsVals, bgVals)

  # rename rasters to match originals
  projExtRas2 <- projExtRas
  names(projExtRas2) <- names(bgMsk)

  smartProgress(logger, message = "Generating MESS map...", {
    mss <- suppressWarnings(dismo::mess(projExtRas2, allVals))
    # for mapping purposes, set all infinite values to NA
    mss[is.infinite(mss)] <- NA
    logger %>% writeLog(hlSpp(spN), "Generated MESS map for ", time, ".")
  })

  return(mss)
}
