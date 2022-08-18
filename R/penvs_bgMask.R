
#' @title penvs_bgMask Mask environmental data
#' @description This functions crops and masks the environmental data to the
#'   provided background area.
#'
#' @details
#'  This function is used in the select study region component. Here, the
#'    environmental layers to be used in the modeling are cropped and masked
#'    to the provided background area. The background area is determined in
#'    the function penvs_bgExtent from the same component. The function returns
#'    the provided environmental layers cropped and masked in the provided
#'    format (either a rasterBrick or a rasterStack).
#'
#' @param occs data frame of cleaned or processed occurrences obtained from
#'   components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param envs a RasterStack or RasterBrick of environmental layers to be
#'   processed. This determines the output type.
#' @param bgExt a SpatialPolygonsDataFrame with the background area to be used
#'   for processing.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL.
#' @param spN species name to be used for all logger messages
#' @examples
#' \dontrun{
#' occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
#'                  package = "wallace"))[, 2:3]
#' occs$occID <- 1:nrow(occs)
#' envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = TRUE),
#'                       rasName = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = FALSE))
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMask <- penvs_bgMask(occs, envs, bgExt)
#' }
#'
#' @return A RasterStack or a RasterBrick of environmental layers cropped and
#'   masked to match the provided background extent.
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @seealso   \code{\link{penvs_userBgExtent}},
#'   \code{\link{penvs_drawBgExtent}}, \code{\link{penvs_bgExtent}},
#'   \code{\link{penvs_bgSample}}
#' @export

penvs_bgMask <- function(occs, envs, bgExt, logger = NULL, spN = NULL) {
  if (is.null(bgExt)) {
    logger %>% writeLog(
      type = 'error',
      hlSpp(spN),
      "Before sampling background points, define the background extent.")
    return()
  }
  # mask envs by background extent
  smartProgress(logger,
                       message = paste0("Masking rasters for ",
                                        spName(spN), "..."), {

    bgCrop <- raster::crop(envs, bgExt)
    bgMask <- raster::mask(bgCrop, bgExt)
    # GEPB: Workaround when raster alignment is changed after crop, which makes appears
    # new duplicated occs in the same grid cells.
    occsEnvsVals <- as.data.frame(raster::extract(bgMask,
                                                  occs[, c('longitude', 'latitude')],
                                                  cellnumbers = TRUE))
    occs.dups <- duplicated(occsEnvsVals[, 1])
    if (sum(occs.dups) > 0) {
      bgMask <- terra::project(terra::rast(bgMask),
                               terra::rast(envs), method = 'near')
      bgMask <- methods::as(bgMask, "Raster")
    }
  })

  logger %>% writeLog(hlSpp(spN), 'Environmental data masked.')

  return(bgMask)
}
