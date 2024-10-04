
#' @title mask_tempExtract
#' @description To mask prediction by temporal thresholds.
#' @param lowerInp Lower bound value
#' @param upperInp Upper bound value
#' @param maskRaster Masking raster
#' @param pred Prediction
#' @param logger stores all notification messages to be displayed in the Log
#'  Window of Wallace GUI. insert the logger reactive list here for running in
#'  shiny, otherwise leave the default NULL
#' @param spN Species name.
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'

mask_tempExtract <- function(lowerInp, upperInp, maskRaster, pred,
                             logger = NULL, spN = NULL) {
  if (is.na(lowerInp) & is.na(upperInp)) {
    logger %>% writeLog(type = 'error', hlSpp(spN), "Please, provide bounds (**)")
    return()
  }
  # compare prediction and mask Raster
  smartProgress(logger, message = "Masking ...", {
    pred <- terra::rast(pred)
    maskRaster <- terra::rast(maskRaster)
    pred <- terra::resample(pred, maskRaster, "near")
    sameExt <- terra::compareGeom(maskRaster, pred, ext = FALSE,
                                  rowcol = FALSE, crs = TRUE, res = TRUE)
    if (sameExt == FALSE) {
      logger %>%
        writeLog(type = 'warning', hlSpp(spN),
                 "Rasters don't have the same resolution, crs or origin. (**)")
      return()
    }
    maskRaster <- terra::crop(maskRaster, pred)
    postPred <- pred * (maskRaster >= lowerInp) * (maskRaster <= upperInp)
    predValues <- terra::spatSample(x = postPred,
                                    size = 100, na.rm = TRUE)[, 1]
    if (any(predValues > 0 & predValues < 1)) {
      postPred[postPred <= 0] <- NA
    }
    postPred <- postPred %>% terra::trim() %>% raster::raster()
  })
  return(postPred)
}
