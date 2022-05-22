
#' @title mask_tempExtract
#' @description To mask prediction by temporal thresholds.
#' @param lowerInp Lower bound value
#' @param upperInp Upper bound value
#' @param maskRaster Masking raster
#' @param pred Prediction
#' @param logger logger
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
    postPred[postPred <= 0] <- NA
    postPred <- postPred %>% terra::trim() %>% raster::raster()
  })
  return(postPred)
}
