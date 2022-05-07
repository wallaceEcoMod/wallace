
#' @title mask_tempExtract
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param lower x
#' @param upper x
#' @param maskRaster x
#' @param pred x
#' @param logger x
#' @param spN x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

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
