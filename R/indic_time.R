#' @title indic_time
#' @description Calculate change in range area through time
#' @param range sf polygon. A polygon with the range map.
#' @param envs rasterStack. Environmental variables.
#' @param thrh interger. Threshold of where envs layers
#'  should be thresholded
#' @param bound character. character string characterizing the way the threshold
#'  should happen. "upper" removes values above the threshold (e.g., maximum
#'  human footprint). "lower" removes values below the threshold (e.g., minimum
#'  forest cover). "neither" does not threshold at any point. "both" thresholds
#'  at both threshold values (if provided; e.g., minimum and maximum temperature).
#' @param logger logger
#' @param spN species name
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'

indic_time <- function(range, envs, thrh, bound, logger = NULL, spN = NULL) {
  smartProgress(
    logger,
    message = "Calculating area indic through time ", {
      ##run function
      rangeTime <- changeRangeR::envChange(
        rStack = envs,
        binaryRange = range,
        threshold = thrh,
        bound = bound)
    })
  logger %>% writeLog(hlSpp(spN),
                      "Range area after masking for environmental variables ",
                      "through time calculation done. (**)")
  return(rangeTime$Area)
}

