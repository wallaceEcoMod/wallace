
#' @title mask_tempAnnotate
#' @description Obtain temporal matched values
#' @param occs Occurrences
#' @param env Stack of raster
#' @param envDates String with the years of rasters
#' @param logger logger
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'
mask_tempAnnotate <- function(occs, env, envDates, logger = NULL) {
  # this should be a formal date object of class "POSIXct" "POSIXt"
  # smartProgress(logger, message = "Annotate ... (**)", {
    envDates <- lubridate::parse_date_time(envDates, orders = c("Y", "Ym"))
    datedOccs <- occs
    datedOccs$date <- lubridate::parse_date_time(datedOccs$year, orders = c("Y", "Ym"))
    # convert to spatial object
    sp::coordinates(datedOccs) <- c('longitude','latitude')
    raster::projection(datedOccs) <- raster::projection(env)
    datedOccs <- maskRangeR::annotate(datedOccs = datedOccs,
                                      env = env,
                                      envDates = envDates,
                                      dateScale = "year")
    bounds <- raster::quantile(datedOccs$env,
                               prob = c(0, .025, .25, .5, .75, .975, 1),
                               na.rm = TRUE)
  return(bounds)
}
