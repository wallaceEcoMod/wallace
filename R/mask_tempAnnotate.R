
#' @title mask_tempAnnotate
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs x
#' @param envList x
#' @param envExtent x
#' @param envDates x
#' @param logger x
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

mask_tempAnnotate <- function(occs, env, envDates, logger = NULL) {
  # this should be a formal date object of class "POSIXct" "POSIXt"
  # smartProgress(logger, message = "Annotate ... (**)", {
    envDates <- lubridate::parse_date_time(envDates, orders = c("Y", "Ym"))
    print(envDates)
    datedOccs <- occs
    print(datedOccs)
    datedOccs$date <- lubridate::parse_date_time(datedOccs$year, orders = c("Y", "Ym"))
    # convert to spatial object
    sp::coordinates(datedOccs) <- c('longitude','latitude')
    raster::projection(datedOccs) <- raster::projection(env)
    print(datedOccs)
    datedOccs <- maskRangeR::annotate(datedOccs = datedOccs,
                                      env = env,
                                      envDates = envDates,
                                      dateScale = "year")
    bounds <- raster::quantile(datedOccs$layer,
                               prob = c(0, .025, .25, .5, .75, .975, 1),
                               na.rm = T)
  # })
  # tempValues <- as.data.frame(tempExtractValues)
  # tempValues <- tempValues$env[order(as.numeric(row.names(tempValues)))]
  return(bounds)
}
