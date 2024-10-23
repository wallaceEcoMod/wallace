# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# mask_tempAnnotate.R
# File author: Wallace EcoMod Dev Team. 2023.
# --------------------------------------------------------------------------
# This file is part of the Wallace EcoMod application
# (hereafter “Wallace”).
#
# Wallace is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# Wallace is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Wallace. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
#' @title mask_tempAnnotate
#' @description Obtain temporally matched values
#' @details Annotate point data (e.g., occurrences) with raster values (e.g.,
#' env data) based on matching years. Function returns the quantiles of values
#' for use in selecting a threshold value for masking.
#' @param occs Dataframe of occurrence/point data. Must include years
#' @param env Rasterstack of annual environmental data
#' @param envDates String with the years corresponding to the rasters
#' @param logger Stores all notification messages to be displayed in the Log
#' Window of Wallace GUI. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL.
#' @examples
#' \dontrun{
#' ### Set parameters
#' occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv", package = "wallace"))
#' # assigning dummy dates, but ideally the occ data would have years
#' dates <- sample(2000:2019,35, replace=T)
#' occs$year <- dates
#' env <- list.files(path = system.file('extdata/MODIS', package = "wallace"),full.names = TRUE)
#' env <- raster::stack(env)
#' envDates <- c(2005,2006,2008,2009,2010)
#' ### Run function
#' bounds <- mask_tempAnnotate(occs, env, envDates, logger = NULL)
#' }
#' @return Numeric. Returns the quantiles
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @seealso \code{\link[maskRangeR]{annotate}}, \code{\link{mask_tempExtract}}
#' @export
#'
mask_tempAnnotate <- function(occs, env, envDates, logger = NULL) {
  # this should be a formal date object of class "POSIXct" "POSIXt"
  smartProgress(logger, message = "Annotating ... ", {
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
  })
}
