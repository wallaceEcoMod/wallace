
#' @title xfer_mess generate MESS map for transferred raster
#' @description This function generates a MESS map for the new variables for
#' transferring based on variables and points used for modeling in previous
#' components.
#'
#' @details
#' This functions allows for the creation of a MESS map for the new provided
#' variables for transferring. These variables are either user uploaded or
#' selected from WorldClim database. MESS map is based on occurrence and
#' background points used for generating the model and the environmental values
#' at those points.
#'
#' @param occs a data frame of occurrences used for modeling and values of
#'   environmental variables for each point.
#' @param bg a data frame of points used as background for modeling and values
#'   of environmental variables for each point.
#' @param bgMsk a rasterBrick or rasterStack of environmental variables used
#'   for modeling. They must be cropped and masked to extent used in model
#'   training.
#' @param xferExtRas a rasterStack or rasterBrick of environmental variables
#'   to be used for transferring.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL.
#' @param spN character. Used to obtain species name for logger messages
#' @examples
#' \dontrun{
#' envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = TRUE),
#'                       rasName = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = FALSE))
#' # load model
#' m <- readRDS(system.file("extdata/model.RDS",
#'                          package = "wallace"))
#' occsEnvs <- m@@occs
#' bgEnvs <- m@@bg
#' envsFut <- list.files(path = system.file('extdata/wc/future',
#'                                          package = "wallace"),
#'                       full.names = TRUE)
#' envsFut <- raster::stack(envsFut)
#' ## run function
#' xferMess <- xfer_mess(occs = occsEnvs, bg = bgEnvs, bgMsk = envs,
#'                       xferExtRas = envsFut)
#' }
# @return
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>

# @note
#' @seealso \code{\link[dismo]{mess}}, \code{\link{xfer_time}}
#' \code{\link{xfer_userEnvs}}
#' @export

xfer_mess <- function(occs, bg, bgMsk, xferExtRas, logger = NULL, spN = NULL) {

  occsVals <- occs[, names(bgMsk)]
  if (is.null(bg)) {
    allVals <- occsVals
  } else {
    bgVals <- bg[, names(bgMsk)]
    allVals <- rbind(occsVals, bgVals)
  }

  # rename rasters to match originals
  xferExtRas2 <- xferExtRas
  names(xferExtRas2) <- names(bgMsk)

  smartProgress(logger, message = "Generating MESS map...", {
    mss <- suppressWarnings(dismo::mess(xferExtRas2, allVals))
    # for mapping purposes, set all infinite values to NA
    mss[is.infinite(mss)] <- NA
    logger %>% writeLog(hlSpp(spN), "Generated MESS map.")
  })

  return(mss)
}
