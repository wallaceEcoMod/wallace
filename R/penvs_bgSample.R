
#' @title penvs_bgSample Sample background points
#' @description This function samples background points from an area determined
#'   by a rasterBrick or RasterStack of environmental layers previously cropped
#'   and masked to user determined extent.
#'
#' @details
#' This function is used in the select study region component. Here, a user
#'   provided amount of points is randomly sampled from the RasterBrick or
#'   RasterStack of environmental variables cropped and masked to a given
#'   background extent. The maximum number of points to be sampled is the number
#'   of non NA cells in each layer of the reference RasterBrick or RasterStack
#'   If the requested number of points is larger than the number of cells in
#'   the reference RasterBrick or RasterStack then only a proportion of the
#'   requested will be returned.
#'
#' @param occs data frame of cleaned or processed occurrences obtained from
#'   components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param bgMask a RasterStack or a RasterBrick of environmental layers cropped
#'   and masked.
#' @param bgPtsNum numeric. Number of points to be sampled from the area, they
#'   will be sampled as long as <= non NA cells in any reference layer.
#' @param logger  Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL.
#' @param spN data frame of cleaned occurrences obtained from component occs:
#'   Obtain occurrence data. Used to obtain species name for logger messages.
#' @examples
#' \dontrun{
#' occs <-  occs_queryDb(spName = "Panthera onca", occDb = "gbif",
#'                       occNum = 100)
#' occs <- as.data.frame(occs[[1]]$cleaned)
#' envs <- envs_worldclim(bcRes = 10,
#'                        bcSel = c("bio03", "bio04", "bio13", "bio14"),
#'                        doBrick = TRUE)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMask <- penvs_bgMask(occs, envs, bgExt)
#' bgsample <- penvs_bgSample(occs, bgMask, bgPtsNum = 1000)
#' }
#'
#' @return a dataframe containing point coordinates (longitude and latitude).
#' All points are within the area provided in the RasterBrick or RasterStack (bgMask).
#' Maximum number of points is equal to non NA cells in each layer of the
#' reference brick or stack.
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @seealso \code{\link{penvs_bgMask}}, \code{\link{penvs_bgExtent}}
#'   \code{\link{penvs_userBgExtent}}, \code{\link{penvs_drawBgExtent}},
#'   \code{\link[dismo]{randomPoints}}
#' @importFrom rlang .data
#' @export

penvs_bgSample <- function(occs, bgMask, bgPtsNum, logger = NULL, spN = NULL) {
  # sample random background points
  smartProgress(logger, message = "Generating background points...", {
    bgXY <- dismo::randomPoints(bgMask, bgPtsNum)
    bgXY <- bgXY %>% as.data.frame() %>%
      dplyr::select(longitude = .data$x, latitude = .data$y)
    bgNonNA <- raster::ncell(bgMask) - raster::freq(bgMask, value = NA)[[1]]
  })
  bg.prop <- round(nrow(bgXY)/bgPtsNum, digits = 2)
  if(bg.prop == 1) {
    logger %>%
      writeLog(
        hlSpp(spN), bgPtsNum, " random background points sampled out of ",
        bgNonNA, " total points. ")
  } else {
    logger %>%
      writeLog(type = "warning",
        hlSpp(spN), bgPtsNum, " random background points requested, but only ",
        100 * bg.prop, "% of points (n = ", nrow(bgXY), ") were able to be sampled. ",
        "The maximum number of background points available to be sample on the polygon extent is ",
        bgNonNA, ".")
  }
  return(bgXY)
}
