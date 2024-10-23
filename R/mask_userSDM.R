# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# mask_userSDM.R
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
#' @title  mask_userSDM
#' @description Upload user-specified SDM prediction
#'
#' @details This function uploads a raster of a species' range prediction to be
#' used in Wallace EcoMod. The file name needs to named in Genus_species format
#' (Ex: Bassaricyon_neblina.tif) to have Wallace functionality. The raster can
#' be binary or continuous, but may need to be binary for some further analyses
#' in Wallace. This function returns a list of the range prediction raster as
#' well as a spatial polygon of the extent of the range prediction.
#'
#' @param rasPath character of path to raster, must be the full path including file name and extension.
#' Filename need to be name with genus_species format.
#' @param rasName character vector of raster names to be assigned to loaded raster
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN Species name
#'
#' @examples
#' \dontrun{
#' ### Set parameters
#' rasPath <- system.file("extdata/Bassaricyon_neblina.tif",package = "wallace")
#' rasName <- "Bassaricyon_neblina.tif"
#' ### Run function
#' sdm <- mask_userSDM(rasPath, rasName, logger = NULL, spN = NULL)
#' }
#'
#' @return A list of two: the prediction rasterlayer and a spatialpolygon of the extent
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#'
# @keywords
# @note
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export
#'
#'

mask_userSDM <- function(rasPath, rasName, logger = NULL, spN = NULL)  {
  r <- raster::raster(rasPath)
  if (!raster::couldBeLonLat(r)) {
    logger %>%
      writeLog(
        type = "error", hlSpp(spN),
        'Input rasters have undefined coordinate reference system (CRS). ',
        'Mapping functionality will not work. ',
        'Please define their projections and upload again. ',
        'See guidance text in this module for more details.')
    return()
  }
  smartProgress(logger, message = "Uploading user-specified SDM...", {
    r <- raster::trim(r)
    names(r) <- fileNameNoExt(rasName)
    extPoly <- raster::extent(r)
    if (extPoly@xmin < -180 | extPoly@xmax > 180 |
        extPoly@ymin < -90 | extPoly@ymax > 90) {
      logger %>%
        writeLog(
          type = "error", "Wrong extent projection. '", rasName,"' cannot be uploaded. ")
      return()
    }
    extPoly <- methods::as(extPoly, 'SpatialPolygons')
    return(list(sdm = r, extSdm = extPoly))
  })
}
