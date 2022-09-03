#' @title indic_overlap
#' @description Overlap range map with shapefile or raster
#' @param rangeMap sf polygon. A polygon with the range map.
#' @param inputOverlap sf polygon or raster. A feature to apply overlap.
#' @param field character. Name of the field to select categories/attributes.
#' NULL is inputOverlap is a raster.
#' @param category character. String with names of the categories/attributes
#' selected to overlap with range map. NULL is inputOverlap is a raster.
#' @param logger logger
#' @param spN species name
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'

indic_overlap <- function(rangeMap, inputOverlap, field = NULL,
                          category = NULL, logger = NULL, spN = NULL) {
  if ("sf" %in% class(inputOverlap)) {
    # GEPB: Uncomment when subfield in changeRangeR::ratioOverlap is working
    # catAv <- unique(inputOverlap[[field]])
    # if (length(catAv) == length(category)) {
    #   categoryUse <- "All"
    # } else {
    #   categoryUse <- category
    # }
    smartProgress(
      logger,
      message = "Calculating range overlap ", {
        sf::sf_use_s2(FALSE)
        ratioOverlap <- changeRangeR::ratioOverlap(
          r = sf::as_Spatial(rangeMap),
          shp = sf::as_Spatial(inputOverlap),
          field = field,
          category = category)
        simpRangeMap <- sf::st_union(rangeMap)
        simpInputOverlap <- sf::st_union(
          subset(inputOverlap, inputOverlap[[field]] %in% category))
        overlapPolygon <- sf::st_intersection(simpRangeMap, simpInputOverlap)
      }
    )
  } else if ("RasterLayer" %in% class(inputOverlap)) {
    smartProgress(
      logger,
      message = "Calculating range overlap ", {
        ratioOverlap <- changeRangeR::ratioOverlap(
          r = sf::as_Spatial(rangeMap),
          shp = inputOverlap)
        # GEPB: Not sure when this scenario occurs.
        # if (is.list(maskedRange) & length(maskedRange) > 1) {
        #   names(maskedRange) <- NULL
        #   maskedRange$fun <- mean
        #   maskedRange$na.rm <- TRUE
        #   maskedRange <- do.call(raster::mosaic, maskedRange)
        # }
        simpRangeMap <- sf::st_union(rangeMap)
        simpInputOverlap <- terra::rast(inputOverlap)
        simpInputOverlap[!is.na(simpInputOverlap)] <- 1
        simpInputOverlap <- terra::as.polygons(simpInputOverlap) %>% sf::st_as_sf()
        overlapPolygon <- sf::st_intersection(simpRangeMap, simpInputOverlap)
      }
    )
  }
  return(list(overlapPolygon = overlapPolygon,
              overlapRatio = ratioOverlap$ratio))
}
