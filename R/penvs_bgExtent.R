
#' @title penvs_bgExtent
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs x
#' @param envs x
#' @param bgSel x
#' @param bgBuf x
#' @param logger x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export
#'

penvs_bgExtent <- function(occs, bgSel, bgBuf, logger = NULL) {

  if (nrow(occs) <= 2) {
    logger %>% writeLog(type = 'error', 'Too few localities (<2) to create a background polygon.')
    return()
  }

  # extract just coordinates
  occs.xy <- occs[c('longitude', 'latitude')]

  # make spatial pts object of original occs and preserve origID
  occs.sp <- sp::SpatialPointsDataFrame(occs.xy, data=occs['occID'])

  # generate background extent - one grid cell is added to perimeter of each shape
  # to ensure cells of points on border are included
  if (bgSel == 'bounding box') {
    xmin <- occs.sp@bbox[1]
    xmax <- occs.sp@bbox[3]
    ymin <- occs.sp@bbox[2]
    ymax <- occs.sp@bbox[4]
    bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)
    bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(bb)), 1)))
    msg <- paste(em(spName(occs)), " study extent: bounding box.")
  } else if (bgSel == "minimum convex polygon") {
    bgExt <- mcp(occs.xy)
    # bb <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords

  } else if (bgSel == 'point buffers') {
    if (bgBuf == 0) {
      logger %>% writeLog(type = 'error', 'Change buffer distance to positive
                             or negative value.')
      return()
    }
    bgExt <- rgeos::gBuffer(occs.sp, width = bgBuf)
    msg <- paste(em(spName(occs)), " study extent: buffered points.")
  }
  msg <- paste0(em(spName(occs)), " study extent: ", bgSel, ".")

  if (bgBuf > 0 & bgSel != 'point buffers') {
    bgExt <- rgeos::gBuffer(bgExt, width = bgBuf)
    logger %>% writeLog(msg, ' Buffered by ', bgBuf, ' degrees.')
  }else{
    logger %>% writeLog(msg)
  }

  # make into SP data frame
  bgExt <- sp::SpatialPolygonsDataFrame(bgExt, data = data.frame(x=1), match.ID = FALSE)

  return(bgExt)
}
