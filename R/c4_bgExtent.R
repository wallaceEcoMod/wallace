#' @export

c4_bgExtent <- function(occs, envs, bgSel, bgBuf, shinyLogs=NULL) {
  if (is.null(envs)) {
    shinyLogs %>% writeLog(type = 'error', "Before defining the background extent, 
                      obtain environmental data in component 3.")
    return()
  }
  if (nrow(occs) <= 2) {
    shinyLogs %>% writeLog(type = 'error', 'Too few localities (<2) to create a background polygon.')
    return()
  }
  
  # extract just coordinates
  occs.xy <- occs[c('longitude', 'latitude')]

  # make spatial pts object of original occs and preserve origID
  occs.sp <- sp::SpatialPointsDataFrame(occs.xy, data=occs['occID'])
  
  # generate background extent - one grid cell is added to perimeter of each shape
  # to ensure cells of points on border are included
  if (bgSel == 'bb') {
    xmin <- occs.sp@bbox[1]
    xmax <- occs.sp@bbox[3]
    ymin <- occs.sp@bbox[2]
    ymax <- occs.sp@bbox[4]
    bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)
    bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(bb)), 1)))
    msg <- paste(em(spName(occs)), " study extent: bounding box,")
  } else if (bgSel == 'mcp') {
    bgExt <- mcp(occs.xy)
    # bb <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords
    msg <- paste(em(spName(occs)), " study extent: minimum convex polygon,")
  } else if (bgSel == 'ptbuf') {
    if (bgBuf == 0) {
      shinyLogs %>% writeLog(type = 'error', 'Change buffer distance to positive or negative value.')
      return()
    }
    bgExt <- rgeos::gBuffer(occs.sp, width = bgBuf)
    msg <- paste(em(spName(occs)), " study extent: buffered points,")
  }
  
  if (bgBuf > 0 & bgSel != 'ptbuf') {
    bgExt <- rgeos::gBuffer(bgExt, width = bgBuf)
    shinyLogs %>% writeLog(msg, ' with buffer of ', bgBuf, ' degrees.')
  }
  
  # make into SP data frame
  bgExt <- sp::SpatialPolygonsDataFrame(bgExt, data = data.frame(x=1), match.ID = FALSE)
  
  return(bgExt)
}
