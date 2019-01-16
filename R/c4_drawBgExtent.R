c4_drawBgExtent <- function(polyExtXY, polyExtID, drawBgBuf, occs, 
                            shinyLogs = NULL) {
  ptRem <- NULL
  occs.xy <- occs[c('longitude', 'latitude')]
  # make spatial pts object of original occs and preserve origID
  pts <- sp::SpatialPointsDataFrame(occs.xy, data=occs['occID'])
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyExtXY)), ID=polyExtID)))
  intersect <- sp::over(pts, newPoly)
  ptRem <- ifelse(all(!is.na(intersect)), 0, as.numeric(which(is.na(intersect))))
  if (ptRem == 0) {
    bgExt <- rgeos::gBuffer(newPoly, width = drawBgBuf)
    if (drawBgBuf == 0 ) {
      shinyLogs %>% writeLog(em(spName(occs)), ' : Draw polygon without buffer(**).')
    } else {
      shinyLogs %>% writeLog(em(spName(occs)), ' : Draw polygon with buffer of ', drawBgBuf, ' degrees (**).')
    }
    bgExt <- sp::SpatialPolygonsDataFrame(bgExt, data = data.frame(x=1), match.ID = FALSE)
    return(bgExt)
  } else if (ptRem > 0) {
    shinyLogs %>% writeLog(type = 'error', 
                           "The draw polygon did not include all localities(**). Remove the polygon before to draw a new one.")
    return()
  }
}