c4_drawBgExtent <- function(polyExtXY, polyExtID, drawBgBuf, shinyLogs = NULL) {
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyExtXY)), ID=polyExtID)))
  bgExt <- rgeos::gBuffer(newPoly, width = drawBgBuf)
  return(bgExt)
}