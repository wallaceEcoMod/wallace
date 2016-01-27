source("functions.R")

map_drawPolys <- function(mapClick, component) {
  lonlat <- c(mapClick$lng, mapClick$lat)
  # this functionality prevents existing map click from being added to new polygon
  if (values$polyErase) {
    if (identical(lonlat, values$mapClick)) {
      return()
    } else {
      values$polyErase <- FALSE
    }
  }
  if (is.null(mapClick)) return()
  values$mapClick <- lonlat
  if (component == 2) {
    values$drawPolyCoordsSelLocs <- isolate(rbind(values$drawPolyCoordsSelLocs, lonlat))
    proxy %>% removeShape("drawPolySelLocs")
    proxy %>% addPolygons(values$drawPolyCoordsSelLocs[,1], values$drawPolyCoordsSelLocs[,2],
                          layerId='drawPolySelLocs', fill=FALSE, weight=3, color='green')
  }
  if (component == 8) {
    values$drawPolyCoordsProjExt <- isolate(rbind(values$drawPolyCoordsProjExt, lonlat))
    proxy %>% removeShape("drawPolyProjExt")
    proxy %>% addPolygons(values$drawPolyCoordsProjExt[,1], values$drawPolyCoordsProjExt[,2],
                          layerId='drawPolyProjExt', fill=FALSE, weight=4, color='red')
  }
  
}

map_plotLocs <- function(locs, clearMarkers=TRUE, clearShapes=TRUE, clearImages=TRUE, fillColor='red') {
  if (clearMarkers) proxy %>% clearMarkers()
  if (clearShapes) proxy %>% clearShapes()
  if (clearImages) proxy %>% clearImages()
  proxy %>% addCircleMarkers(data = locs, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red',
                             fill = TRUE, fillColor = fillColor, weight = 2, popup = ~pop)
}

