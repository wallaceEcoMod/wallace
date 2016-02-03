# clear current mapped features and plot occurrence locations
map_plotLocs <- function(locs, clearMarkers=TRUE, clearShapes=TRUE, clearImages=TRUE, fillColor='red', fillOpacity=0.2) {
  if (clearMarkers) proxy %>% clearMarkers()
  if (clearShapes) proxy %>% clearShapes()
  if (clearImages) proxy %>% clearImages()
  proxy %>% addCircleMarkers(data = locs, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fill = TRUE, 
                             fillColor = fillColor, fillOpacity = fillOpacity, 
                             weight = 2, popup = ~pop)
}