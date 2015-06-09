## -------------------------------------------------------------------- ##
## Define functions
## -------------------------------------------------------------------- ##

# for naming files
nameAbbr <- function(spname) {
  namespl <- strsplit(tolower(spname[1,1]), " ")
  genusAbbr <- substring(namespl[[1]][1], 1, 1)
  fullNameAbbr <- paste0(genusAbbr, "_", namespl[[1]][2])
  return(fullNameAbbr)
}

# make a minimum convex polygon as SpatialPolygons object
mcp <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1))))
}


makeOccIcons <- function(width = 10, height = 10, ...) {
  occIcons <- c('H', 'O', 'P', 'U', 'F', 'M', 'I', 'L', 'A', 'X')
  files <- character(9)
  # create a sequence of png images
  for (i in 1:9) {
    f <- tempfile(fileext = '.png')
    png(f, width = width, height = height, bg = 'transparent')
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = occIcons[i], cex = min(width, height) / 8, col='red', ...)
    dev.off()
    files[i] <- f
  }
  files
}

popUpContent <- function(x) {
  as.character(tagList(
    tags$strong(paste("ID:", x['row'])),
    tags$br(),
    tags$strong(paste("Latitude:", x['lat'])),        
    tags$strong(paste("Longitude:", x['lon']))
  ))
}