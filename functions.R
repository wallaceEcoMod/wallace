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

# this is currently only used for the raster mapping, but was originally used for all maps
# before we implemented leaflet (the version of leaflet we used did not have raster plotting
# functionality)-- we plan to recode using the new leaflet version (with new syntax) as a next 
# step, whereupon a leaflet map will be used for plotting rasters too
plotMap <- function(pts=NULL, poly=NULL, pred=NULL, addpo=NULL, pts2=NULL) {
  mapWorld <- borders('world', colour = 'white', fill = 'white')
  if (!is.null(pts)) {
    xl <- c(min(pts$lon) - 5, max(pts$lon) + 5)
    yl <- c(min(pts$lat) - 5, max(pts$lat) + 5)
    mp <- ggplot(pts, aes(x = lon, y = lat)) + 
      mapWorld + theme(panel.background = element_rect(fill = 'lightblue')) +
      geom_point(size = 3, colour = 'blue', position = 'jitter', shape = 1) +
      coord_cartesian(xlim = xl, ylim = yl) + 
      geom_text(label = row.names(pts), hjust = 1, vjust = -1)
    if (!is.null(poly)) {
      mp <- mp + geom_path(aes(x = long, y = lat), fortify(poly), colour = 'red')
    }
    print(mp)
  }
  if (!is.null(pred)) {
    # Convert raster to points
    pred.p <- rasterToPoints(pred)
    # Make the points a dataframe for ggplot
    pred.df <- data.frame(pred.p)
    #Make appropriate column headings
    colnames(pred.df) <- c("lon", "lat", "val")
    e <- extent(pred)
    if(!addpo){
      mp <- ggplot(data = pred.df, aes(x = lon, y = lat)) + 
        mapWorld + theme(panel.background = element_rect(fill = 'lightblue')) + 
        geom_raster(aes(fill = val)) + 
        coord_equal() +
        coord_cartesian(xlim =c(e[1] - 2, e[2] + 2), ylim =c(e[3] - 2, e[4] + 2)) +
        #scale_fill_gradient("relative suitability (raw output)", limits = c(pred@data@min, pred@data@max), low = 'grey', high = 'blue')
        scale_fill_gradientn("relative suitability (raw output)", limits = c(pred@data@min, pred@data@max), colours=matlab.like2(50))
    }else{
      mp <- ggplot(data = pred.df, aes(x = lon, y = lat)) + 
        mapWorld + theme(panel.background = element_rect(fill = 'lightblue')) + 
        geom_raster(aes(fill = val)) + 
        coord_equal() +
        coord_cartesian(xlim =c(e[1] - 2, e[2] + 2), ylim =c(e[3] - 2, e[4] + 2)) +
        #scale_fill_gradient("relative suitability (raw output)", limits = c(pred@data@min, pred@data@max), low = 'grey', high = 'blue')
        scale_fill_gradientn("relative suitability (raw output)", limits = c(pred@data@min, pred@data@max), colours=matlab.like2(50))
      mp <- mp + geom_point(data=pts2, aes(x = lon, y = lat), size = 3, colour = 'red',
                            position = 'jitter', shape = 1)  
    }
    print(mp)
  }
}