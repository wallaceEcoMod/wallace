#' @export

c4_userBgExtent <- function(bgShp_path, bgShp_name, userBgBuf, shinyLogs=NULL) {
    
    pathdir <- dirname(bgShp_path)
    pathfile <- basename(bgShp_path)
    # get extensions of all input files
    exts <- sapply(strsplit(bgShp_name, '\\.'), FUN=function(x) x[2])
    
    if (length(exts) == 1 & exts == 'csv') {
      f <- read.csv(bgShp_path, header = TRUE)
      bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(f)), 1)))
    } else if ('shp' %in% exts) {
      if (length(exts) < 3) {
        shinyLogs %>% writeLog(type = 'error', 'If entering a shapefile, please select all the following files: .shp, .shx, .dbf.')
        return()
      }
      file.rename(bgShp_path, file.path(pathdir, bgShp_name))
      # get index of .shp
      i <- which(exts == 'shp')
      shpName <- strsplit(bgShp_name[i], '\\.')[[1]][1]
      # read in shapefile and extract coords
      bgExt <- rgdal::readOGR(pathdir[i], shpName)
    } else {
      shinyLogs %>% writeLog(type = 'error', 'Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf).')
      return()
    }
    shinyLogs %>% writeLog("Study extent: user-defined polygon.")
  
    if (userBgBuf > 0) {
      bgExt <- rgeos::gBuffer(userBgShp(), width = userBgBuf)
      shinyLogs %>% writeLog('Study extent buffered by ', userBgBuf, ' degrees.')
    }

    return(bgExt)
}