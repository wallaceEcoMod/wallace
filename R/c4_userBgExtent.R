c4_userBgExtent <- function(userBgShp_name, userBgShp_datapath, userBgBuf) {
    if (is.null(userBgShp)) {
      logs %>% writeLog(type = 'error', 'Background extent files not uploaded.')
      return()
    }
    
    pathdir <- dirname(userBgShp_datapath)
    pathfile <- basename(userBgShp_datapath)
    # get extensions of all input files
    exts <- sapply(strsplit(userBgShp_name, '\\.'), FUN=function(x) x[2])
    
    if (length(exts) == 1 & exts == 'csv') {
      f <- read.csv(userBgShp_datapath, header = TRUE)
      bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(f)), 1)))
    } else if ('shp' %in% exts) {
      if (length(exts) < 3) {
        logs %>% writeLog(type = 'error', 'If entering a shapefile, please select all the following files: .shp, .shx, .dbf.')
        return()
      }
      file.rename(userBgShp_datapath, file.path(pathdir, userBgShp_name))
      # get index of .shp
      i <- which(exts == 'shp')
      shpName <- strsplit(userBgShp_name[i], '\\.')[[1]][1]
      # read in shapefile and extract coords
      bgExt <- rgdal::readOGR(pathdir[i], shpName)
    } else {
      logs %>% writeLog(type = 'error', 'Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf).')
      return()
    }
    logs %>% writeLog("Study extent: user-defined polygon.")
  
    if (userBgBuf > 0) {
      bgExt <- rgeos::gBuffer(userBgShp(), width = userBgBuf)
      logs %>% writeLog('Study extent buffered by ', userBgBuf, ' degrees.')
    }

    return(bgExt)
}