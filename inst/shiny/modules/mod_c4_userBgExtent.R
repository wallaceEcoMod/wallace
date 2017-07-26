
userBgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userBgShp"), label = 'Upload polygon with field order: longitude, latitude (.csv)',
              accept=c(".csv", ".dbf", ".shx", ".shp"), multiple = TRUE),
    numericInput(ns("userBgBuf"), label = "Study region buffer distance (degree)", value = 0, min = 0, step = 0.5),
    shinyBS::bsPopover('userBgBuf', title = 'Tip',
                       'Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
                       placement = 'right', options = list(container = "body"))
  )
}

userBgExtent_MOD <- function(input, output, session, rvs) {
  userBgShp <- reactive({
    req(input$userBgShp)
    
    # record for RMD
    rvs$bgBuf <- input$userBgBuf
    
    names <- input$userBgShp$name
    inPath <- input$userBgShp$datapath
    pathdir <- dirname(inPath)
    pathfile <- basename(inPath)
    # get extensions of all input files
    exts <- sapply(strsplit(names, '\\.'), FUN=function(x) x[2])
    
    if (length(exts) == 1 & exts == 'csv') {
      # record for RMD
      rvs$bgUser <- 'csv'
      rvs$bgUserCSVPath <- inPath
      f <- read.csv(inPath, header = TRUE)
      
      bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(f)), 1)))
    } else if ('shp' %in% exts) {
      if (length(exts) < 3) {
        rvs %>% writeLog(type = 'error', 'If entering a shapefile, please select all the following files: .shp, .shx, and .dbf.')
        return()
      }
      file.rename(inPath, file.path(pathdir, names))
      # get index of .shp
      i <- which(exts == 'shp')
      shpName <- strsplit(names[i], '\\.')[[1]][1]
      # record for RMD
      rvs$bgUser <- 'shp'
      rvs$bgUserShpPar <- list(dsn=pathdir[i], layer=shpName)
      # read in shapefile and extract coords
      bgExt <- rgdal::readOGR(pathdir[i], shpName)
    } else {
      rvs %>% writeLog(type = 'error', 'Please enter either a CSV file of vertex coordinates or shapefile (.shp, .dbf, and .shx).')
      return()
    }
    rvs %>% writeLog("Study extent: user-defined polygon.")
    return(bgExt)
  })
  
  bufBg <- reactive({
    req(userBgShp())
    
    bufWid <- input$userBgBuf
    if (bufWid > 0) {
      bgExt <- rgeos::gBuffer(userBgShp(), width = bufWid)
      rvs %>% writeLog('Study extent buffered by', bufWid, 'degrees.')
    } else {
      bgExt <- userBgShp()
    }
    return(bgExt)
  })
  
  return(bufBg)
}