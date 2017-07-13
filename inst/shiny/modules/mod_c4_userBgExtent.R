
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
    names <- input$userBgShp$name
    inPath <- input$userBgShp$datapath
    pathdir <- dirname(inPath)
    pathfile <- basename(inPath)
    # get extensions of all input files
    exts <- sapply(strsplit(names, '\\.'), FUN=function(x) x[2])
    
    if (length(exts) == 1 & exts == 'csv') {
      f <- read.csv(inPath, header = TRUE)
      
      bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(f)), 1)))
    } else if ('shp' %in% exts) {
      file.rename(inPath, file.path(pathdir, names))
      # get index of .shp
      i <- which(exts == 'shp')
      # read in shapefile and extract coords
      bgExt <- readOGR(pathdir[i], strsplit(names[i], '\\.')[[1]][1])
    } else {
      rvs %>% writeLog(type = 'warning', 'Please enter either a CSV file of vertex coordinates or shapefile.')
      return()
    }
    rvs %>% writeLog("Study extent: user-defined polygon.")
    return(bgExt)
  })
  
  bufBg <- reactive({
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