
userBgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userBgShp"), label = 'Upload polygon with field order: longitude, latitude (.csv)',
              accept=c(".csv"), multiple=TRUE),
    numericInput("userBgBuf", label = "Study region buffer distance (degree)", value = 0, min = 0, step = 0.5),
    shinyBS::bsPopover('userBgBuf', title = 'Tip',
                       'Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
                       placement = 'right', options = list(container = "body"))
  )
}

userBgExtent_MOD <- function(input, output, session, logs, occs, envs) {
  reactive({
    
    #       file <- shinyFileChoose(input, 'userBgShp', root=c(root='.'))
    #       path <- input$userBgShp$datapath
    
    names <- input$userBgShp$name
    inPath <- input$userBgShp$datapath
    pathdir <- dirname(inPath)
    pathfile <- basename(inPath)
    # get extensions of all input files
    exts <- sapply(strsplit(names, '\\.'), FUN=function(x) x[2])
    
    if (exts == 'csv') {
      f <- read.csv(inPath, header = TRUE)
      
      bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(f)), 1)))
      bgExt <- rgeos::gBuffer(bgExt, width = input$bgBuf)
    } else {
      logs %>% writeLog(type = 'warning', 'Please enter a CSV file of vertex coordinates.')
      return()
    }
    
    #       } else if (exts == 'shp') {
    #         # rename temp files to their original names - nice hack for inputting shapefiles in shiny
    #         sinkRmdob(buf, "Define the buffer size of the study extent:")
    #
    #         sinkRmdmult(c(
    #           file.rename(inPath, file.path(pathdir, names)),
    #           # get index of .shp
    #           i <- which(exts == 'shp'),
    #           # read in shapefile and extract coords
    #           # poly <- readOGR(pathdir[i], strsplit(names[i], '\\.')[[1]][1])),
    #           poly <- readShapePoly(file.path(pathdir[i], strsplit(names[i], '\\.')[[1]][1]))),
    #           "Read the shapefile for the study extent:")
    #
    #
    #         sinkRmdmult(c(
    #           poly <- rgeos::gBuffer(poly, width = buf + raster::res(values$preds)[1]),
    #           values$backgExt <- poly,
    #           bb <- poly@polygons[[1]]@Polygons[[1]]@coords),
    #           "Generate the user-defined study extent plus the buffer:")
    #       }
    
    logs %>% writeLog("Study extent: user-defined polygon.")
    logs %>% writeLog('Study extent buffered by', input$userBgBuf, 'degrees.')
    
    return(bgExt)
  })
}