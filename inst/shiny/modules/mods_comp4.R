comp4_studyReg <- function(buf, backgSel) {
  if (nrow(values$df) <= 2) {
    isolate(writeLog("ERROR: Too few localities (<2) to create a background polygon."))
    return()
  }
  # generate background extent - one grid cell is added to perimeter of each shape
  # to ensure cells of points on border are included
  if (backgSel == 'bb') {
    xmin <- min(values$df$longitude) - (buf + res(values$preds)[1])
    xmax <- max(values$df$longitude) + (buf + res(values$preds)[1])
    ymin <- min(values$df$latitude) - (buf + res(values$preds)[1])
    ymax <- max(values$df$latitude) + (buf + res(values$preds)[1])
    bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)
    values$backgExt <- SpatialPolygons(list(Polygons(list(Polygon(bb)), 1)))
    values$bbTxt <- 'bounding box'
  } else if (backgSel == 'mcp') {
    xy_mcp <- mcp(values$df[,2:3])
    xy_mcp <- gBuffer(xy_mcp, width = buf + res(values$preds)[1])
    values$backgExt <- xy_mcp
    bb <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords
    values$bbTxt <- 'minimum convex polygon'
  } else if (backgSel == 'user') {
    if (is.null(input$userBackg)) return()
    #       file <- shinyFileChoose(input, 'userBackg', root=c(root='.'))
    #       path <- input$userBackg$datapath
    
    names <- input$userBackg$name
    inPath <- input$userBackg$datapath
    pathdir <- dirname(inPath)
    pathfile <- basename(inPath)
    # get extensions of all input files
    exts <- sapply(strsplit(names, '\\.'), FUN=function(x) x[2])
    
    if (exts == 'csv') {
      shp <- read.csv(inPath, header = TRUE)
      
      shp <- SpatialPolygons(list(Polygons(list(Polygon(shp)), 1)))
      shp <- gBuffer(shp, width = buf + res(values$preds)[1])
      values$backgExt <- shp
      bb <- shp@polygons[[1]]@Polygons[[1]]@coords
    } else {
      isolate(writeLog("* WARNING: Please enter a CSV file of vertex coordinates for user-specified polygon."))
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
    #           poly <- gBuffer(poly, width = buf + res(values$preds)[1]),
    #           values$backgExt <- poly,
    #           bb <- poly@polygons[[1]]@Polygons[[1]]@coords),
    #           "Generate the user-defined study extent plus the buffer:")
    #       }
    values$bbTxt <- 'user-defined'
  }
  isolate(writeLog(paste0("* Study extent: ", values$bbTxt, ".")))
  isolate(writeLog(paste('* Study extent buffered by', buf, 'degrees.')))
  
  values$bb <- bb
  proxy %>% fitBounds(max(bb[,1]), max(bb[,2]), min(bb[,1]), min(bb[,2]))
  proxy %>% addPolygons(lng=bb[,1], lat=bb[,2], layerId="backext",
                        weight=10, col="red")
}

comp4_mskStudyReg <- function() {
  # clip and mask rasters based on study region
  withProgress(message = "Processing environmental data...", {
    predCrop <- crop(values$preds, values$backgExt)
    values$predsMsk <- mask(predCrop, values$backgExt)
  })
  isolate(writeLog(paste0('* Environmental data masked by ', values$bbTxt, '.')))
  
  if (is.null(values$bg.coords)) {
    withProgress(message = "Generating background points...", {
      bg.coords <- randomPoints(values$predsMsk, 10000)
      values$bg.coords <- as.data.frame(bg.coords)
    })
    isolate(writeLog(paste0('* Random background points sampled (N = 10,000).')))
  }
}