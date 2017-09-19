
bgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("bgSel"), "Background Extents:",
                 choices = list("Bounding box" = 'bb', 
                                "Minimum convex polygon" = 'mcp',
                                "Point buffers" = 'ptbuf'),
                 selected='bb'),
    tags$div(title='Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
             numericInput(ns("bgBuf"), label = "Study region buffer distance (degree)", value = 0.5, min = 0, step = 0.5))
  )
}

bgExtent_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$envs)) {
      rvs %>% writeLog(type = 'error', "Before defining the background extent, 
                       obtain environmental data in component 3.")
      return()
    }
    if (nrow(rvs$occs) <= 2) {
      rvs %>% writeLog(type = 'error', 'Too few localities (<2) to create a background polygon.')
      return()
    }
    
    # record for RMD
    rvs$comp4.shp <- input$bgSel
    rvs$comp4.buf <- input$bgBuf
    
    # extract just coordinates
    occs.xy <- rvs$occs[c('longitude', 'latitude')]
    # make spatial pts object of original occs and preserve origID
    occs.sp <- sp::SpatialPointsDataFrame(occs.xy, data=rvs$occs['occID'])
    
    # generate background extent - one grid cell is added to perimeter of each shape
    # to ensure cells of points on border are included
    if (input$bgSel == 'bb') {
      xmin <- occs.sp@bbox[1]
      xmax <- occs.sp@bbox[3]
      ymin <- occs.sp@bbox[2]
      ymax <- occs.sp@bbox[4]
      bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)
      bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(bb)), 1)))
      msg <- "Study extent: bounding box."
    } else if (input$bgSel == 'mcp') {
      bgExt <- mcp(occs.xy)
      # bb <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords
      msg <- "Study extent: minimum convex polygon."
    } else if (input$bgSel == 'ptbuf') {
      if (input$bgBuf == 0) {
        rvs %>% writeLog(type = 'error', 'Change buffer distance to positive or negative value.')
        return()
      }
      bgExt <- rgeos::gBuffer(occs.sp, width = input$bgBuf)
      msg <- "Study extent: buffered points."
    }
    
    if (input$bgBuf > 0) {
      bgExt <- rgeos::gBuffer(bgExt, width = input$bgBuf)
      rvs %>% writeLog(msg, 'Study extent buffered by', input$bgBuf, 'degrees.')
    }
    
    return(bgExt)
  })
}
