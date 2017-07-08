
bgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("backgSel"), "Background Extents:",
                 choices = list("Bounding box" = 'bb', "Minimum convex polygon" = 'mcp')),
    numericInput(ns("bgBuf"), label = "Study region buffer distance (degree)", value = 0, min = 0, step = 0.5),
    shinyBS::bsPopover(ns("bgBuf"), title = 'Tip',
                       'Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
                       placement = 'right', options = list(container = "body"))
  )
}

bgExtent_MOD <- function(input, output, session, logs, occs) {
  reactive({
    req(occs())
    
    if (nrow(occs()) <= 2) {
      logs %>% writeLog(type = 'error', 'Too few localities (<2) to create a background polygon.')
      return()
    }
    # generate background extent - one grid cell is added to perimeter of each shape
    # to ensure cells of points on border are included
    if (input$backgSel == 'bb') {
      lon <- occs()$longitude
      lat <- occs()$latitude
      xmin <- min(lon)
      xmax <- max(lon)
      ymin <- min(lat)
      ymax <- max(lat)
      bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)
      bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(bb)), 1)))
      logs %>% writeLog("Study extent: bounding box.")
    } else if (input$backgSel == 'mcp') {
      bgExt <- mcp(occs()[,2:3])
      # bb <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords
      logs %>% writeLog("Study extent: minimum convex polygon.")
    }
    
    if (input$bgBuf > 0) {
      bgExt <- rgeos::gBuffer(bgExt, width = input$bgBuf)
      logs %>% writeLog('Study extent buffered by', input$bgBuf, 'degrees.')
    }
    
    return(bgExt)
  })
}
