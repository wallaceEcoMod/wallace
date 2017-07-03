
bgSelect_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("backgSel"), "Background Extents:",
                 choices = list("Bounding box" = 'bb', "Minimum convex polygon" = 'mcp'))
  )
}

bgSelect_MOD <- function(input, output, session, logs, occs, envs) {
  reactive({
    if (nrow(occs()) <= 2) {
      logs %>% writeLog('<font color="red"><b>! ERROR</b></font> : 
                        Too few localities (<2) to create a background polygon.')
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
      logs %>% writeLog("> Study extent: bounding box.")
    } else if (input$backgSel == 'mcp') {
      bgExt <- mcp(occs()[,2:3])
      # bb <- xy_mcp@polygons[[1]]@Polygons[[1]]@coords
      logs %>% writeLog("> Study extent: minimum convex polygon.")
    }
    
    return(bgExt)
  })
}