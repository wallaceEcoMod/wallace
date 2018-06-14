
userBgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userBgShp"), label = 'Upload polygon with field order: longitude, latitude (.csv)',
              accept=c(".csv", ".dbf", ".shx", ".shp"), multiple = TRUE),
    tags$div(title='Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
             numericInput(ns("userBgBuf"), label = "Study region buffer distance (degree)", value = 0, min = 0, step = 0.5)),
    checkboxInput(ns("batch"), label = strong("Batch"), value = TRUE)
  )
}

userBgExtent_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(envs())) {
      shinyLogs %>% writeLog(type = 'error', 'Environmental variables missing. Obtain them
                        in component 3.')
      return()
    }
    if (is.null(input$userBgShp)) {
      shinyLogs %>% writeLog(type = 'error', 'Background extent files not uploaded.')
      return()
    }
    # FUNCTION CALL ####
    userBgExt <- c4_userBgExtent(input$userBgShp$datapath,
                                 input$userBgShp$name,
                                 input$userBgBuf,
                                 shinyLogs)
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    # PROCESSING ####
    for(sp in spLoop) {
      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgExt <- userBgExt
      
      # METADATA ####
      # get extensions of all input files
      exts <- sapply(strsplit(input$userBgShp$name, '\\.'), FUN=function(x) x[2])
      if('csv' %in% exts) {
        spp[[sp]]$rmm$code$wallaceSettings$userBgExt <- 'csv'
        spp[[sp]]$rmm$code$wallaceSettings$userBgPath <- input$userBgShp$datapath
      }
      else if('shp' %in% exts) {
        spp[[sp]]$rmm$code$wallaceSettings$userBgExt <- 'shp'
        # get index of .shp
        i <- which(exts == 'shp')
        shpName <- strsplit(input$userBgShp$name[i], '\\.')[[1]][1]
        spp[[sp]]$rmm$code$wallaceSettings$userBgShpParams <- list(dsn=input$userBgShp$datapath[i], layer=shpName)
      }
    }
  })
}

userBgExtent_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  if(is.null(bgExt())) {
    map %>% clearAll() %>%     
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
  }else{
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
    for(shp in bgShpXY()) {
      map %>%
        addPolygons(lng=shp[,1], lat=shp[,2], weight=4, color="gray", group='bgShp')
    }
    bb <- bgExt()@bbox
    map %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
  }
}

userBgExtent_INFO <- infoGenerator(modName = "User-specified Study Region",
                                   modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                                   pkgName = NULL)