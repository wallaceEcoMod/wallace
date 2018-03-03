
userBgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userBgShp"), label = 'Upload polygon with field order: longitude, latitude (.csv)',
              accept=c(".csv", ".dbf", ".shx", ".shp"), multiple = TRUE),
    tags$div(title='Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
             numericInput(ns("userBgBuf"), label = "Study region buffer distance (degree)", value = 0, min = 0, step = 0.5))
  )
}

userBgExtent_MOD <- function(input, output, session) {
  reactive({
    if (is.null(spp[[curSp()]]$envs)) {
      logs %>% writeLog(type = 'error', 'Environmental variables missing. Obtain them
                        in component 3.')
      return()
    }
    # FUNCTION CALL ####
    userBgExt <- c4_userBgExtent(input$userBgShp$name,
                    input$userBgShp$datapath,
                    input$userBgBuf,
                    logs, shiny = TRUE)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$procEnvs$bgExt <- userBgExt
    
    # METADATA ####
    # get extensions of all input files
    exts <- sapply(strsplit(userBgShp_name, '\\.'), FUN=function(x) x[2])
    if('csv' %in% exts) {
      rmm$code$wallaceSettings$userBgExt <- 'csv'
      rmm$code$wallaceSettings$userBgPath <- input$userBgShp$datapath
    }
    else if('shp' %in% exts) {
      rmm$code$wallaceSettings$userBgExt <- 'shp'
      # get index of .shp
      i <- which(exts == 'shp')
      shpName <- strsplit(userBgShp_name[i], '\\.')[[1]][1]
      rmm$code$wallaceSettings$userBgShpParams <- list(dsn=input$userBgShp$datapath[i], layer=shpName)
    }
    
    # return(userBgExt)
  })
}