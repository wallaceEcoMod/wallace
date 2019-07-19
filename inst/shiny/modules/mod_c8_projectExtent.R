projExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('projExt'), label = "Select method (**)",
                choices = list("Draw polygon(**)" = 'pjDraw',
                               "User-specified(**)" = 'pjUser',
                               "Same extent (**)" = 'pjCur')),
    conditionalPanel(
      sprintf("input['%s'] == 'pjUser'", ns("projExt")),
      fileInput(ns("userPjShp"),
                label = paste0('Upload polygon in shapefile (.shp, .shx, .dbf) or ',
                               'CSV file with field order (longitude, latitude)'),
                accept = c(".csv", ".dbf", ".shx", ".shp"),
                multiple = TRUE),
      tags$div(title = paste0('Buffer area in degrees (1 degree = ~111 km). Exact',
                              ' length varies based on latitudinal position.'),
               numericInput(ns("userPjBuf"),
                            label = "Study region buffer distance (degree)",
                            value = 0, min = 0, step = 0.5))),
    conditionalPanel(
      sprintf("input['%s'] == 'pjDraw'", ns("projExt")),
      tagList("Draw a polygon and select buffer distance(**)", br(), br(),
              tags$div(
                title = paste0('Buffer area in degrees (1 degree = ~111 km). Exact',
                               ' length varies based on latitudinal position.'),
                numericInput(ns("drawPjBuf"),
                             label = "Study region buffer distance (degree)",
                             value = 0, min = 0, step = 0.5))
      )
    ),
    conditionalPanel(
      sprintf("input['%s'] == 'pjCur'", ns("projExt")),
      tagList('You will use the same extent (**)')
    )
  )
}

projExtent_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      shinyLogs %>%
        writeLog(type = 'error',
                 'Calculate a model prediction in component 7 before projecting.')
      return()
    }
    if (input$projExt == 'pjDraw') {
      if (is.null(spp[[curSp()]]$polyPjXY)) {
        shinyLogs %>%
          writeLog(
            type = 'error',
            paste0("The polygon has not been drawn and finished. Please use the ",
                   "draw toolbar on the left-hand of the map to complete the ",
                   "polygon."))
        return()
      }
    }
    if (input$projExt == 'pjUser') {
      if (is.null(input$userPjShp$datapath)) {
        shinyLogs %>%
          writeLog(
            type = 'error',
            paste0("Specified filepath(s) (**)"))
        return()
      }
    }

    # FUNCTION CALL ####
    if (input$projExt == 'pjDraw') {
      polyPj <- c8_projectDraw(spp[[curSp()]]$polyPjXY, spp[[curSp()]]$polyPjID,
                               input$drawPjBuf, shinyLogs)
      if (input$drawPjBuf == 0 ) {
        shinyLogs %>% writeLog(
          em(spName(occs())), ' : Draw polygon without buffer(**).')
      } else {
        shinyLogs %>% writeLog(
          em(spName(occs())), ' : Draw polygon with buffer of ', input$drawPjBuf,
          ' degrees (**).')
      }
      # METADATA ####
      polyX <- printVecAsis(round(spp[[curSp()]]$polyPjXY[, 1], digits = 4))
      polyY <- printVecAsis(round(spp[[curSp()]]$polyPjXY[, 2], digits = 4))
      spp[[curSp()]]$rmm$code$wallaceSettings$drawExtPolyPjCoords <-
        paste0('X: ', polyX, ', Y: ', polyY)
    }

    if (input$projExt == 'pjUser') {
      polyPj <- c4_userBgExtent(input$userPjShp$datapath,
                                input$userPjShp$name,
                                input$userPjBuf, shinyLogs)
      # METADATA ####
      # get extensions of all input files
      exts <- sapply(strsplit(input$userPjShp$name, '\\.'),
                     FUN = function(x) x[2])
      if('csv' %in% exts) {
        spp[[curSp()]]$rmm$code$wallaceSettings$userPjExt <- 'csv'
        spp[[curSp()]]$rmm$code$wallaceSettings$userPjPath <- input$userPjShp$datapath
      }
      else if('shp' %in% exts) {
        spp[[curSp()]]$rmm$code$wallaceSettings$userPjExt <- 'shp'
        # get index of .shp
        i <- which(exts == 'shp')
        shpName <- strsplit(input$userPjShp$name[i], '\\.')[[1]][1]
        spp[[curSp()]]$rmm$code$wallaceSettings$userPjShpParams <-
          list(dsn = input$userPjShp$datapath[i], layer = shpName)
      }
    }

    if (input$projExt == 'pjCur') {
      polyPj <- spp[[curSp()]]$procEnvs$bgExt
      shinyLogs %>% writeLog(
        em(spName(occs())), ' : Projection extent equal to current extent region. (**)')
    }

    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjExt <- polyPj

  })
}
