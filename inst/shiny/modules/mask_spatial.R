mask_spatial_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Upload Spatial Data(**)", class = "stepText"), br(), br(),
    fileInput(ns("maskShp"),
              label = 'Upload polygon in shapefile (.shp, .shx, .dbf)',
              accept = c(".dbf", ".shx", ".shp"), multiple = TRUE),
    actionButton(ns("goMaskShp"), "Load (**)"), br(),
    tags$hr()
  )
}

mask_spatial_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  logger <- common$logger

  observeEvent(input$goMaskShp, {
    # WARNING ####
    if (is.null(spp[[curSp()]]$postProc$prediction)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()), 'Calculate/Upload a model prediction (**)')
      return()
    }
    if (is.null(input$maskShp$datapath)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()), "Specified filepath(s) (**)")
      return()
    }
    # FUNCTION CALL ####
    spatialMask <- mask_spatialPoly(input$maskShp$datapath, input$maskShp$name,
                                    spp[[curSp()]]$procEnvs$bgExt,
                                    logger, spN = curSp())
    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$spatialMask <- spatialMask

    logger %>% writeLog(hlSpp(curSp()), "Spatial data uploaded.")
    # METADATA ####

  })

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO SPP ####

    # METADATA ####
  })

  output$result <- renderText({
    # Result
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))

}

mask_spatial_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

mask_spatial_module_map <- function(map, common) {
  # Map logic
}

mask_spatial_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_spatial_knit = FALSE
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  )
}

