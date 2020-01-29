env_corrs_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    checkboxInput(ns("batch1"), label = strong("Batch"), value = T), # This line taken from
    actionButton(ns("runCorrs"), "Run module env_corrs")
  )
}

env_corrs_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  bgMask <- common$bgMask

  observeEvent(input$runCorrs, {
    # WARNING ####
 if (input$batch1 == TRUE) spLoop <- allSp() else spLoop <- curSp()

  for (sp in spLoop){
    # FUNCTION CALL ####
    envCorrs <- raster::layerStats(x = bgMask(), na.rm = T, stat = 'pearson')
    # LOAD INTO SPP ####
    spp[[sp]]$procEnvs$envCorrs <- envCorrs[[1]]
    # METADATA ####
  }

  output$envCorrTable <- renderText({
    # Result
    #knitr::kable(spp[[curSp()]]$procEnvs$envCorrs, format = 'html')
    knitr::kable(spp[[sp]]$procEnvs$envCorrs, format = 'html')
  })
  })
}

env_corrs_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  htmlOutput(ns("envCorrTable"))
}

env_corrs_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    module_knit = species$rmm$code$wallaceSettings$someFlag,
    var1 = species$rmm$code$wallaceSettings$someSetting1,
    var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

