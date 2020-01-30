correlations_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    ## Add a checkbox for batch processing (more than 1 species)
    checkboxInput(ns("batch1"), label = strong("Batch"), value = T),
    ## Give the action button a name and a label.
    actionButton(ns("runCorrs"), "Calculate correlations")
  )
}

correlations_module_server <- function(input, output, session, common) {
  ## Define common objects that will be used in this function
  logger <- common$logger
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  bgMask <- common$bgMask # raster stack after masking by background

  ## When action button defined in the ui function above is clicked, do the following:
  observeEvent(input$runCorrs, {
    # WARNING ####
  # Set up if you want batch to be allowed
  if (input$batch1 == TRUE) spLoop <- allSp() else spLoop <- curSp()
  # If batch is true, loop through all species
  for (sp in spLoop){
    # FUNCTION CALL ####
  envCorrs <- raster::layerStats(x = bgMask(), na.rm = T, stat = "pearson")
    # LOAD INTO SPP ####
  spp[[sp]]$procEnvs$envCorrs <- envCorrs[[1]]
    # METADATA ####
  }
  # Define output as a table
  output$envCorrTable <- renderText({
    # Result
  knitr::kable(spp[[sp]]$procEnvs$envCorrs, format = 'html')
  })
  })
}


correlations_module_result <- function(id) {
  ns <- NS(id)

  # Result UI as html
  htmlOutput(ns("envCorrTable"))
}

correlations_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    module_knit = species$rmm$code$wallaceSettings$someFlag,
    var1 = species$rmm$code$wallaceSettings$someSetting1,
    var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

