correlations_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    ## Add a checkbox for batch processing (more than 1 species)
    checkboxInput(ns("batch1"), label = strong("Batch"), value = T),
    ## Give the action button a name and a label.
    actionButton(ns("runCorrs"), label = "Calculate correlations"),
    ## UI for reselecting variables after calculating correlations
    uiOutput(ns("VarSelect")),
    actionButton(ns("selectConfirm"), label = "Select Variables")
  )
}

correlations_module_server <- function(input, output, session, common) {
  ## Define common objects that will be used in this function
  logger <- common$logger
  spp    <- common$spp
  allSp  <- common$allSp
  curSp  <- common$curSp
  bgMask <- common$bgMask # raster stack after masking by background
  VarSelector <- common$VarSelector

  output$VarSelect <- renderUI({
    req(bgMask())
    shinyWidgets::pickerInput(
      "VarSelector",
      label = "Select variables (**)",
      choices = setNames(as.list(names(bgMask())), names(bgMask())),
      multiple = TRUE,
      selected = names(bgMask()),
      options = list(`actions-box` = TRUE))
  })

  ## When action button defined in the ui function above is clicked, do the following:
  observeEvent(input$runCorrs, {
    req(bgMask())
    # WARNING ####
  # Set up if you want batch to be allowed
  if (input$batch1 == TRUE) spLoop <- allSp() else spLoop <- curSp()
  # If batch is true, loop through all species
  for (sp in spLoop){
    # FUNCTION CALL ####
    smartProgress(logger, message = "Calculating pairwise correlations", {envCorrs <- raster::layerStats(x = bgMask(), na.rm = T, stat = "pearson")
    })
    logger %>% writeLog("Finished calculating correlations")
  #envCorrs <- raster::layerStats(x = bgMask(), na.rm = T, stat = "pearson")

    # LOAD INTO SPP ####
  spp[[sp]]$procEnvs$envCorrs <- envCorrs$`pearson correlation coefficient`
    # METADATA ####
  }
  # Define output as a table
  output$envCorrTable <- renderText({
    # Result
  knitr::kable(spp[[curSp()]]$procEnvs$envCorrs, format = 'html')
  })
  })

  ## Observe when selection is confirmed
  observeEvent(input$selectConfirm, {
    req(spp[[curSp()]]$procEnvs$envCorrs)
    print(spp[[curSp()]]$procEnvs$bgMask)
    print(VarSelector())
#################################################
    ## update bg object
     spp[[curSp()]]$procEnvs$bgMask <- spp[[curSp()]]$procEnvs$bgMask[[VarSelector()]]

    spp[[curSp()]]$procEnvs$bgExt <- spp[[curSp()]]$procEnvs$bgMask[[VarSelector()]]
    print(spp[[curSp()]]$procEnvs$bgExt)
    print(spp[[curSp()]]$procEnvs$bgMask)
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
    correlations_knit = FALSE
    # correlations_knit = species$rmm$code$wallaceSettings$someFlag,
    # var1 = species$rmm$code$wallaceSettings$someSetting1,
    # var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

