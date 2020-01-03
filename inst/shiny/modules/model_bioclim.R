model_bioclim_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE),
    actionButton(ns('goBIOCLIM'), 'Run')
  )
}

model_bioclim_module_server <- function(input, output, session, common) {

  allSp <- common$allSp
  curSp <- common$curSp
  spp <- common$spp
  logger <- common$logger

  observeEvent(input$goBIOCLIM, {
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    # PROCESSING ####
    for(sp in spLoop) {
      # ERRORS ####
      if(is.null(spp[[sp]]$occs$partition)) {
        logger %>% writeLog(
          type = 'error',
          "Before building a model, please partition occurrences for ",
          "cross-validation for ", spName(spp[[sp]]), ".")
        return()
      }

      # FUNCTION CALL ####
      m.bioclim <- model_bioclim(occs = spp[[sp]]$occs,
                                 bg = spp[[sp]]$bg,
                                 occsGrp = spp[[sp]]$occs$partition,
                                 bgGrp = spp[[sp]]$bg$partition,
                                 bgMsk = spp[[sp]]$procEnvs$bgMask,
                                 logger)

      req(m.bioclim)

      # LOAD INTO SPP ####
      spp[[sp]]$evalOut <- m.bioclim

      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- "BIOCLIM"
      spp[[sp]]$rmm$model$bioclim$notes <- "ENMeval/dismo package implementation"
    }
    common$update_component(tab = "Results")
    common$remove_module(component = "vis", module = "vis_mxEvalPlot")
    common$remove_module(component = "vis", module = "vis_responsePlot")
  })

  output$bioclimPrint <- renderPrint({
    req(spp[[curSp()]]$evalOut)
    spp[[curSp()]]$evalOut
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

model_bioclim_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  verbatimTextOutput(ns("bioclimPrint"))
}

model_bioclim_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    module_knit = species$rmm$code$wallaceSettings$someFlag,
    var1 = species$rmm$code$wallaceSettings$someSetting1,
    var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

