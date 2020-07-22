model_bioclim_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(
      title = "Add Batch guidance text here (**)",
      checkboxInput(ns("batch"), label = strong("Batch"), value = TRUE) # Check default (value = FALSE)
    ),
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
          "cross-validation for ", em(spName(sp)), ".")
        return()
      }

      # FUNCTION CALL ####
      m.bioclim <- model_bioclim(occs = spp[[sp]]$occs,
                                 bg = spp[[sp]]$bg,
                                 occsGrp = spp[[sp]]$occs$partition,
                                 bgGrp = spp[[sp]]$bg$partition,
                                 bgMsk = spp[[sp]]$procEnvs$bgMask,
                                 logger,
                                 spN = sp)

      req(m.bioclim)

      # LOAD INTO SPP ####
      spp[[sp]]$evalOut <- m.bioclim

      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- "BIOCLIM"
      spp[[sp]]$rmm$model$bioclim$notes <- "ENMeval/dismo package implementation"
    }
    common$update_component(tab = "Results")
  })

  output$evalTblsBioclim <- renderUI({
    req(spp[[curSp()]]$rmm$model$algorithm)
    if (spp[[curSp()]]$rmm$model$algorithm == "BIOCLIM") {
      req(spp[[curSp()]]$evalOut)
      res <- spp[[curSp()]]$evalOut@results
      res.grp <- spp[[curSp()]]$evalOut@results.grp
      res.round <- cbind(round(res[, 1:11], digits = 3))
      res.grp.round <- round(res.grp[, 2:6], digits = 3)
      # define contents for both evaluation tables
      options <- list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">')
      output$evalTbl <- DT::renderDataTable(res.round, options = options)
      output$evalTblBins <- DT::renderDataTable(res.grp.round, options = options)

      tagList(br(),
              span("Evaluation statistics: full model and partition averages",
                   class = "stepText"), br(), br(),
              DT::dataTableOutput(session$ns('evalTbl')), br(),
              span("Evaluation statistics: individual partitions",
                   class = "stepText"), br(), br(),
              DT::dataTableOutput(session$ns('evalTblBins')))
    }
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
  uiOutput(ns('evalTblsBioclim'))
}

model_bioclim_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    model_bioclim_knit = FALSE
    # model_bioclim_knit = species$rmm$code$wallaceSettings$someFlag,
    # var1 = species$rmm$code$wallaceSettings$someSetting1,
    # var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

