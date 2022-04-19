model_bioclim_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
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
          type = 'error', hlSpp(sp),
          "Before building a model, please partition occurrences for cross-validation.")
        return()
      }

      user_grp <- list(occs.grp = spp[[sp]]$occs$partition,
                       bg.grp = spp[[sp]]$bg$partition)

      # FUNCTION CALL ####
      m.bioclim <- model_bioclim(occs = spp[[sp]]$occs,
                                 bg = spp[[sp]]$bg,
                                 user.grp = user_grp,
                                 bgMsk = spp[[sp]]$procEnvs$bgMask,
                                 logger,
                                 spN = sp)

      req(m.bioclim)

      # LOAD INTO SPP ####
      spp[[sp]]$evalOut <- m.bioclim

      # REFERENCES
      knitcitations::citep(citation("dismo"))
      knitcitations::citep(citation("ENMeval", auto = TRUE))

      # METADATA ####
      spp[[sp]]$rmm$model$algorithms <- "BIOCLIM"
      spp[[sp]]$rmm$model$algorithm$bioclim$notes <- "ENMeval/dismo package implementation"
    }
    common$update_component(tab = "Results")
  })

  output$evalTblsBioclim <- renderUI({
    req(spp[[curSp()]]$rmm$model$algorithms)
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      req(spp[[curSp()]]$evalOut)
      res <- spp[[curSp()]]$evalOut@results
      res.grp <- spp[[curSp()]]$evalOut@results.partitions
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
}

model_bioclim_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  uiOutput(ns('evalTblsBioclim'))
}

model_bioclim_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    model_bioclim_knit = if (!is.null(species$rmm$model$algorithms)) {
      species$rmm$model$algorithms == "BIOCLIM"} else {FALSE}
  )
}

