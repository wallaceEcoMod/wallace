espace_occDens_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("goOccDens"), "Run")
  )
}

espace_occDens_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp

  observeEvent(input$goOccDens, {
    # ERRORS ####
    if (length(curSp()) != 2) {
      logger %>% writeLog(
        type = "error",
        "Please select two species to run the occurrence density grid module."
      )
      return()
    }
    # if no multispecies analysis has been run yet
    mspName <- paste(curSp(), collapse = ".")
    if (is.null(spp[[mspName]])) {
      logger %>% writeLog(
        type = "error",
        "Please run PCA with two species before running the occurrence density grid module."
      )
      return()
    }
    # if a multispecies analysis has been run, but not PCA
    if (is.null(spp[[mspName]]$pca)) {
      logger %>% writeLog(
        type = "error",
        "Please run PCA with two species before running the occurrence density grid module."
      )
      return()
    }

    # FUNCTION CALL ####
    sp1 <- curSp()[1]
    sp2 <- curSp()[2]
    occDens <- espace_occDens(sp1, sp2, spp[[mspName]]$pca, logger)

    # LOAD INTO SPP ####
    req(occDens)
    spp[[mspName]]$occDens <- occDens

    # REFERENCES
    knitcitations::citep(citation("adehabitatHR"))
    knitcitations::citep(citation("ecospat"))

    common$update_component(tab = "Results")
  })

  # PLOTS ####
  output$occDensPlot <- renderPlot({
    graphics::par(mfrow = c(1,2))
    if (length(curSp()) == 2) {
      mSp <- paste(curSp(), collapse = ".")
      sp1 <- curSp()[1]
      sp2 <- curSp()[2]
    } else {
      mSp <- curSp()
    }
    req(spp[[mSp]]$occDens)
    ecospat::ecospat.plot.niche(spp[[mSp]]$occDens[[sp1]], title = spName(sp1))
    ecospat::ecospat.plot.niche(spp[[mSp]]$occDens[[sp2]], title = spName(sp2))
  })
}

espace_occDens_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  plotOutput(ns("occDensPlot"))
}

espace_occDens_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    espace_occDens_knit = !is.null(species$occDens)
  )
}

