espace_nicheOv_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("goNicheOv"), "Run")
  )
}

espace_nicheOv_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp

  observeEvent(input$goNicheOv, {
    if (length(curSp()) != 2) {
      logger %>% writeLog(
        type = "error",
        "Please select two species to run the niche overlap module."
      )
      return()
    }
    mspName <- paste(curSp(), collapse = ".")
    if (is.null(spp[[mspName]])) {
      logger %>% writeLog(
        type = "error",
        paste0("Please run PCA and occurrence density with two species before",
               " running the niche overlap module.")
      )
      return()
    }
    # if a multispecies analysis has been run, but not occDens
    if (is.null(spp[[mspName]]$occDens)) {
      logger %>% writeLog(
        type = "error",
        paste0("Please run occurrence density with two species before running",
               " the niche overlap module.")
      )
      return()
    }

    # FUNCTION CALL ####
    sp1 <- curSp()[1]
    sp2 <- curSp()[2]
    z1 <- spp[[mspName]]$occDens[[sp1]]
    z2 <- spp[[mspName]]$occDens[[sp2]]
    nicheOv <- espace_nicheOv(z1, z2, logger = logger)
    if (is.null(nicheOv)) return()

    # LOAD INTO SPP ####
    spp[[mspName]]$nicheOv <- nicheOv

    # REFERENCES
    knitcitations::citep(citation("ecospat"))

    common$update_component(tab = "Results")
  })

  output$nicheOvText <- renderUI({
    if (length(curSp()) == 2) {
      mSp <- paste(curSp(), collapse = ".")
      sp1 <- curSp()[1]
      sp2 <- curSp()[2]
    } else {
      mSp <- curSp()
    }
    req(spp[[mSp]]$nicheOv)
    HTML(
      paste(
        "Overlap D = ", round(spp[[mSp]]$nicheOv$overlap$D, 2),
        " | Sp1 only :", round(spp[[mSp]]$nicheOv$USE[3], 2),
        " | Sp2 only :", round(spp[[mSp]]$nicheOv$USE[1], 2),
        " | Both :", round(spp[[mSp]]$nicheOv$USE[2], 2)
      )
    )
  })

  output$nicheOvPlot <- renderPlot({
    if (length(curSp()) == 2) {
      mSp <- paste(curSp(), collapse = ".")
      sp1 <- curSp()[1]
      sp2 <- curSp()[2]
    } else {
      mSp <- curSp()
    }
    req(spp[[mSp]]$nicheOv)
    graphics::par(mfrow = c(1, 2))

    ecospat::ecospat.plot.niche.dyn(
      spp[[mSp]]$occDens[[sp1]],
      spp[[mSp]]$occDens[[sp2]],
      0.5,
      title = mSp,
      col.unf = "blue",
      col.exp = "red",
      col.stab = "purple",
      colZ1 = "blue",
      colZ2 = "red",
      transparency = 25
    )
    box()
    # if (!is.null(spp[[mSp]]$nicheOv$equiv))
    #   ecospat::ecospat.plot.overlap.test(spp[[mSp]]$nicheOv$equiv,
    #                                      "D", "Equivalency test")
    if (!is.null(spp[[mSp]]$nicheOv$simil))
      ecospat::ecospat.plot.overlap.test(spp[[mSp]]$nicheOv$simil,
                                         "D", "Similarity test")
    graphics::par(mfrow = c(1, 1))
  })
}

espace_nicheOv_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  tagList(
    htmlOutput(ns("nicheOvText")), br(), br(),
    plotOutput(ns("nicheOvPlot"))
  )
}

espace_nicheOv_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    espace_nicheOv_knit = !is.null(species$nicheOv),
    simil_rmd = !is.null(species$nicheOv$simil),
    equiv_rmd = !is.null(species$nicheOv$equiv)
  )
}

