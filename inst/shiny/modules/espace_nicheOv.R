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

    # METADATA ####
  })

  output$nicheOvText <- renderUI({
    HTML(
      paste(
        "Overlap D = ", round(nicheOv$overlap$D, 2),
        "\n",
        "Sp1 only :", round(nicheOv$USE[3], 2),
        " | Sp2 only :", round(nicheOv$USE[1], 2),
        " | Both :", round(nicheOv$USE[2], 2)
      )
    )
  })

  output$nicheOvPlot <- renderPlot({
    #plots
    layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3), 4, 3, byrow = F))
    #layout.show(3)

    ecospat::ecospat.plot.niche.dyn(
      z1,
      z2,
      0.5,
      title = mspName,
      colz1 = "blue",
      colz2 = "red",
      colinter = "purple",
      colZ1 = "blue",
      colZ2 = "red"
    )
    if (!is.null(nicheOv$equiv))
      ecospat::ecospat.plot.overlap.test(nicheOv$equiv, "D", "Equivalency test")
    if (!is.null(nicheOv$simil))
      ecospat::ecospat.plot.overlap.test(nicheOv$simil, "D", "Similarity test")
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
    module_knit = species$rmm$code$wallaceSettings$someFlag,
    var1 = species$rmm$code$wallaceSettings$someSetting1,
    var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

