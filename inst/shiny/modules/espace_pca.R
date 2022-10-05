espace_pca_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("pcaSel")),
    selectInput(ns("pcaPlotSel"), "Plot selection:",
                choices = list("None selected" = "",
                               "Occurrences only" = "occs",
                               "Occurrences + Background" = "occsBg")),
    uiOutput(ns("pcaControls")),
    actionButton(ns("goPCA"), "Run")
  )
}

espace_pca_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  envs.global <- common$envs.global
  envs <- common$envs


  output$pcaSel <- renderUI({
    ns <- session$ns
    req(curSp())
    if (length(curSp()) == 1) {
      shiny::tagList(
        shiny::em("Select two species in species menu"),
        br()
      )
    } else if (length(curSp()) == 2) {
      sp1 <- curSp()[1]
      sp2 <- curSp()[2]
      if (is.null(spp[[sp1]]$envs)) return()
      if (is.null(spp[[sp2]]$envs)) return()
      sp1.envNames <- names(envs.global[[spp[[sp1]]$envs]])
      sp2.envNames <- names(envs.global[[spp[[sp2]]$envs]])
      shared_Names <- intersect(sp1.envNames, sp2.envNames)
      shiny::tagList(
        shinyWidgets::pickerInput(
          ns("pcaSel"),
          label = "Select variables available for both species",
          choices = setNames(as.list(shared_Names), shared_Names),
          multiple = TRUE,
          selected = shared_Names,
          options = list(`actions-box` = TRUE))
      )
    }
  })

  observeEvent(input$goPCA, {
    # ERRORS ####
    if (length(curSp()) != 2) {
      logger %>% writeLog(
        type = "error",
        "Please select two species to run the PCA module."
      )
      return()
    }
    for(sp in curSp()) {
      if (is.null(spp[[sp]]$procEnvs$bgMask)) {
        logger %>% writeLog(
          type = 'error', hlSpp(sp),
          "Before partitioning occurrences, mask your ",
          "environmental variables by your background extent.")
        return()
      }
    }
    # ERRORS ####
    if(input$pcaPlotSel == "") {
      logger %>% writeLog(type = "error", "Please choose a PCA plotting type.")
      return()
    }

    # PROCESSING ####
    sp1 <- curSp()[1]
    sp1.envNames <- names(envs.global[[spp[[sp1]]$envs]])
    sp2 <- curSp()[2]
    sp2.envNames <- names(envs.global[[spp[[sp2]]$envs]])
    pcaSel <- input$pcaSel
    if (is.null(pcaSel)) {
      logger %>% writeLog(
        type = "error", hlSpp(paste0(curSp()[1], " and ", curSp()[2])),
        " must have the same environmental variables."
      )
      return()
    }
    sp1.occsVals <- spp[[sp1]]$occs[pcaSel]
    sp1.bgVals <- spp[[sp1]]$bg[pcaSel]
    sp2.occsVals <- spp[[sp2]]$occs[pcaSel]
    sp2.bgVals <- spp[[sp2]]$bg[pcaSel]

    # FUNCTION CALL ####
    pca <- espace_pca(sp1, sp2,
                      sp1.occsVals,
                      sp2.occsVals,
                      sp1.bgVals,
                      sp2.bgVals,
                      logger)

    req(pca)

    # LOAD INTO SPP ####
    # this name concatenates the species names when there are two,
    # and returns the same name when there is only one species name
    mspName <- paste(curSp(), collapse = ".")
    if (is.null(spp[[mspName]])) {
      spp[[mspName]] <- list(pca = pca)
    } else {
      spp[[mspName]]$pca <- pca
    }

     spp[[mspName]]$pcaSel <- pcaSel
     spp[[mspName]]$pcaPlotSel <- input$pcaPlotSel
     ###Save inputs for PCA
     spp[[mspName]]$pc1 <- input$pc1
     spp[[mspName]]$pc2 <- input$pc2
    common$update_component(tab = "Results")

    # REFERENCES
    knitcitations::citep(citation("ade4"))
  })

  output$pcaControls <- renderUI({
    tagList(
      numericInput(session$ns("pc1"), "X-axis Component",
                   value = 1, min = 1, max = length(input$pcaSel)),
      numericInput(session$ns("pc2"), "Y-axis Component",
                   value = 2, min = 1, max = length(input$pcaSel))
    )
  })

  # PLOTS ####
  output$pcaResults <- renderUI({
    output$pcaScatter <- renderPlot({
      if (length(curSp()) == 1) {
        mSp <- curSp()
      } else if (length(curSp()) == 2) {
        mSp <- paste(curSp(), collapse = ".")
      }
      req(spp[[mSp]]$pca)
      if (input$pcaPlotSel == "occs") {
        x <- spp[[mSp]]$pca$scores[spp[[mSp]]$pca$scores$bg == 'sp', ]
        x.f <- factor(x$sp)
      } else if (input$pcaPlotSel == "occsBg") {
        x <- spp[[mSp]]$pca$scores[spp[[mSp]]$pca$scores$sp == 'bg', ]
        x.f <- factor(x$bg)
      }
      ade4::s.class(x, x.f, xax = input$pc1, yax = input$pc2,
                    col = c("red", "blue"), cstar = 0, cpoint = 0.1, sub = "",
                    possub = "topright")
      title(xlab = paste0("PC", input$pc1), ylab = paste0("PC", input$pc2))
    })
    output$pcaCorCircle <- renderPlot({
      if (length(curSp()) == 1) {
        mSp <- curSp()
      } else if (length(curSp()) == 2) {
        mSp <- paste(curSp(), collapse = ".")
      }
      req(spp[[mSp]]$pca)
      ade4::s.corcircle(spp[[mSp]]$pca$co, xax = input$pc1, yax = input$pc2,
                        lab = input$pcaSel, full = FALSE, box = TRUE)
      title(xlab = paste0("PC", input$pc1), ylab = paste0("PC", input$pc2))
    })
    output$pcaScree <- renderPlot({
      if (length(curSp()) == 1) {
        mSp <- curSp()
      } else if (length(curSp()) == 2) {
        mSp <- paste(curSp(), collapse = ".")
      }
      req(spp[[mSp]]$pca)
      stats::screeplot(spp[[mSp]]$pca, main = NULL)
    })
    output$pcaOut <- renderPrint({
      if (length(curSp()) == 1) {
        mSp <- curSp()
      } else if (length(curSp()) == 2) {
        mSp <- paste(curSp(), collapse = ".")
      }
      req(spp[[mSp]]$pca)
      k <- round(100 * spp[[mSp]]$pca$eig / sum(spp[[mSp]]$pca$eig), 2)
      names(k) <- paste0("PC", 1:length(spp[[mSp]]$pca$eig), "(%)")
      j <- spp[[mSp]]$pca$c1
      names(j) <- paste0("PC", 1:length(spp[[mSp]]$pca$eig))
      cat(c("Variance explained:",
            capture.output(k), "",
            "Loadings:",
            capture.output(j), "",
            capture.output(summary(spp[[mSp]]$pca))),
            sep = "\n")
    })
    tabsetPanel(
      tabPanel("PCA scatter plot",
               tagList(
                 plotOutput(session$ns('pcaScatter'))
               )),
      tabPanel("PCA correlation circle",
               tagList(
                 plotOutput(session$ns('pcaCorCircle'))
               )),
      tabPanel("PCA screeplot",
               tagList(
                 plotOutput(session$ns('pcaScree'))
               )),
      tabPanel("PCA results summary",
               tagList(
                verbatimTextOutput(session$ns("pcaOut"))
               ))
    )
  })

  return(list(
    save = function() {
      list(
        pcaSel = input$pcaSel,
        pcaPlotSel = input$pcaPlotSel
      )
    },
    load = function(state) {
      shinyWidgets::updatePickerInput(session, "pcaSel", selected = state$pcaSel)
      updateSelectInput(session, "pcaPlotSel", selected = state$pcaPlotSel)
    }
  ))
  updateSelectInput(session, "curSp", selected = curSp())
}

espace_pca_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  uiOutput(ns('pcaResults'))
}

espace_pca_module_rmd <- function(species) {
  list(
    espace_pca_knit = !is.null(species$pca),
    pcaSel_rmd = printVecAsis(species$pcaSel),
    pcaPlotSel_rmd = species$pcaPlotSel,
    pc1_rmd = species$pc1,
    pc2_rmd = species$pc2
  )
}
