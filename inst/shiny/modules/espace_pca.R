espace_pca_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    checkboxInput(ns("pcaVarSel"),
                  label = "Specify variables to use in analysis?"),
    conditionalPanel(paste0("input['", ns("pcaVarSel"), "']"),
                     uiOutput(ns("pcaSel"))),
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
    sp <- curSp()[1]
    if(is.null(spp[[sp]]$envs)) return()
    sp1.envNames <- names(envs.global[[spp[[sp]]$envs]])
    checkboxGroupInput(ns("pcaSel"), label = "Select",
                       choices = sp1.envNames,
                       inline = TRUE, selected = sp1.envNames)
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
    # ERRORS ####
    if(input$pcaPlotSel == "") {
      logger %>% writeLog(type = "error", "Please choose a PCA plotting type.")
      return()
    }

    # PROCESSING ####
    sp1 <- curSp()[1]
    sp1.envNames <- names(envs.global[[spp[[sp1]]$envs]])
    if (is.null(input$pcaSel)) pcaSel <- sp1.envNames else pcaSel <- input$pcaSel
    sp1.occsVals <- spp[[sp1]]$occs[pcaSel]
    sp1.bgVals <- spp[[sp1]]$bg[pcaSel]
    sp2 <- curSp()[2]
    sp2.envNames <- names(envs.global[[spp[[sp2]]$envs]])
    if (all(sp1.envNames == sp2.envNames) == FALSE) {
      logger %>% writeLog(
        type = "error", hlSpp(curSp()[1], " and ", curSp()[2]),
        " must have the same environmental variables."
      )
      return()
    }
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

    # spp[[mspName]]$rmm$wallace$pcaSel <- pcaSel
    common$update_component(tab = "Results")
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
                    col = c("red", "blue"), cstar = 0, cpoint = 0.1)
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
    })
    output$pcaScree <- renderPlot({
      if (length(curSp()) == 1) {
        mSp <- curSp()
      } else if (length(curSp()) == 2) {
        mSp <- paste(curSp(), collapse = ".")
      }
      req(spp[[mSp]]$pca)
      screeplot(spp[[mSp]]$pca, main = NULL)
    })
    output$pcaOut <- renderPrint({
      if (length(curSp()) == 1) {
        mSp <- curSp()
      } else if (length(curSp()) == 2) {
        mSp <- paste(curSp(), collapse = ".")
      }
      req(spp[[mSp]]$pca)
      summary(spp[[mSp]]$pca)
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
        pcaVarSel = input$pcaVarSel,
        pcaPlotSel = input$pcaPlotSel
      )
    },
    load = function(state) {
      updateCheckboxInput(session, "pcaVarSel", value = state$pcaVarSel)
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
  # Variables used in the module's Rmd code
  # list(espace.sp1 = strsplit(sp, ".", fixed = TRUE)[[1]][1],
  #      espace.sp2 = strsplit(sp, ".", fixed = TRUE)[[1]][2],
  #      pcaSel = printVecAsis(spp[[sp]]$rmm$wallace$pcaSel))
  list(
    espace_pca_knit = !is.null(species$pca)
  )
}
