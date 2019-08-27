
pca_controlsUI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("pcaVarSel"), label = "Specify variables to use in analysis?"),
    conditionalPanel(paste0("input['", ns("pcaVarSel"), "']"),
                     uiOutput(ns("pcaSel"))),
    selectInput(ns("pcaPlotSel"), "Plot selection:", choices = list("None selected" = "",
                                                                    "Occurrences only" = "occs",
                                                                    "Occurrences + Background" = "occsBg")),
    uiOutput(ns("pcaControls"))
  )
}

pca_resultsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('pcaResults'))
}

pca_MOD <- function(input, output, session, .curSp) {
  curSp <- .curSp

  output$pcaSel <- renderUI({
    req(curSp())
    sp <- curSp()[1]
    if(is.null(spp[[sp]]$envs)) return()
    sp1.envNames <- names(envs.global[[spp[[sp]]$envs]])
    checkboxGroupInput(session$ns("pcaSel"), label = "Select",
                       choices = sp1.envNames,
                       inline = TRUE, selected = sp1.envNames)
  })

  reactive({
    # ERRORS ####
    if(input$pcaPlotSel == "") {
      shinyLogs %>% writeLog(type = "error", "Please choose a PCA plotting type.")
      return()
    }

    # PROCESSING ####
    sp1 <- curSp()[1]
    sp1.envNames <- names(envs.global[[spp[[sp1]]$envs]])
    if(is.null(input$pcaSel)) pcaSel <- sp1.envNames else pcaSel <- input$pcaSel
    sp1.occsVals <- spp[[sp1]]$occs[pcaSel]
    sp1.bgVals <- spp[[sp1]]$bg[pcaSel]
    if(length(curSp()) > 1) {
      sp2 <- curSp()[2]
      sp2.envNames <- names(envs.global[[spp[[sp2]]$envs]])
      if(all(sp1.envNames == sp2.envNames) == FALSE) {
        shinyLogs %>% writeLog(type = "error", "Species 1 and species 2 must have the same environmental variables.")
        return()
      }
      sp2.occsVals <- spp[[sp2]]$occs[pcaSel]
      sp2.bgVals <- spp[[sp2]]$bg[pcaSel]
    }else{
      sp2 <- NULL
      sp2.occsVals <- NULL
      sp2.bgVals <- NULL
    }

    # FUNCTION CALL ####
    pca <- cESpace_pca(sp1, sp2,
                       sp1.occsVals,
                       sp2.occsVals,
                       sp1.bgVals,
                       sp2.bgVals,
                       shinyLogs)

    req(pca)

    output$pcaControls <- renderUI({
      tagList(
        numericInput(session$ns("pc1"), "X-axis Component", value = 1, min = 1, max = length(pcaSel)),
        numericInput(session$ns("pc2"), "Y-axis Component", value = 2, min = 1, max = length(pcaSel))
      )
    })

    # LOAD INTO SPP ####
    # this name concatenates the species names when there are two,
    # and returns the same name when there is only one species name
    mspName <- paste(curSp(), collapse = ".")
    if(is.null(spp[[mspName]])) {
      spp[[mspName]] <- list(pca = pca)
    }else{
      spp[[mspName]]$pca <- pca
    }

    # METADATA ####
    spp[[mspName]]$rmm$wallaceSettings$pcaSel <- pcaSel

    # PLOTS ####
    output$pcaResults <- renderUI({
      output$pcaScatter <- renderPlot({
        if(input$pcaPlotSel == "occs") {
          x <- pca$scores[pca$scores$bg == 'sp', ]
          x.f <- factor(x$sp)
        }else if(input$pcaPlotSel == "occsBg") {
          x <- pca$scores[pca$scores$sp == 'bg', ]
          x.f <- factor(x$bg)
        }
        ade4::s.class(x, x.f, xax = input$pc1, yax = input$pc2,
                      col = c("red", "blue"), cstar = 0, cpoint = 0.1)
      })
      output$pcaCorCircle <- renderPlot({
        ade4::s.corcircle(pca$co, xax = input$pc1, yax = input$pc2,
                          lab = pcaSel, full = FALSE, box = TRUE)
      })
      output$pcaScree <- renderPlot({
        screeplot(pca)
      })
      output$pcaOut <- renderPrint({
        summary(pca)
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

    return(pca)
  })
}

espace_pca_INFO <- infoGenerator(modName = "Environmental Ordination",
                                 modAuts = "Olivier Broennimann, Jamie Kass",
                                 pkgName = "ade4")

espace_pca_RMD <- function(sp) {
  list(espace.sp1 = strsplit(sp, ".", fixed = TRUE)[[1]][1],
       espace.sp2 = strsplit(sp, ".", fixed = TRUE)[[1]][2],
       pcaSel = printVecAsis(spp[[sp]]$rmm$wallaceSettings$pcaSel))
}
