part_spat_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("partitionSpatSel"), "Options Available:",
                choices = list("None selected" = '',
                               "Block (k = 4)" = "block",
                               "Checkerboard 1 (k = 2)" = "cb1",
                               "Checkerboard 2 (k = 4)" = "cb2")), # Check default (no selected)
    conditionalPanel(sprintf("input['%1$s'] == 'cb1' | input['%1$s'] == 'cb2'",
                             ns("partitionSpatSel")),
                     numericInput(ns("aggFact"), label = "Aggregation Factor",
                                  value = 2, min = 2)),
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
    actionButton(ns("goPartitionSpat"), "Partition")
  )
}

part_spat_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  bgMask <- common$bgMask

  observeEvent(input$goPartitionSpat, {
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    # PROCESSING ####
    for(sp in spLoop) {
      if (is.null(bgMask())) {
        logger %>% writeLog(
          type = 'error', hlSpp(sp),
          "Before partitioning occurrences, mask your ",
          "environmental variables by your background extent.")
        return()
      }

      # FUNCTION CALL ####
      group.data <- part_partitionOccs(spp[[sp]]$occs, spp[[sp]]$bg,
                                       input$partitionSpatSel, kfolds = NULL,
                                       bgMask = spp[[sp]]$procEnvs$bgMask,
                                       aggFact = input$aggFact, logger,
                                       spN = sp)
      req(group.data)

      # LOAD INTO SPP ####
      spp[[sp]]$occs$partition <- group.data$occs.grp
      spp[[sp]]$bg$partition <- group.data$bg.grp
      spp[[sp]]$rmm$code$wallace$partition_code <- input$partitionSpatSel
      spp[[sp]]$rmm$code$wallace$partition_agg <- input$aggFact

      # REFERENCES
      knitcitations::citep(citation("ENMeval", auto = TRUE))

      # METADATA ####
      spp[[sp]]$rmm$model$partition$Spatial <- 'Spatial'
      if(input$partitionSpatSel == 'block') {
        spp[[sp]]$rmm$model$partition$numberFolds <- 4
        spp[[sp]]$rmm$model$partition$partitionRule <- 'spatial block'
      }
      if(input$partitionSpatSel == 'cb1') {
        spp[[sp]]$rmm$model$partition$numberFolds <- 2
        spp[[sp]]$rmm$model$partition$partitionRule <- 'checkerboard'
      }
      if(input$partitionSpatSel == 'cb2') {
        spp[[sp]]$rmm$model$partition$numberFolds <- 4
        spp[[sp]]$rmm$model$partition$partitionRule <- 'hierarchical checkerboard'
        spp[[sp]]$rmm$model$partition$notes <- paste('aggregation factor =',
                                                     input$aggFact)
      }
    }
    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      list(
        partitionSpatSel = input$partitionSpatSel,
        aggFact = input$aggFact
      )
    },
    load = function(state) {
      updateSelectInput(session, "partitionSpatSel", selected = state$partitionSpatSel)
      updateNumericInput(session, "aggFact", value = state$aggFact)
    }
  ))

}

part_spat_module_map <- function(map, common) {
  occs <- common$occs
  # Map logic
  if (!is.null(occs()$partition)) {
    occsGrp <- occs()$partition
    # colors for partition symbology
    if (max(occsGrp) < 3) {
      newColors <- RColorBrewer::brewer.pal(n = 3, "Set2")[1:max(occsGrp)]
    } else if (max(occsGrp) < 9) {
      newColors <- RColorBrewer::brewer.pal(n = max(occsGrp), "Set2")
    } else if (max(occsGrp) < 12) {
      newColors <- RColorBrewer::brewer.pal(n = max(occsGrp), "RdYlBu")
    } else {
      newColors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, "RdYlBu"))(max(occsGrp))
    }
    partsFill <- newColors[occsGrp]
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE,
                       fillColor = partsFill, fillOpacity = 1, weight = 2,
                       popup = ~pop) %>%
      addLegend("bottomright", colors = newColors,
                title = "Partition Groups", labels = sort(unique(occsGrp)),
                opacity = 1)
  } else {
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs())
  }
}

part_spat_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    part_spat_knit = !is.null(species$rmm$model$partition$Spatial)&is.null(species$rmm$code$wallace$partition_agg),
    method_rmd = species$rmm$model$partition$partitionRule,
    method_code_rmd = species$rmm$code$wallace$partition_code,
    part_spat_aggreg_knit = !is.null(species$rmm$code$wallace$partition_agg),
    aggFact_rmd = species$rmm$code$wallace$partition_agg
  )
}

