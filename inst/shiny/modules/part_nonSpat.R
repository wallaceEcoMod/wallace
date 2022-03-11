part_nonSpat_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("partNspSel"), "Options Available:",
                choices = list("None selected" = '',
                               "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand")),
    conditionalPanel(sprintf("input['%s'] == 'rand'", ns("partNspSel")),
                     numericInput(ns("kfolds"), label = "Number of Folds",
                                  value = 2, min = 2)),
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
    actionButton(ns("goPartitionNonSpat"), "Partition")
  )
}

part_nonSpat_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  bgMask <- common$bgMask

  observeEvent(input$goPartitionNonSpat, {
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    # PROCESSING ####
    for(sp in spLoop) {
      if (is.null(bgMask())) {
        logger %>% writeLog(
          type = 'error', hlSpp(sp),
          "Before partitioning occurrences, mask your ",
          "environmental variables by your background extent."
        )
        return()
      }

      #### FUNCTION CALL
      group.data <- part_partitionOccs(spp[[sp]]$occs, spp[[sp]]$bg,
                                       input$partNspSel, kfolds = input$kfolds,
                                       bgMask = NULL, aggFact = NULL, logger,
                                       spN = sp)
      req(group.data)

      # LOAD INTO SPP ####
      spp[[sp]]$occs$partition <- group.data$occs.grp
      spp[[sp]]$bg$partition <- group.data$bg.grp

      # REFERENCES
      knitcitations::citep(citation("ENMeval", auto = TRUE))

      # METADATA ####
      spp[[sp]]$rmm$code$wallace$partition_code <- input$partNspSel
      spp[[sp]]$rmm$model$partition$NonSpatial <- 'Non-spatial'
      if(input$partNspSel == 'jack') {
        spp[[sp]]$rmm$model$partition$numberFolds <- nrow(spp[[sp]]$occs)
        spp[[sp]]$rmm$model$partition$partitionRule <- 'jackknife'
      }
      if(input$partNspSel == 'rand') {
        spp[[sp]]$rmm$model$partition$numberFolds <- input$kfolds
        spp[[sp]]$rmm$model$partition$partitionRule <- 'random k-fold'
      }
    }
    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      list(
        partNspSel = input$partNspSel,
        kfolds = input$kfolds
      )
    },
    load = function(state) {
      updateSelectInput(session, "partNspSel", selected = state$partNspSel)
      updateNumericInput(session, "kfolds", value = state$kfolds)
    }
  ))

}

part_nonSpat_module_map <- function(map, common) {
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

part_nonSpat_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    part_nonSpat_knit =  !is.null(species$rmm$model$partition$NonSpatial),
    k_folds_rmd = species$rmm$model$partition$numberFolds,
    method_rmd = species$rmm$model$partition$partitionRule,
    method_code_rmd = species$rmm$code$wallace$partition_code
  )
}

