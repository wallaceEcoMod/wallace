part_spat_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("partitionSpatSel"), "Options Available:",
                choices = list("None selected" = '',
                               "Block (k = 4)" = "block",
                               "Checkerboard 1 (k = 2)" = "cb1",
                               "Checkerboard 2 (k = 4)" = "cb2"),
                selected = 'block'), # Check default (no selected)
    conditionalPanel(sprintf("input['%1$s'] == 'cb1' | input['%1$s'] == 'cb2'",
                             ns("partitionSpatSel")),
                     numericInput(ns("aggFact"), label = "Aggregation Factor",
                                  value = 2, min = 2)),
    checkboxInput(ns("batch"), label = strong("Batch"), value = T), # Check default (value = FALSE)
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
          type = 'error',
          "Before partitioning occurrences for ", em(spName(sp)), ", mask your ",
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
      spp[[sp]]$occs$partition <- group.data$occ.grp
      spp[[sp]]$bg$partition <- group.data$bg.grp

      # METADATA ####
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
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))

}

part_spat_module_map <- function(map, common) {
  occs <- common$occs
  # Map logic
  req(occs()$partition)
  occsGrp <- occs()$partition
  # colors for partition symbology
  newColors <- gsub("FF$", "", rainbow(max(occsGrp)))
  partsFill <- newColors[occsGrp]
  map %>% clearAll() %>%
    map_occs(occs(), fillColor = partsFill, fillOpacity = 1) %>%
    addLegend("bottomright", colors = newColors,
              title = "Partition Groups", labels = sort(unique(occsGrp)),
              opacity = 1)
}

part_spat_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    part_spat_knit = FALSE
    # part_spat_knit = species$rmm$code$wallaceSettings$someFlag,
    # var1 = species$rmm$code$wallaceSettings$someSetting1,
    # var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

