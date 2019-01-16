
partitionSpat_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partitionSpatSel"), "Options Available:",
                choices = list("None selected" = '',
                               "Block (k = 4)" = "block",
                               "Checkerboard 1 (k = 2)" = "cb1",
                               "Checkerboard 2 (k = 4)" = "cb2")),
    conditionalPanel(sprintf("input['%1$s'] == 'cb1' | input['%1$s'] == 'cb2'", 
                             ns("partitionSpatSel")),
                     numericInput(ns("aggFact"), label = "Aggregation Factor", 
                                  value = 2, min = 2)),
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
  )
}

partitionSpat_MOD <- function(input, output, session) {
  reactive({
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    # PROCESSING ####
    for(sp in spLoop) {
      if (is.null(bgMask())) {
        shinyLogs %>% writeLog(type = 'error', "Before partitioning occurrences for ", sp,
                               ", mask your environmental variables by your background extent.")
        return()
      }
      
      # FUNCTION CALL ####
      group.data <- c5_partitionOccs(spp[[sp]]$occs, spp[[sp]]$bg, input$partitionSpatSel, 
                                     kfolds = NULL, bgMask = spp[[sp]]$procEnvs$bgMask, 
                                     aggFact = input$aggFact, shinyLogs)
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
        spp[[sp]]$rmm$model$partition$notes <- paste('aggregation factor =', input$aggFact)
      }
    }
    
  })
}

partitionSpat_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
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

partitionSpat_INFO <- infoGenerator(modName = "Spatial Partition",
                                    modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                                    pkgName = "ENMeval")