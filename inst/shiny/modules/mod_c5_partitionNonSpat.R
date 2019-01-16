
partitionNonSpat_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partNspSel"), "Options Available:",
                choices = list("None selected" = '', "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand")),
    conditionalPanel(sprintf("input['%s'] == 'rand'", 
                             ns("partNspSel")),
                     numericInput(ns("kfolds"), label = "Number of Folds", 
                                  value = 2, min = 2)),
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
  )
}

partitionNonSpat_MOD <- function(input, output, session) {
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
      
      #### FUNCTION CALL
      group.data <- c5_partitionOccs(spp[[sp]]$occs, spp[[sp]]$bg, input$partNspSel, 
                                     kfolds = input$kfolds, bgMask = NULL, aggFact = NULL, shinyLogs)
      req(group.data)
      
      # LOAD INTO SPP ####
      spp[[sp]]$occs$partition <- group.data$occ.grp
      spp[[sp]]$bg$partition <- group.data$bg.grp
      
      # METADATA ####
      if(input$partNspSel == 'jack') {
        spp[[sp]]$rmm$model$partition$numberFolds <- nrow(spp[[sp]]$occs)
        spp[[sp]]$rmm$model$partition$partitionRule <- 'jackknife'
      }
      if(input$partNspSel == 'rand') {
        spp[[sp]]$rmm$model$partition$numberFolds <- input$kfolds
        spp[[sp]]$rmm$model$partition$partitionRule <- 'random k-fold'
      }
    }
  })
}

partitionNonSpat_MAP <- function(map, session) {
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

partitionNonSpat_INFO <- infoGenerator(modName = "Non-spatial Partition",
                                       modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                                       pkgName = "ENMeval")