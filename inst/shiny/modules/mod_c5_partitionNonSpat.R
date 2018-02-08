
partNsp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partNspSel"), "Options Available:",
                choices = list("None selected" = '', "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand")),
    numericInput(ns("kfolds"), label = "Number of Folds", value = 2, min = 2)
  )
}

partNsp_MOD <- function(input, output, session) {
  reactive({
    
    #### FUNCTION CALL
    sp <- spp[[curSp()]]
    partNonSpat <- c5_partitionNonSpat(sp$occs, sp$bgPts, input$partNspSel, input$kfolds, logs, shiny = TRUE)
    
    if (is.null(partNonSpat)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$Parts$occ.grp <- partNonSpat$occ.grp
    spp[[curSp()]]$Parts$bg.grp <- partNonSpat$bg.grp
    
    # RMD VALUES ####
    spp[[curSp()]]$rmd$c5 <- list(partNspSel = input$partNspSel, kfolds = input$kfolds)
    
  })
}
