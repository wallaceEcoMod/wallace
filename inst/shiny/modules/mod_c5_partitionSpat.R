
partSpat_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput("partSel2", "Options Available:",
                choices = list("None selected" = '',
                               "")),
    
    conditionalPanel("input.partSel == 'sp' & (input.partSel2 == 'cb1' | input.partSel2 == 'cb2')",
                     numericInput("aggFact", label = "Aggregation Factor", value = 2, min = 2))
  )
}

partSpat_MOD <- function(input, output, session, logs, occs) {
  reactive({
    req(occs())
    
    if (!require('rJava')) {
      writeLog('<font color="red"><b>! ERROR</b></font> : Package rJava cannot load. 
               Please download the latest version of Java, and make sure it is the 
               correct version (e.g. 64-bit if you have a 64-bit system). After the
               download, try "library(rJava)". If it loads properly, restart Wallace
               and try again.')
      return()
    }
    
    occs <- values$df[,2:3]
    
    if (partSelect2 == 'block') {
      pt <- 'block'
      group.data <- ENMeval::get.block(occs, values$bg.coords)
      writeLog("> Data partition by block method.")
    }
    if (partSelect2 == 'cb1') {
      pt <- "checkerboard 1"
      group.data <- ENMeval::get.checkerboard1(occs, values$predsMsk, values$bg.coords, aggFact)
      writeLog("> Data partition by checkerboard 1 method.")
    }
    if (partSelect2 == 'cb2') {
      pt <- "checkerboard 2"
      group.data <- ENMeval::get.checkerboard2(occs, values$predsMsk, values$bg.coords, aggFact)
      writeLog("> Data partition by checkerboard 2 method.")
    }
    if (partSelect2 == 'jack') {
      pt <- "jackknife"
      group.data <- ENMeval::get.jackknife(occs, values$bg.coords)
      writeLog("> Data partition by jackknife method.")
    }
    if (partSelect2 == 'random') {
      pt <- paste0("random k-fold (k = ", kfolds, ")")
      group.data <- ENMeval::get.randomkfold(occs, values$bg.coords, kfolds)
      writeLog(paste("> Data partition by", paste0("random k-fold (k = ", kfolds, ")"), ":"))
    }
    
    values$modParams <- list(occ.pts=occs, bg.pts=values$bg.coords, occ.grp=group.data[[1]], bg.grp=group.data[[2]])
    #newColors <- brewer.pal(max(group.data[[1]]), 'Accent')
    #     values$df$parts <- factor(group.data[[1]])
    #     newColors <- colorFactor(rainbow(max(group.data[[1]])), values$df$parts)
    #     fillColor = ~newColors(parts)
    newColors <- gsub("FF$", "", rainbow(max(group.data[[1]])))  # colors for partition symbology
    values$partFill <- newColors[group.data[[1]]]
    #newColors <- sample(colors(), max(group.data[[1]]))
    proxy %>% 
      clearMarkers() %>% 
      map_plotLocs(values$df, fillColor = values$partFill, fillOpacity = 1)
  })
}
