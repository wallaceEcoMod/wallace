sppSelUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns('sppSel'), label = "Current species", choices = NULL,
                   multiple = TRUE, selected = NULL)
  )
}

sppSel <- function(input, output, session, forceSel=NULL) {
  reactive({
    # check that a species is in the list already -- if not, don't proceed
    req(length(reactiveValuesToList(spp)) > 0)
    # get the species names
    n <- names(spp)
    # make a named list of their names
    sppNameList <- setNames(as.list(n), n)
    # if no current species selected, select the first name
    if(!is.null(input$sppSel)) {
      selected <- input$sppSel
      } else if(is.null(forceSel)) { 
        selected <- n[1]
      } else {
        selected <- forceSel
      }
    # if espace component, allow for multiple species selection
    if(tabs() == 'espace') options <- list(maxItems = 2) else options <- list(maxItems = 1)
    print(selected)
    print(options)
    updateSelectizeInput(session, "sppSel", choices = sppNameList, selected = selected, options = options)  
    return(selected)
  })
}
