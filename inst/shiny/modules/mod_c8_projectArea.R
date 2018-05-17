projectArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    threshPred_UI(ns('threshPred'))
  )
}

projectArea_MOD <- function(input, output, session) {
  reactive({
    
    
    
    
    return(list(pjMsk=projMsk, pjPred=pjPred))
  })
}

projectArea_INFO <- infoGenerator(modName = "Project to New Extent",
                                  modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson", 
                                  pkgName = "dismo")
