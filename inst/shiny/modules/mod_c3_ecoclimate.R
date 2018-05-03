
ecoClimate_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Select AOGCM',
                   selectInput(ns("bcAOGCM"), label = "Select the Atmospheric Oceanic General Circulation Model you want to use",
                               choices = list("Select AOGCMs" = "",
                                              "CCSM" = "CCSM",
                                              "CNRM"= "CNRM", 
                                              "MIROC"="MIROC", 
                                              "FGOALS"="FGOALS", 
                                              "GISS"="GISS", 
                                              "IPSL"="IPSL",
                                              "MRI"= "MRI", 
                                              "MPI"= "MPI"
                                              ))),
 
    checkboxInput(ns("bcSelChoice"), label = "Specify variables to use in analysis?"),
    conditionalPanel(paste0("input['", ns("bcSelChoice"), "']"),
                     checkboxGroupInput(ns("bcSels"), label = "Select",
                                        choices = setNames(as.list(paste0('bio', 1:19)), paste0('bio', 1:19)), 
                                        inline=TRUE, selected = paste0('bio', 1:19)))
    
  )
}

ecoClimate_MOD <- function(input, output, session, logs) {
  reactive({
    
    if (is.null(vals$occs)) {
      logs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                        obtain occurrence data in component 1.")
      return()
    }
    
    # record for RMD
    ## what is bcSels. people can tun this? or is it already pre-defined?
    rvs$bcSels <- input$bcSels
    
    c3_ecoClimate(input$bcAOGCM, vals$timeInterval, input$bcSelChoice, input$bcSels, logs)
  })
}

ecoclimate_INFO <- infoGenerator(modName = "ecoClimate",
                                 modAuts = "Jamie M. Kass, Robert P. Anderson",
                                 pkgName = "raster")