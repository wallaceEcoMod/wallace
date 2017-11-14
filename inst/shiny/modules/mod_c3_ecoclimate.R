
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
    tags$div(title='Select Temporal Scenario',
             selectInput(ns("bcScenario"), label = "Select the temporal scenario",
                         choices = list("Select AOGCMs" = "",
                                        "Last Glacial Maximum" = "LGM",
                                        "Holocene"="Holo", 
                                        "Present"="Present", 
                                        "Future RCP 2.6"="Future 2.6", 
                                        "Future RCP 4.5"="Future 4.5", 
                                        "Future RCP 6"="Future 6", 
                                        "Future RCP 8.5"="Future 8.5"
                         ))),
    checkboxInput(ns("bcSelChoice"), label = "Specify variables to use in analysis?"),
    conditionalPanel(paste0("input['", ns("bcSelChoice"), "']"),
                     checkboxGroupInput(ns("bcSels"), label = "Select",
                                        choices = setNames(as.list(paste0('bio', 1:19)), paste0('bio', 1:19)), 
                                        inline=TRUE, selected = paste0('bio', 1:19)))
    
  )
}

ecoClimate_MOD <- function(input, output, session, logs, mapCntr, envs) {
  reactive({
    c3_ecoClimate(input$bcAOGCM, input$bcScenario, input$bcSelChoice, input$bcSels, rvs)
  })
}