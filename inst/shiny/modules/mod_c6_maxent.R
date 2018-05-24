
maxent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    strong("Select feature classes "), strong(em("(flexibility of modeled response)")), br(),
    "key: ", strong("L"), "inear, ", strong("Q"), "uadratic, ", strong("H"), "inge, ", strong("P"), "roduct, ", strong("T"), "hreshold",
    tags$div(title='Feature combinations to be explored. Features are constructed using different relationships within and among the environmental predictors, and are used to constrain the computed probability distribution. In short, more features = more potential model complexity.',
             checkboxGroupInput(ns("fcs"), label='',
                                choices = list("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), inline = TRUE)),
    strong("Select regularization multipliers "), strong(em("(penalty against complexity)")),
    tags$div(title='Range of regularization multipliers to explore. Greater values of the regularization multiplier lead to increased penalty against overly complex and/or overfit models. A value of 0 results in no regularization.',
             sliderInput(ns("rms"), label = "",
                         min = 0.5, max = 10, step=0.5, value = c(1, 2))),
    tags$div(title='Value used to step through regularization multiplier range (e.g. range of 1-3 with step 0.5 results in [1, 1.5, 2, 2.5, 3]).',
             numericInput(ns("rmsStep"), label = "Multiplier step value", value = 1)),
    tags$div(title='If checked, the response will resist extrapolation to environmental values outside those used to build the model. See guidance for details.',
             checkboxInput(ns('clamp'), label = 'Clamp predictions?'))
  )
}

maxent_MOD <- function(input, output, session) {
  reactive({
    
    for(sp in spIn()) {  
      # ERRORS ####
      if (is.null(spp[[sp]]$occs$partition)) {
        shinyLogs %>% writeLog(type = 'error', "Before building a model, please partition 
                          occurrences for cross-validation for", spName(spp[[sp]]), ".")
        return()
      }
      # FUNCTION CALL ####
      m.maxent <- c6_maxent(spp[[sp]]$occs, 
                            spp[[sp]]$bg, 
                            spp[[sp]]$occs$partition,
                            spp[[sp]]$bg$partition,
                            spp[[sp]]$procEnvs$bgMask, 
                            input$rms, 
                            input$rmsStep, 
                            input$fcs, 
                            input$clamp, 
                            shinyLogs)
      req(m.maxent)
      
      # LOAD INTO SPP ####
      spp[[sp]]$results <- m.maxent
      
      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- "Maxent"
      spp[[sp]]$rmm$model$maxent$featureSet <- input$fcs
      spp[[sp]]$rmm$model$maxent$regularizationMultiplierSet <- input$rms
      spp[[sp]]$rmm$model$maxent$regularizationRule <- paste("increment by", input$rmsStep)
      spp[[sp]]$rmm$model$maxent$notes <- "dismo package implementation"
    }
    
    
  })
}

maxent_INFO <- infoGenerator(modName = "Maxent",
                             modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson",
                             pkgName = c("ENMeval", "dismo"))
