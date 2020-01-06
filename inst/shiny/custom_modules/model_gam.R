runGAM <- function(occs.vals, bg.vals, degFr, spN, logger = NULL) {
  if (!require(gam)) {
    logger %>% writeLog("Please install the gam package before running.")
    return()
  }
  # make vector of each variable wrapped in the spline function s() with
  # the assigned degrees of freedom
  degFrs <- paste0("s(", names(occs.vals), ", ", degFr, ")")
  # bind all environmental values together
  vals <- rbind(occs.vals, bg.vals)
  # make vector of 1's and 0's for identifying occurrence from background
  bin <- c(rep(1, nrow(occs.vals)), rep(0, nrow(bg.vals)))
  # put everything together in a table for modeling
  d <- data.frame(pa = bin, vals)
  # make formula
  f <- as.formula(paste("pa", paste(degFrs, collapse = " + "), sep = " ~ "))
  # run the GAM
  smartProgress(logger, message = paste("Running GAM for", spN), {
    mod <- gam(f, family = "binomial", data = d)
  })
  # write log message
  logger %>% writeLog("GAM ran successfully for ", spN, ".")
  # output model object
  return(mod)
}

gam_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      title = "These degrees of freedom represent the smoothing parameter for the splines corresponding to each variable.",
      numericInput(ns("degFr"), "Degrees of Freedom", value = 1)
    ),
    actionButton(ns('goGAM'), 'Run')
  )
}

gam_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  occs <- common$occs
  envs <- common$envs

  observeEvent(input$goGAM, {
    for (sp in list(curSp())) {
      # ERRORS ####
      if (is.null(spp[[sp]]$occs$partition)) {
        logger %>% writeLog(type = 'error', "Before building a model, please
                               partition occurrences for cross-validation for ",
                               em(spName(curSp())), ".")
        return()
      }

      # FUNCTION CALL ####
      mod.gam <- runGAM(spp[[sp]]$occs[,names(envs())], spp[[sp]]$bg[,names(envs())], input$degFr, sp, logger)

      # LOAD INTO SPP ####
      spp[[sp]]$results <- mod.gam

      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- "GAM"
      spp[[sp]]$rmm$model$gam$family <- "binominal"
      f <- as.character(mod.gam$formula)
      spp[[sp]]$rmm$model$gam$formula <- paste(f[2],f[1],f[3])
      spp[[sp]]$rmm$model$gam$notes <- "gam package implementation"
    }

    common$update_component(tab = "Results")
    common$remove_module(component = "vis", module = "mapPreds")
  })

  output$gamSummary <- renderPrint({
    summary(spp[[curSp()]]$results)
  })
}

gam_module_result <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("gamSummary"))
}
