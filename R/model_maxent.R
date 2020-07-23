
#' @title model_maxent Generate maxent or maxnet model
#' @description This functions generates maxent or maxnet models using ENMeval 2.0 and user provided tuning parameters.
#'
#' @details
#' The function generates model in ENMeval using a user porvided parition of occurrences from previous components i GUI.
#' User can activate clamping and input de tuning argumenta to be used for model building
#'
#' @param occs x
#' @param bg  x
#' @param user.grp
#' @param bgMsk x
#' @param rms x
#' @param rmsStep x
#' @param fcs x
#' @param parallel
#' @param numCores
#' @param logger logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN Species name to be used for all logger messages

# @keywords
#'
# @examples
#'
#'
#' @return Fucntion returns an ENMevaluate object with all the evaluated models.
#' selection of appropriate fields.
#' @author Jamie M. Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be
# linked in the documentation.
#' @export

model_maxent <- function(occs, bg, user.grp, bgMsk, rms, rmsStep, fcs,
                         clampSel, algMaxent, catEnvs, parallel=FALSE, numCores=NULL, logger = NULL,
                         spN = NULL) {

  if (is.null(occsGrp)) {
    logger %>% writeLog(
      type = 'error',
      "Before building a model, please partition occurrences for cross-validation."
    )
    return()
  }

  # if maxent.jar selected check for jar file and whether rJava can be loaded
  if (algMaxent == "maxent.jar") {
    # error for no maxent.jar in dismo directory
    jar <- paste(system.file(package = "dismo"), "/java/maxent.jar", sep = '')
    if (!file.exists(jar)) {
      logger %>%
        writeLog(
          type = 'error',
          "To use Maxent, make sure you download, ", strong("maxent.jar"),
          " from the ",
          a("AMNH Maxent webpage",
            href = "http://biodiversityinformatics.amnh.org/open_source/maxent/",
            target = "_blank"),
            " and place it in this directory:", br(), em(jar))
      return()
    }

    if (!require('rJava')) {
      logger %>% writeLog(
        type = "error",
        paste0('Package rJava cannot load. Please download the latest version of ',
               'Java, and make sure it is the correct version (e.g. 64-bit for a ',
               '64-bit system). After installing, try "library(rJava)". If it ',
               'loads properly, restart Wallace and try again. If it does not, ',
               'please consult www.github.com/wallaceecomod/wallace for more ',
               'tips on getting rJava to work.'))
      return()
    }

    if (maxentJARversion() < "3.4.1") {
      logger %>% writeLog(
        type = "error",
        "Please, use the updated version of Maxent (v3.4.1). Currently, you are",
        "using (", maxentJARversion(), ").(**)"
      )
      return()
    }
  }

  # define the vector of RMs to input
  rms.interval <- seq(rms[1], rms[2], rmsStep)
  ##set up tuning parameter argument
  tune.args=list(fc = fcs, rm = rms.interval)
  # create the Progress Bar object for ENMeval
  if (!is.null(logger)) {
    progress <- shiny::Progress$new()
    progress$set(message = paste0("Building/Evaluating ENMs for ",
                                  spName(spN), "..."), value = 0)
    on.exit(progress$close())
    n <- length(rms.interval) * length(fcs)
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$inc(amount = 1/n, detail = detail)
    }
  } else {
    n <- length(rms.interval) * length(fcs)
    updateProgress <- FALSE
  }

  # get just coordinates
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  bg.xy <- bg %>% dplyr::select(longitude, latitude)
  # run ENMeval
  e <- ENMeval::ENMevaluate(occs = occs.xy, envs = bgMsk, bg = bg.xy,
                            tune.args=tune.args,
                            taxon.name=NULL,categoricals = catEnvs,
                            mod.name = algMaxent, user.enm=NULL,partitions = 'user', user.grp=user.grp,
                            occs.ind=NULL, kfolds=NA, aggregation.factor=c(2,2), orientation="lat_lon",
                            n.bg=10000, overlap= FALSE, overlapStat=c("D","I"),
                            clamp = clampSel, pred.type="cloglog", abs.auc.diff=FALSE,
                            user.test.grps = NULL,
                            parallel=parallel,numCores=numCores,parallelType="doSNOW",
                            updateProgress = updateProgress,quiet=FALSE)

  occPredVals <- raster::extract(e@predictions, occs.xy)

  endTxt <- paste("]), using", algMaxent, "with clamping",
                  ifelse(clampSel, "on.", "off."))

  logger %>% writeLog(hlSpp(spN),
    "Maxent ran successfully and output evaluation ",
    "results for ", nrow(e@results), " models (Regularization multiplier values: [",
    paste(rms.interval, collapse = ", "),"]; Feature classes: [",
    paste(fcs, collapse = ", "), endTxt, "(**)")
  return(e)

}

