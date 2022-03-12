
#' @title model_maxent Generate maxent or maxnet model
#' @description This functions generates maxent or maxnet models using ENMeval 2.0 and user provided tuning parameters.
#'
#' @details
#' The function generates model in ENMeval using a user provided partition of occurrences from previous components in the GUI.
#' User can activate clamping and input tuning arguments to be used for model building
#'
#' @param occs data frame of cleaned or processed occurrences obtained from components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param bg  coordinates of background points to be used for modeling
#' @param user.grp  a list of two vectors containing group assignments for occurrences (occs.grp) and background points (bg.grp)
#' @param bgMsk a RasterStack or a RasterBrick of environmental layers cropped and masked to match the provided background extent
#' @param rms vector of range of regularization multipliers to be used in the ENMeval run
#' @param rmsStep step to be used when defining regularization multipliers to be used from the provided range.
#' @param fcs feature classes to be tested in the ENMeval run
#' @param clampSel Boolean use of clamping in the model
#' @param algMaxent character. algorithm to be used in modeling. A selection of "maxnet" or "maxent.jar"
#' @param catEnvs  if categorical predictor variables are included must provide the names
#' @param parallel logical. Whether to use parallel in the generation of models. Default is FALSE
#' @param numCores numeric. If using parallel how many cores to use. Default is NULL
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN character. Species name to be used for all logger messages

# @keywords
#'
#' @examples
#' out.gbif <- occs_queryDb(spName = "Panthera onca", occDb = "gbif", occNum = 1000)
#' occs <- as.data.frame(out.gbif[[1]]$cleaned)
#' envs <- envs_worldclim(bcRes = 10,
#'                        bcSel = c("bio01","bio02","bio07","bio13",
#'                                  "bio14","bio15","bio19"),
#'                        doBrick = FALSE)
#' ## remove records without environmental values
#' records <- which(is.na(raster::extract(envs$bio01, occs[,3:4])) == TRUE)
#' occs <- occs[-records, ]
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMsk <- penvs_bgMask(occs, envs, bgExt)
#' bg <-penvs_bgSample(occs, bgMsk, bgPtsNum = 10000)
#' partblock <- part_partitionOccs(occs, bg, method = 'block',
#'                                 kfolds = NULL, bgMask = NULL,
#'                                 aggFact = NULL)
#' rms <- c(1:2)
#' rmsStep <- 1
#' fcs <- c('L', 'LQ')
#' m <- model_maxent(occs = occs, bg = bg, user.grp = partblock,
#'                   bgMsk = bgMsk, rms = rms, rmsStep, fcs,
#'                   clampSel = TRUE, algMaxent = "maxnet",
#'                   parallel = FALSE)

#'
#' @return Function returns an ENMevaluate object with all the evaluated models and a selection of appropriate fields.
#' @author Jamie M. Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note

#' @seealso \code{\link[ENMeval]{ENMevaluate}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be
# linked in the documentation.
#' @importFrom rlang .data
#' @export

model_maxent <- function(occs, bg, user.grp, bgMsk, rms, rmsStep, fcs,
                         clampSel, algMaxent, catEnvs = NULL, parallel = FALSE,
                         numCores = NULL, logger = NULL, spN = NULL) {

  if (is.null(user.grp)) {
    logger %>% alfred.writeLog(
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
        alfred.writeLog(
          type = 'error',
          "To use Maxent, make sure you download, ", strong("maxent.jar"),
          " from the ",
          a("AMNH Maxent webpage",
            href = "http://biodiversityinformatics.amnh.org/open_source/maxent/",
            target = "_blank"),
            " and place it in this directory:", br(), em(jar))
      return()
    }

    if (!requireNamespace('rJava')) {
      logger %>% alfred.writeLog(
        type = "error",
        paste0('Package rJava cannot load. Please download the latest version of ',
               'Java, and make sure it is the correct version (e.g. 64-bit for a ',
               '64-bit system). After installing, try "library(rJava)". If it ',
               'loads properly, restart Wallace and try again. If it does not, ',
               'please consult www.github.com/wallaceecomod/wallace for more ',
               'tips on getting rJava to work.'))
      return()
    }

    # Check maxent version
    if (is.null(getOption('dismo_rJavaLoaded'))) {
      # to avoid trouble on macs
      Sys.setenv(NOAWT=TRUE)
      if ( requireNamespace('rJava') ) {
        rJava::.jpackage('dismo')
        options(dismo_rJavaLoaded=TRUE)
      } else {
        stop('rJava cannot be loaded')
      }
    }
    mxe <- rJava::.jnew("meversion")
    maxentJARversion <- try(rJava::.jcall(mxe, "S", "meversion"))

    if (maxentJARversion < "3.4.4") {
      logger %>% alfred.writeLog(
        type = "error",
        "Please, use the updated version of Maxent (v3.4.4). Currently, you are ",
        "using (", maxentJARversion, ")."
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
                                  alfred.spName(spN), "..."),
                 value = 0)
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
  occs.xy <- occs %>% dplyr::select(.data$longitude, .data$latitude)
  bg.xy <- bg %>% dplyr::select(.data$longitude, .data$latitude)
  # run ENMeval
  e <- ENMeval::ENMevaluate(occs = as.data.frame(occs.xy),
                            bg = as.data.frame(bg.xy),
                            partitions = 'user',
                            user.grp = user.grp,
                            envs = bgMsk,
                            tune.args = tune.args,
                            doClamp = clampSel,
                            algorithm = algMaxent,
                            categoricals = catEnvs,
                            parallel = parallel,
                            numCores = numCores,
                            parallelType = "doSNOW",
                            # taxon.name = NULL,
                            # user.enm = NULL,
                            # occs.ind = NULL,
                            # kfolds = NA,
                            # aggregation.factor = c(2,2),
                            # orientation = "lat_lon",
                            # overlap = FALSE,
                            # overlapStat = c("D","I"),
                            # pred.type = "cloglog",
                            # abs.auc.diff = FALSE,
                            # user.test.grps = NULL,
                            updateProgress = updateProgress,
                            quiet = FALSE)

  occPredVals <- raster::extract(e@predictions, occs.xy)

  endTxt <- paste("]), using", algMaxent, "with clamping",
                  ifelse(clampSel, "on.", "off."))

  logger %>% alfred.writeLog(alfred.hlSpp(spN),
    "Maxent ran successfully and output evaluation ",
    "results for ", nrow(e@results), " models (Regularization multiplier values: [",
    paste(rms.interval, collapse = ", "),"]; Feature classes: [",
    paste(fcs, collapse = ", "), endTxt, "")
  return(e)

}

