
#' @title envs_ecoClimate Obtain ecoClimate variables
#' @description download ecoClimate variables. See www.ecoclimate.org.
#'
#' @details This function is called by the module envs to download ecoClimate
#'   variables from www.ecoclimate.org. The variables to be downloaded are
#'   selected by the user with bcSel and the resolution is fixed to 0.5 degrees.
#'   This function currently gets variables from Dropbox and the process takes
#'   significantly more time than for other datasets. It returns a rasterStack
#'   of selected variables.
#'
#' @param bcAOGCM Name of the Atmospheric and Oceanic Global Circulation Model.
#'   Options are: "CCSM", "CNRM", "MIROC", "FGOALS", "GISS", "IPSL","MRI", "MPI"
#' @param bcScenario Select the temporal scenario that you want to download.
#'   Options are: "LGM" (21,000 years ago), "Holo" (6,000 years ago),
#'   "Present", "Future 2.6" (rcp 2.6), "Future 4.5" (rcp 4.5),
#'   "Future 6" (rcp 6), "Future 8.5" (rcp 8.5)
#' @param ecoClimSel Numeric vector with list of variables to select.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL.
#'
#' @examples
#' bcAOGCM <- "CCSM"
#' bcScenario <- "LGM"
#' ecoClimSel <- c(1,2,3)
#' \dontrun{
#' varsEcoClimate <- envs_ecoClimate(bcAOGCM, bcScenario, ecoClimSel)
#' }
#'
#' @return A rasterStack of selected variables
#'
#' @author Sara Varela <sara_varela@@yahoo.com>
#' @author Jamie M. Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#'
#' @export
#'
envs_ecoClimate <- function(bcAOGCM, bcScenario, ecoClimSel, logger = NULL) {
  smartProgress(logger, message = "Retrieving ecoClimate data...", {
    ecoClimatelayers <- ecoClimate_getdata(AOGCM = bcAOGCM,
                                           Baseline = "Modern",
                                           Scenario = bcScenario,
                                           logger)
  })
  if (is.null(ecoClimatelayers)) return()
  ecoClimatelayers <- ecoClimate_select(ecoClimatelayers, Sels = ecoClimSel)
  # Changing rasters names
  names(ecoClimatelayers) <- gsub("\\.", "", names(ecoClimatelayers))
  i <- grep('bio[0-9]$', names(ecoClimatelayers))
  editNames <- paste('bio', sapply(strsplit(names(ecoClimatelayers)[i], 'bio'),
                                   function(x) x[2]), sep = '0')
  names(ecoClimatelayers)[i] <- editNames

  # Define WGS84
  raster::crs(ecoClimatelayers) <- raster::crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  logger %>% writeLog("Environmental predictors: ecoClimate bioclimatic variables ",
                      paste(names(ecoClimatelayers), collapse = ", "),
                      " at 0.5 degree resolution. Global Circulation Model = ",
                      bcAOGCM, ", Scenario = ", bcScenario, ". ")

  return(ecoClimatelayers)
}

###Auxiliary functions
#' @title ecoClimate_getdata
#'
#' @description download ecoClimate layers. more info at www.ecoclimate.org
#'
#' @usage ecoClimate_getdata(AOGCM, Baseline, Scenario, logger)
#'
#' @param AOGCM Select the AOGCM.
#'   Options are: "CCSM", "CNRM", "MIROC", "COSMOS", "FGOALS", "GISS", "IPSL",
#'   "MRI", "MPI"
#' @param Baseline Select a baseline for the climatic layers.
#'   Options are: "Pre-industrial" (piControl-1760), "Historical" (1900-1949),
#'   "Modern" (1950-1999)
#' @param Scenario Select a temporal scenario.
#'   Options are: "LGM" (21,000 years ago), "Holo" (6,000 years ago),
#'   "Present", "Future 2.6" (rcp 2.6), "Future 4.5" (rcp 4.5), "Future 6" (rcp 6),
#'   "Future 8.5" (rcp 8.5)
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'   otherwise leave the default NULL
#' @export
#'
#' @examples \dontrun{
#' CCSM_mod_present <- ecoclimate_getdata("CCSM", "Modern", "Present")
#' dev.new()
#' plot(CCSM_mod_present)
#' }
#'

ecoClimate_getdata <- function (AOGCM, Baseline, Scenario, logger = NULL) {

  if (!(AOGCM %in% c("CCSM", "CNRM", "MIROC", "COSMOS", "FGOALS", "GISS",
                     "IPSL", "MRI", "MPI"))) {
    stop(paste0("ecoClimate has no data for AOGCM=", AOGCM, ". Check the spelling."))
  }

  if (!(Baseline %in% c("Pre-industrial", "Historical","Modern"))) {
    stop(paste0("ecoClimate has no data for Baseline=", Baseline,
                ". Check the spelling."))
  }

  if (!(Scenario %in% c("LGM", "Holo", "Present", "Future 2.6", "Future 4.5",
                        "Future 6", "Future 8.5"))) {
    stop(paste0("ecoClimate has no data for Scenario=", Scenario, ". Check the spelling."))
  }

  if (AOGCM == "CCSM" && Baseline == "Modern" && Scenario == "Present") {
    FinURL <- paste0("https://www.dropbox.com/sh/ntl1ieo3fb5q2g9/AAByugxGAwEvuNxCMsj7gfI2a/bio%20%23%20CCSM_Modern%281950-1999%29.txt?dl=1")
  }

  if (AOGCM == "CNRM" && Baseline == "Modern" && Scenario == "Present") {
    FinURL <- paste0("https://www.dropbox.com/sh/ntl1ieo3fb5q2g9/AAB7ltZRxzYkjv6gZ4QNVWBka/bio%20%23%20CNRM_Modern%281950-1999%29.txt?dl=1")
  }

  if (AOGCM == "MIROC" && Baseline == "Modern" && Scenario == "Present") {
    FinURL <- paste0("https://www.dropbox.com/sh/ntl1ieo3fb5q2g9/AAA4zPbhp-TMN8ohv6plWoFha/bio%20%23%20MIROC_Modern%281950-1999%29.txt?dl=1")
  }

  if (AOGCM == "COSMOS" && Baseline == "Modern" && Scenario == "Present") {
    logger %>% writeLog(type = 'error',
    "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }

  if (AOGCM == "FGOALS" && Baseline == "Modern" && Scenario == "Present") {
    FinURL <- paste0("https://www.dropbox.com/sh/ntl1ieo3fb5q2g9/AACThTqhfBRHpECBKmTJNA4Za/bio%20%23%20FGOALS_Modern%281950-1999%29.txt?dl=1")
  }
  if (AOGCM == "GISS" && Baseline == "Modern" && Scenario == "Present") {
    FinURL <- paste0("https://www.dropbox.com/sh/ntl1ieo3fb5q2g9/AADt6BQzXu1Uk_B9oW6_XnWUa/bio%20%23%20GISS_Modern%281950-1999%29.txt?dl=1")
  }
  if (AOGCM == "IPSL" && Baseline == "Modern" && Scenario == "Present") {
    FinURL <- paste0("https://www.dropbox.com/sh/ntl1ieo3fb5q2g9/AAC0Sv9Ga5BmU5EhrYUxguA6a/bio%20%23%20IPSL_Modern%281950-1999%29.txt?dl=1")
  }
  if (AOGCM == "MRI" && Baseline == "Modern" && Scenario == "Present") {
    FinURL <- paste0("https://www.dropbox.com/sh/ntl1ieo3fb5q2g9/AAAwZpUpIYfD69d4sgThrWB2a/bio%20%23%20MRI_Modern%281950-1999%29.txt?dl=1")
  }
  if (AOGCM == "MPI" && Baseline == "Modern" && Scenario == "Present") {
    FinURL <- paste0("https://www.dropbox.com/sh/ntl1ieo3fb5q2g9/AADlJ8v41rP0Nd65PMCeCIxFa/bio%20%23%20MPI_Modern%281950-1999%29.txt?dl=1")
  }

  ## LGM
  if (AOGCM == "CCSM" && Baseline == "Modern" && Scenario == "LGM") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AADWJOs0_8zQmhc0XJxJE9a2a/bio%20%23baseline_Modern%281950-1999%29%23%20CCSM_LGM%2821ka%29.txt?dl=1")
  }
  if (AOGCM == "CNRM" && Baseline == "Modern" && Scenario == "LGM") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AABWiVQptLSgkPkVC4aJCM2Ta/bio%20%23baseline_Modern%281950-1999%29%23%20CNRM_LGM%2821ka%29.txt?dl=1")
  }
  if (AOGCM == "MIROC" && Baseline == "Modern" && Scenario == "LGM") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AAB5NqekMCHtD4_6eJWPJs1ja/bio%20%23baseline_Modern%281950-1999%29%23%20MIROC_LGM%2821ka%29.txt?dl=1")
  }

  if (AOGCM == "COSMOS" && Baseline == "Modern" && Scenario == "LGM") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }

  if (AOGCM == "FGOALS" && Baseline == "Modern" && Scenario == "LGM") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AABSZKM3FJKJnatGPW164WK5a/bio%20%23baseline_Modern%281950-1999%29%23%20FGOALS_LGM%2821ka%29.txt?dl=1")
  }
  if (AOGCM == "GISS" && Baseline == "Modern" && Scenario == "LGM") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AADiQFLKVFunSXNC-qaNL_voa/bio%20%23baseline_Modern%281950-1999%29%23%20GISS_LGM%2821ka%29.txt?dl=1")
  }
  if (AOGCM == "IPSL" && Baseline == "Modern" && Scenario == "LGM") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AABmebhEnLInaR1-d6R6Giara/bio%20%23baseline_Modern%281950-1999%29%23%20IPSL_LGM%2821ka%29.txt?dl=1")
  }
  if (AOGCM == "MRI" && Baseline == "Modern" && Scenario == "LGM") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AADUevi4Go4dkm4smZCei6Mqa/bio%20%23baseline_Modern%281950-1999%29%23%20MRI_LGM%2821ka%29.txt?dl=1")
  }
  if (AOGCM == "MPI" && Baseline == "Modern" && Scenario == "LGM") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AAB51J8w7P_awhDlif4mmte0a/bio%20%23baseline_Modern%281950-1999%29%23%20MPI_LGM%2821ka%29.txt?dl=1")
  }


  ## Holocene

  if (AOGCM == "CCSM" && Baseline == "Modern" && Scenario == "Holo") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AADh9RT99OC7J5wfJojxiJGQa/bio%20%23baseline_Modern%281950-1999%29%23%20CCSM_mHol%286ka%29.txt?dl=1")
  }
  if (AOGCM == "CNRM" && Baseline == "Modern" && Scenario == "Holo") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AABZo3wRHaMImb8xde2yc50xa/bio%20%23baseline_Modern%281950-1999%29%23%20CNRM_mHol%286ka%29.txt?dl=1")
  }
  if (AOGCM == "MIROC" && Baseline == "Modern" && Scenario == "Holo") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AACP_KZWd4cceIUXTkEkfMfQa/bio%20%23baseline_Modern%281950-1999%29%23%20MIROC_mHol%286ka%29.txt?dl=1")
  }

  if (AOGCM == "COSMOS" && Baseline == "Modern" && Scenario == "Holo") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }

  if (AOGCM == "FGOALS" && Baseline == "Modern" && Scenario == "Holo") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AAAvdu0QfwI7BF1xNtgUe6y8a/bio%20%23baseline_Modern%281950-1999%29%23%20FGOALS_mHol%286ka%29.txt?dl=1")
  }
  if (AOGCM == "GISS" && Baseline == "Modern" && Scenario == "Holo") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }

  if (AOGCM == "IPSL" && Baseline == "Modern" && Scenario == "Holo") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AAB30ZvQWFycw9h0yuqPolqua/bio%20%23baseline_Modern%281950-1999%29%23%20IPSL_mHol%286ka%29.txt?dl=1")
  }
  if (AOGCM == "MRI" && Baseline == "Modern" && Scenario == "Holo") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AACdpC0rERiDFuucY-6zHkW6a/bio%20%23baseline_Modern%281950-1999%29%23%20MRI_mHol%286ka%29.txt?dl=1")
  }
  if (AOGCM == "MPI" && Baseline == "Modern" && Scenario == "Holo") {
    FinURL <- paste0("https://www.dropbox.com/sh/kijh17ehg8v3uv8/AABkgb52UgXyq-4TvVeaRgCMa/bio%20%23baseline_Modern%281950-1999%29%23%20MPI_mHol%286ka%29.txt?dl=1")
  }


  ## FUTURE 8.5
  if (AOGCM == "CCSM" && Baseline == "Modern" && Scenario == "Future 8.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AABMSJRupXipBfwa33rFAOWIa/bio%20%23baseline_Modern%281950-1999%29%23%20CCSM_rcp85%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "CNRM" && Baseline == "Modern" && Scenario == "Future 8.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAC5kjvYo-tlD1rplQzR5Yvga/bio%20%23baseline_Modern%281950-1999%29%23%20CNRM_rcp85%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MIROC" && Baseline == "Modern" && Scenario == "Future 8.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAAXxCEacxrks78dEY_ZVHpha/bio%20%23baseline_Modern%281950-1999%29%23%20MIROC_rcp85%282080-2100%29.txt?dl=1")
  }

  if (AOGCM == "COSMOS" && Baseline == "Modern" && Scenario == "Future 8.5") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }

  if (AOGCM == "FGOALS" && Baseline == "Modern" && Scenario == "Future 8.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AABEhuQiHinPOg2xhdduGsCOa/bio%20%23baseline_Modern%281950-1999%29%23%20FGOALS_rcp85%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "GISS" && Baseline == "Modern" && Scenario == "Future 8.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAASGrldKVv6zV_GCDf-T78ka/bio%20%23baseline_Modern%281950-1999%29%23%20GISS_rcp85%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "IPSL" && Baseline == "Modern" && Scenario == "Future 8.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAA-7SOARlsIHE5WalMwFluPa/bio%20%23baseline_Modern%281950-1999%29%23%20IPSL_rcp85%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MRI" && Baseline == "Modern" && Scenario == "Future 8.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AADQ-geA4e9nzXQSH4SOdq3la/bio%20%23baseline_Modern%281950-1999%29%23%20MRI_rcp85%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MPI" && Baseline == "Modern" && Scenario == "Future 8.5") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }


  ## FUTURE 2.6
  if (AOGCM == "CCSM" && Baseline == "Modern" && Scenario == "Future 2.6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAD2rXFucDHfwmOW7LUAhF5ia/bio%20%23baseline_Modern%281950-1999%29%23%20CCSM_rcp26%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "CNRM" && Baseline == "Modern" && Scenario == "Future 2.6") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }
  if (AOGCM == "MIROC" && Baseline == "Modern" && Scenario == "Future 2.6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAA49-6_FwVvRxsQLBborOkha/bio%20%23baseline_Modern%281950-1999%29%23%20MIROC_rcp26%282080-2100%29.txt?dl=1")
  }

  if (AOGCM == "COSMOS" && Baseline == "Modern" && Scenario == "Future 2.6") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }

  if (AOGCM == "FGOALS" && Baseline == "Modern" && Scenario == "Future 2.6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AACGdnVRhJfh1x6lhE2Yq9iaa/bio%20%23baseline_Modern%281950-1999%29%23%20FGOALS_rcp26%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "GISS" && Baseline == "Modern" && Scenario == "Future 2.6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAAC5MgcCaL4E_i9TiffD8Iga/bio%20%23baseline_Modern%281950-1999%29%23%20GISS_rcp26%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "IPSL" && Baseline == "Modern" && Scenario == "Future 2.6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AADQrUtdiTkOm4WVMjMyaWhfa/bio%20%23baseline_Modern%281950-1999%29%23%20IPSL_rcp26%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MRI" && Baseline == "Modern" && Scenario == "Future 2.6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AADxV4qNkInBdqpNMSASycTCa/bio%20%23baseline_Modern%281950-1999%29%23%20MRI_rcp26%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MPI" && Baseline == "Modern" && Scenario == "Future 2.6") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }


  ## FUTURE 4.5
  if (AOGCM == "CCSM" && Baseline == "Modern" && Scenario == "Future 4.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AADV5Qf8D7wgWSMuL30I-XQBa/bio%20%23baseline_Modern%281950-1999%29%23%20CCSM_rcp45%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "CNRM" && Baseline == "Modern" && Scenario == "Future 4.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAAIcA1wbD-YXtMxhHMSx07Sa/bio%20%23baseline_Modern%281950-1999%29%23%20CNRM_rcp45%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MIROC" && Baseline == "Modern" && Scenario == "Future 4.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AADCaiv9XlR32llK2tISaT92a/bio%20%23baseline_Modern%281950-1999%29%23%20MIROC_rcp45%282080-2100%29.txt?dl=1")
  }

  if (AOGCM == "COSMOS" && Baseline == "Modern" && Scenario == "Future 4.5") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }

  if (AOGCM == "FGOALS" && Baseline == "Modern" && Scenario == "Future 4.5") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }
  if (AOGCM == "GISS" && Baseline == "Modern" && Scenario == "Future 4.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAAiV_w1REiX61Si9FjBCMLxa/bio%20%23baseline_Modern%281950-1999%29%23%20GISS_rcp45%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "IPSL" && Baseline == "Modern" && Scenario == "Future 4.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAAXW0R4mAH2LmZJa8TUiIcVa/bio%20%23baseline_Modern%281950-1999%29%23%20IPSL_rcp45%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MRI" && Baseline == "Modern" && Scenario == "Future 4.5") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AABET5mP2c9qPladhp6nkcHBa/bio%20%23baseline_Modern%281950-1999%29%23%20MRI_rcp45%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MPI" && Baseline == "Modern" && Scenario == "Future 4.5") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }


  ## FUTURE 6
  if (AOGCM == "CCSM" && Baseline == "Modern" && Scenario == "Future 6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAAu9NfGSwBSqvQ_sbDrUjtpa/bio%20%23baseline_Modern%281950-1999%29%23%20CCSM_rcp60%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "CNRM" && Baseline == "Modern" && Scenario == "Future 6") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }
  if (AOGCM == "MIROC" && Baseline == "Modern" && Scenario == "Future 6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AADuGCrBF0brWWtrBjomSQfOa/bio%20%23baseline_Modern%281950-1999%29%23%20MIROC_rcp60%282080-2100%29.txt?dl=1")
  }

  if (AOGCM == "COSMOS" && Baseline == "Modern" && Scenario == "Future 4.5") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }

  if (AOGCM == "FGOALS" && Baseline == "Modern" && Scenario == "Future 6") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }
  if (AOGCM == "GISS" && Baseline == "Modern" && Scenario == "Future 6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AADo_ux8CBb6fBGxxSrdRrJRa/bio%20%23baseline_Modern%281950-1999%29%23%20GISS_rcp60%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "IPSL" && Baseline == "Modern" && Scenario == "Future 6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AACBY6nVnv4oeCie5G6vHDm7a/bio%20%23baseline_Modern%281950-1999%29%23%20IPSL_rcp60%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MRI" && Baseline == "Modern" && Scenario == "Future 6") {
    FinURL <- paste0("https://www.dropbox.com/sh/ei6m84sctoinhi9/AAAfNyo79Z3RpJ-7wqzMjdRZa/bio%20%23baseline_Modern%281950-1999%29%23%20MRI_rcp60%282080-2100%29.txt?dl=1")
  }
  if (AOGCM == "MPI" && Baseline == "Modern" && Scenario == "Future 6") {
    logger %>% writeLog(type = 'error', "ecoClimate has no data for AOGCM = ", AOGCM, ", Baseline =  ", Baseline, ", Scenario = ", Scenario)
    return()
  }

  # Download data
  fn <- paste(tempfile(), '.txt', sep='')
  fnDw <- utils::download.file(url = FinURL, destfile = fn, method = "auto",
                              quiet = FALSE, mode = "wb", cacheOK = TRUE)
  if (file.exists(fn) & fnDw == 0) {
    climate_data <- utils::read.table(fn, TRUE)
    sp::gridded(climate_data) <- ~ long + lat
    map_climate<- raster::stack(climate_data)[[-1]]
  } else {
    stop('Could not download the ecoClimate file')
  }
  return(map_climate)
}


#' ecoClimate_select
#'
#' select which bioclimatic variables and set the extent you want (crop the raster stack to your study extent)
#'
#' @usage ecoClimate_select(map_climate, Sels=c(1:19), extent=c(-180, 180, -90, 90))
#'
#' @param map_climate raster stack with all the variables
#' @param Sels vector of integer numbers. 1 for bio1, 2 for bio2, etc. e.g. Sels= c(1,12,6) for selecting bio1, bio12 and bio6
#' @param extent vector. xmin, xmax, ymin, ymax. e.g. c()
#' @export
#' @examples \dontrun{
#' CCSM_mod_present <- ecoclimate_getdata("CCSM", "Modern", "Present")
#' Europe_CCSM_m_p_bio1_12 <- ecoClimate_select(CCSM_mod_present, c(1, 12),
#'                                              extent = c(-20, 80, 20, 80))
#' dev.new()
#' plot(Europe_CCSM_m_p_bio1_12)
#' }
#'
#'

ecoClimate_select <- function(map_climate, Sels = c(1:19), extent = c(-180, 180, -90, 90)) {
  select_var <- map_climate[[Sels]]
  crop_stack <- raster::crop(select_var, extent)
  return(crop_stack)
}
