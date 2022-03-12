#### COMPONENT 3: Obtain Environmental Data
#### MODULE: ecoClimate
context("ecoClimate")

##Set parameters

bcAOGCM="CCSM"
bcScenario="LGM"
ecoClimSel=c(1,2,3,15,16)

##Run function for output tests
varsEcoClimate<-envs_ecoClimate(bcAOGCM, bcScenario, ecoClimSel, logger = NULL)
##set error parameters
wrongAOGCM="CTSM"
wrongScenario='notscenario'
AOGCMFail='COSMOS'
bcScenarioFail='Present'

##start testing
test_that('error_checks', {
  ##wrong AOGCM
  expect_error(envs_ecoClimate(bcAOGCM=wrongAOGCM, bcScenario, ecoClimSel, logger = NULL),
  (paste0("ecoClimate has no data for AOGCM=", wrongAOGCM, ". Check the spelling.")))
  ##wrong Scenario
  expect_error(envs_ecoClimate(bcAOGCM, bcScenario=wrongScenario, ecoClimSel, logger = NULL),
               paste0("ecoClimate has no data for Scenario=", wrongScenario, ". Check the spelling."))

    ##NON existent combination of parameters
  expect_error(envs_ecoClimate(bcAOGCM=AOGCMFail, bcScenario=bcScenarioFail, ecoClimSel, logger = NULL),
               paste0("ecoClimate has no data for AOGCM = ", AOGCMFail, ", Baseline =  Modern", ", Scenario = ", bcScenarioFail))

  })

test_that('output_checks', {
  #output is a rasterstack
  expect_is(varsEcoClimate,"RasterStack")
  ##rasterstack has as many layers as requested
  expect_equal(raster::nlayers(varsEcoClimate),length(ecoClimSel))
  ##names of variables correspond to requested variables
  names<-vector()
  for (i in 1:length(ecoClimSel)){
    if(ecoClimSel[i]<10){names[i]<-paste0('bio0',ecoClimSel[i])}
    else {names[i]<-paste0('bio',ecoClimSel[i])}
  }
  expect_equal(names(varsEcoClimate),names)

})

