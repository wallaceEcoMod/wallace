#### COMPONENT espace: Do analysis in environmental space.
#### MODULE: Occurence density grid
context("espace_occDens")

##Function has no warning or error messages.
## To run this function a PCA must exist, this requires a model to be created
## first allowing for access to environmental data for occs and background
###Using bioclim for testing
###SET PARAMETERS (running model)
sp.name1 <- "Bassaricyon_alleni"
sp.name2 <- "Bassaricyon_neblina"
envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
                                                       package = "wallace"),
                                           pattern = ".tif$", full.names = TRUE),
                      rasName = list.files(system.file("extdata/wc",
                                                       package = "wallace"),
                                           pattern = ".tif$", full.names = FALSE))
occs.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                                package = "wallace"))
occs.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                                package = "wallace"))
bgPts.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
                                 package = "wallace"))
bgPts.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina_bgPoints.csv",
                                 package = "wallace"))
occsExt.z1 <- raster::extract(envs, occs.z1[, c("longitude", "latitude")])
occsExt.z2 <- raster::extract(envs, occs.z2[, c("longitude", "latitude")])
bgExt.z1 <- raster::extract(envs, bgPts.z1[, c("longitude", "latitude")])
bgExt.z2 <- raster::extract(envs, bgPts.z2[, c("longitude", "latitude")])
###Generate pca for further analyses
Testpca <- espace_pca(sp.name1, sp.name2, occsExt.z1, occsExt.z2,
                      bgExt.z1, bgExt.z2)
###RUN FUNCTION
TestOccDens<-espace_occDens(sp.name1, sp.name2,Testpca)
test_that("output checks", {
  #output is a list
  expect_equal(mode(TestOccDens),"list")
  #list is of objects of type list
  expect_is(TestOccDens,"list")
  #list has 2 objects (2 species)
  expect_equal(length(TestOccDens),2)
  #The name of each list is the name of the species
  expect_equal(names(TestOccDens),c(sp.name1,sp.name2))
  #each list contains 10 objects
  expect_equal(length(TestOccDens[[sp.name1]]),10)
  expect_equal(length(TestOccDens[[sp.name2]]),10)
  ##The name of the slots of each list is correct
  expect_equal(names(TestOccDens[[sp.name1]]),
               c("y","x","z","z.uncor","z.cor","Z","glob","glob1","sp","w"))
  expect_equal(names(TestOccDens[[sp.name2]]),
               c("y","x","z","z.uncor","z.cor","Z","glob","glob1","sp","w"))
   ## Test that all outputs but x and y and inputs (including occupancy, density
   ## and weights) are all raster layers
  #sp1
  expect_is(TestOccDens[[sp.name1]]$z,'RasterLayer')
  expect_is(TestOccDens[[sp.name1]]$Z,'RasterLayer')
  expect_is(TestOccDens[[sp.name1]]$z.uncor,'RasterLayer')
  expect_is(TestOccDens[[sp.name1]]$z.cor,'RasterLayer')
  expect_is(TestOccDens[[sp.name1]]$w,'RasterLayer')
  #sp2
  expect_is(TestOccDens[[sp.name2]]$z,'RasterLayer')
  expect_is(TestOccDens[[sp.name2]]$Z,'RasterLayer')
  expect_is(TestOccDens[[sp.name2]]$z.uncor,'RasterLayer')
  expect_is(TestOccDens[[sp.name2]]$z.cor,'RasterLayer')
  expect_is(TestOccDens[[sp.name2]]$w,'RasterLayer')
  })




