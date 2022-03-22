#### COMPONENT espace: Do analysis in environmental space.
#### MODULE: Principal component analysis
context("espace_pca")


##Function has no warning or error messages.
##To run this function a model must be created first allowing for access to
##environmental data for occs and background
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
test_that("output checks", {
  #output is a list
  expect_equal(mode(Testpca),"list")
  #list is of objects of typoe "pca" and "dudi"
  expect_is(Testpca,"pca")
  expect_is(Testpca,"dudi")
  #list has 14 objects
  expect_equal(length(Testpca),14)
  #objects in list are as expected
  expect_equal(c('tab', 'cw','lw', 'eig', 'rank', 'nf', 'c1', 'li', 'co', 'l1',
                 'call', 'cent', 'norm', 'scores'), names(Testpca))
})



