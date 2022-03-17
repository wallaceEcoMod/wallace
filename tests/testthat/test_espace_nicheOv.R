#### COMPONENT espace: Do analysis in environmental space.
#### MODULE: espace
context("espace_nicheOv")

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
###Generate ecospat occDens objects
TestOccDens <- espace_occDens(sp.name1, sp.name2, Testpca)
z1 <- TestOccDens[[sp.name1]]
z2 <-  TestOccDens[[sp.name2]]
###RUN FUNCTION
iter <- 100
TestNicheOv <- espace_nicheOv(z1, z2, iter, equivalency = TRUE,
                              similarity = TRUE, logger = NULL)

### test output features
  test_that("output checks", {
  ##Output is a list
  expect_is(TestNicheOv,"list")
  ##all entries are there
  expect_equal(length(TestNicheOv),4)
  expect_equal(names(TestNicheOv),c("overlap", "USE", "equiv", "simil"))
  ###output type and content is correct
  expect_is(TestNicheOv$overlap,'list')
  expect_is(TestNicheOv$overlap$D,'numeric')
  expect_is(TestNicheOv$overlap$I,'numeric')

  expect_is(TestNicheOv$USE,'numeric')
  expect_equal(length(TestNicheOv$USE),3)

   expect_is(TestNicheOv$equiv,'list')
   expect_equal(length(TestNicheOv$equiv),7)

   expect_is(TestNicheOv$simil,'list')
   expect_equal(length(TestNicheOv$simil),7)
   ###number of iterations for tests matches
   expect_equal(length(TestNicheOv$equiv$sim$D),iter)
   expect_equal(length(TestNicheOv$simil$sim$D),iter)
  })

  ##Function has no warning or error messages
