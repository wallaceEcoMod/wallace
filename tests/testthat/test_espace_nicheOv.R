#### COMPONENT espace: Do analysis in environmental space.
#### MODULE: espace
context("espace_nicheOv")

source("test_helper_functions.R")




espace_nicheOv <- function(z1, z2, iter = 100, equivalency = FALSE,
                           similarity = TRUE, logger = NULL) {
  ##Set parameters
  z1 and z2 come from espace_occDens that comes from espace_pca
  ##Run function

  espace_nicheOv should have multiple entries , shoenersD "overlap",
  nfilling, stability, expansion indices (Guisan et al. 2014 TREE)"USE"
  equiv and simil if TRUE
  ### test output features
  test_that("output checks", {

  })

  ##Function has no warning or error messages
