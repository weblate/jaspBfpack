context("Other: bmi_with_pa")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("bfpackCorrelation (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "bmi_with_pa.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("bfpackCorrelation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("bfpackCorrelation (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "bmi_with_pa.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("bfpackCorrelation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("bfpackCorrelation (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "bmi_with_pa.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("bfpackCorrelation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("bfpackCorrelation (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "bmi_with_pa.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("bfpackCorrelation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("bfpackCorrelation (analysis 5) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "bmi_with_pa.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[5]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("bfpackCorrelation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("bfpackCorrelation (analysis 6) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "bmi_with_pa.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[6]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("bfpackCorrelation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("bfpackCorrelation (analysis 7) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "bmi_with_pa.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[7]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("bfpackCorrelation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

