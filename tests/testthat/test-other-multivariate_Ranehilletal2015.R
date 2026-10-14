context("Other: multivariate_Ranehilletal2015")

# Built from the JASP example file in tests/testthat/jaspfiles/other/.

test_that("bfpackAnova results match", {

  # The ordinal column 'powerful' has empty value labels in the file.
  # jaspTools::extractDatasetFromJASPFile() returns "" for it instead of the original values (2, 3, ...),
  # so the analysis cannot be run on the real data and any expectation would be meaningless.
  skip("jaspTools::extractDatasetFromJASPFile() cannot read column 'powerful' (empty value labels)")

  results <- runJaspFileAnalysis("multivariate_Ranehilletal2015.jasp")
  expect_identical(results[["status"]], "complete")
})
