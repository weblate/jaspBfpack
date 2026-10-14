context("Other: data_neuro")

# Built from the JASP example file in tests/testthat/jaspfiles/other/.
# Expected values are the analysis output at the time the test was written.

results <- runJaspFileAnalysis("data_neuro.jasp")

test_that("Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Strength_with_LBA_in_g1", 0.0544519461657131, 0.942413876574864,
       0.0031341772594229, "Strength_with_LBA_in_g2", 0.797416836293041,
       0.0674659946071406, 0.135117169099818)
  )
})

test_that("Manual Hypotheses Legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Strength_with_LBA_in_g1=Strength_with_LBA_in_g2", "H1", "complement",
       "H2")
  )
})

test_that("Evidence Matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.0976016063012501, "H1", 10.2457330150231, 1, "H2")
  )
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.0889226161304124, "H2", 0.911077383869588)
  )
})

