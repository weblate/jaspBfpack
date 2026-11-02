context("Other: data_meng")

# Built from the JASP example file in tests/testthat/jaspfiles/other/.
# Expected values are the analysis output at the time the test was written.

results <- runJaspFileAnalysis("data_meng.jasp")

test_that("Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("X1_with_Y", 0.154304143432187, 0.834309113427475, 0.011386743140338,
       "X2_with_Y", 0.683311923529352, 0.15570957726585, 0.160978499204797,
       "X2_with_X1", 0.64585408152254, 0.101385740473403, 0.252760178004057
      )
  )
})

test_that("Estimates Table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("X1_with_Y", 0.0744420355375988, 0.5492386314496, 0.579526208724316,
       0.849725715900406, "X2_with_Y", -0.529789016482524, -0.00609548280242769,
       -0.00609642983073659, 0.511560970646283, "X2_with_X1", -0.634879767075375,
       -0.155231218122047, -0.164965709805263, 0.392277140929692)
  )
})

test_that("Manual Hypotheses Legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Y_with_X1=Y_with_X2", "H1", "Y_with_X1&gt;Y_with_X2", "H2")
  )
})

test_that("Evidence Matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.347948560646212, "H1", 2.87398803473362, 1, "H2")
  )
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.258131927882622, "H2", 0.741868072117378)
  )
})

