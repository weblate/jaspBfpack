context("Other: regression_EVS_Germany")

# Built from the JASP example file in tests/testthat/jaspfiles/other/.
# Expected values are the analysis output at the time the test was written.

results <- runJaspFileAnalysis("regression_EVS_Germany.jasp")

test_that("Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Intercept", 0.96078685697917, 0.0347450287276315, 0.00446811429319888,
       "education", 1.00142221183054e-05, 0.99998997064894, 1.51289417904193e-08,
       "income", 0.969644905066915, 0.025120355455502, 0.00523473947758314,
       "gender1", 0.919557171580768, 0.00351926264397938, 0.0769235657752529,
       "class", 0.0608169512273734, 0.939051647675584, 0.000131401097042406
      )
  )
})

test_that("Manual Hypotheses Legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("class&gt;education&gt;income&gt;0", "H1", "education&gt;(class,income)&gt;0",
       "H2", "class=education=income&gt;0", "H3", "complement", "H4"
      )
  )
})

test_that("Evidence Matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.311864172102654, 1.05933919457562, 49.5258709916315, "H1",
       3.20652415203, 1, 3.39679671259875, 158.805901484989, "H2",
       0.943984707750387, 0.29439500936014, 1, 46.7516648541187, "H3",
       0.0201914672064823, 0.00629699520388748, 0.021389612607815,
       1, "H4")
  )
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.193397400112478, "H2", 0.62013343440047, "H3", 0.182564188224862,
       "H4", 0.00390497726219003)
  )
})

