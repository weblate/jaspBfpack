context("Other: multivariate_Ranehilletal2015")

# Built from the JASP example file in tests/testthat/jaspfiles/other/.
# Expected values are the analysis output at the time the test was written.

results <- runJaspFileAnalysis("multivariate_Ranehilletal2015.jasp")

test_that("Posterior Probabilities for Main Effects table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_mainEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("grp", 0.00867197806667229, 0.991328021933328)
  )
})

test_that("Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("grphigh_on_powerful", 1.67547468240577e-73, 1, 4.69829426365861e-76,
       "grplow_on_powerful", 1.4098009452998e-61, 1, 4.10233543770317e-64,
       "grphigh_on_risk", 1.17719306799794e-52, 1, 3.44506703005704e-55,
       "grplow_on_risk", 1.0123774571208e-53, 1, 3.0115925399056e-56,
       "grphigh_on_testosterone", 0.862120247366412, 0.127050648678149,
       0.0108291039554391, "grplow_on_testosterone", 0.104727018613468,
       0.894435973200926, 0.000837008185605938, "grphigh_on_cortisol",
       0.918210847016873, 0.0675167283187099, 0.0142724246644167, "grplow_on_cortisol",
       0.579885420279322, 0.414259552445405, 0.00585502727527334)
  )
})

test_that("Manual Hypotheses Legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("grphigh_on_powerful&gt;grplow_on_powerful&amp;grphigh_on_risk&gt;grplow_on_risk&amp;grphigh_on_testosterone&gt;grplow_on_testosterone&amp;grphigh_on_cortisol&lt;grplow_on_cortisol",
       "H1", "grphigh_on_powerful=grplow_on_powerful&amp;grphigh_on_risk=grplow_on_risk&amp;grphigh_on_testosterone=grplow_on_testosterone&amp;grphigh_on_cortisol=grplow_on_cortisol",
       "H2", "complement", "H3")
  )
})

test_that("Evidence Matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.00146802396269663, 0.159032527746518, "H1", 681.187790806279,
       1, 108.331016241989, "H2", 6.28802179132748, 0.00923096666762737,
       1, "H3")
  )
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.00145248385157791, "H2", 0.989414266038149, "H3", 0.00913325011027315
      )
  )
})

