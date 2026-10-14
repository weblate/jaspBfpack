context("Other: bmi_with_pa")

# Built from the JASP example file in tests/testthat/jaspfiles/other/.
# Expected values are the analysis output at the time the test was written.

results1 <- runJaspFileAnalysis("bmi_with_pa.jasp", 1)
results2 <- runJaspFileAnalysis("bmi_with_pa.jasp", 2)
results3 <- runJaspFileAnalysis("bmi_with_pa.jasp", 3)
results4 <- runJaspFileAnalysis("bmi_with_pa.jasp", 4)
results5 <- runJaspFileAnalysis("bmi_with_pa.jasp", 5)
results6 <- runJaspFileAnalysis("bmi_with_pa.jasp", 6)
results7 <- runJaspFileAnalysis("bmi_with_pa.jasp", 7)

test_that("Analysis 1: Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results1[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_with_bmi", 0.178922071502165, 0.00485702968125373, 0.816220898816582
      )
  )
})

test_that("Analysis 1: Estimates Table results match", {
  table <- results1[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_with_bmi", -0.601655753769678, -0.366383237455825, -0.374472493035487,
       -0.0880717733839792)
  )
})

test_that("Analysis 1: Manual Hypotheses Legend table results match", {
  table <- results1[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_with_bmi=0", "H1", "complement", "H2")
  )
})

test_that("Analysis 1: Evidence Matrix (BFs) table results match", {
  table <- results1[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.21791119367866, "H1", 4.58902538744585, 1, "H2")
  )
})

test_that("Analysis 1: Posterior Model Probability table results match", {
  table <- results1[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.178922071502165, "H2", 0.821077928497835)
  )
})

test_that("Analysis 1: Standard Hypotheses: View BFs table results match", {
  table <- results1[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_stdBfTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9.12375865049949, "", 168.049394873348, "pa_with_bmi")
  )
})

test_that("Analysis 2: Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results2[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("bmi_ord_with_pa", 0.45600833978461, 0.0183007000170391, 0.525690960198351
      )
  )
})

test_that("Analysis 2: Manual Hypotheses Legend table results match", {
  table <- results2[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_with_bmi_ord=0", "H1", "complement", "H2")
  )
})

test_that("Analysis 2: Evidence Matrix (BFs) table results match", {
  table <- results2[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.838263475590886, "H1", 1.19294234941479, 1, "H2")
  )
})

test_that("Analysis 2: Posterior Model Probability table results match", {
  table <- results2[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.45600833978461, "H2", 0.54399166021539)
  )
})

test_that("Analysis 3: Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results3[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("bmi_bin_with_pa", 0.166707446513091, 0.00576518385678089, 0.827527369630128
      )
  )
})

test_that("Analysis 3: Manual Hypotheses Legend table results match", {
  table <- results3[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_with_bmi_bin=0", "H1", "complement", "H2")
  )
})

test_that("Analysis 3: Evidence Matrix (BFs) table results match", {
  table <- results3[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.200058725852648, "H1", 4.99853228464798, 1, "H2")
  )
})

test_that("Analysis 3: Posterior Model Probability table results match", {
  table <- results3[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.166707446513091, "H2", 0.833292553486909)
  )
})

test_that("Analysis 4: Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results4[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_ord_with_bmi_ord", 0.574602040445391, 0.027527855009241, 0.397870104545368
      )
  )
})

test_that("Analysis 4: Manual Hypotheses Legend table results match", {
  table <- results4[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_ord_with_bmi_ord=0", "H1", "complement", "H2")
  )
})

test_that("Analysis 4: Evidence Matrix (BFs) table results match", {
  table <- results4[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 1.35074000130842, "H1", 0.740334926804073, 1, "H2")
  )
})

test_that("Analysis 4: Posterior Model Probability table results match", {
  table <- results4[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.57460204044539, "H2", 0.425397959554609)
  )
})

test_that("Analysis 5: Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results5[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("bmi_bin_with_pa_ord", 0.284046234181622, 0.0114547240567392,
       0.704499041761639)
  )
})

test_that("Analysis 5: Manual Hypotheses Legend table results match", {
  table <- results5[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_ord_with_bmi_bin=0", "H1", "complement", "H2")
  )
})

test_that("Analysis 5: Evidence Matrix (BFs) table results match", {
  table <- results5[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.396738236102355, "H1", 2.52055362705703, 1, "H2")
  )
})

test_that("Analysis 5: Posterior Model Probability table results match", {
  table <- results5[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.284046234181622, "H2", 0.715953765818378)
  )
})

test_that("Analysis 6: Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results6[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_bin_with_bmi_bin", 0.59546536561959, 0.0415513214337021, 0.362983312946708
      )
  )
})

test_that("Analysis 6: Manual Hypotheses Legend table results match", {
  table <- results6[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_bin_with_bmi_bin=0", "H1", "complement", "H2")
  )
})

test_that("Analysis 6: Evidence Matrix (BFs) table results match", {
  table <- results6[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 1.47197622901093, "H1", 0.679358796895745, 1, "H2")
  )
})

test_that("Analysis 6: Posterior Model Probability table results match", {
  table <- results6[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.59546536561959, "H2", 0.40453463438041)
  )
})

test_that("Analysis 7: Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results7[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_with_bmi", 0.730335462702902, 0.0391749252524927, 0.230489612044605
      )
  )
})

test_that("Analysis 7: Manual Hypotheses Legend table results match", {
  table <- results7[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("pa_with_bmi=0", "H1", "complement", "H2")
  )
})

test_that("Analysis 7: Evidence Matrix (BFs) table results match", {
  table <- results7[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 2.70831111136526, "H1", 0.369233798806777, 1, "H2")
  )
})

test_that("Analysis 7: Posterior Model Probability table results match", {
  table <- results7[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.730335462702902, "H2", 0.269664537297098)
  )
})

