context("Other: correlation_analysis_Ichinoseetal2019")

# Built from the JASP example file in tests/testthat/jaspfiles/other/.
# Expected values are the analysis output at the time the test was written.

results <- runJaspFileAnalysis("correlation_analysis_Ichinoseetal2019.jasp")

test_that("Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Del_with_Im_in_g1", 0.00185471240248663, 0.998036980969968, 0.000108306627545063,
       "Wmn_with_Im_in_g1", 0.0879632223676502, 0.905642397157398,
       0.00639438047495192, "Cat_with_Im_in_g1", 0.238516370261021,
       0.741204413232919, 0.0202792165060604, "Fas_with_Im_in_g1",
       0.582419047080636, 0.33595980679143, 0.0816211461279348, "Rat_with_Im_in_g1",
       0.288754970701439, 0.685378275482347, 0.0258667538162137, "Wmn_with_Del_in_g1",
       0.365901771291886, 0.598014340798984, 0.0360838879091294, "Cat_with_Del_in_g1",
       0.549520515458681, 0.38065198208676, 0.0698275024545591, "Fas_with_Del_in_g1",
       0.589878515804337, 0.323313872847114, 0.0868076113485492, "Rat_with_Del_in_g1",
       0.39734294372603, 0.561095397396808, 0.0415616588771618, "Cat_with_Wmn_in_g1",
       0.015223518239652, 0.983784453564231, 0.000992028196116596,
       "Fas_with_Wmn_in_g1", 0.0525155171863329, 0.943606506453921,
       0.00387797635974646, "Rat_with_Wmn_in_g1", 0.241946890778344,
       0.735492004152619, 0.0225611050690375, "Fas_with_Cat_in_g1",
       0.0500891072745779, 0.946139316233708, 0.0037715764917139, "Rat_with_Cat_in_g1",
       0.0166699468372233, 0.982222402723049, 0.00110765043972827,
       "Rat_with_Fas_in_g1", 0.0986776110653901, 0.893331092898306,
       0.00799129603630402, "Del_with_Im_in_g2", 0.455001824955785,
       0.488350113777248, 0.056648061266968, "Wmn_with_Im_in_g2", 0.649120952670265,
       0.15705497186683, 0.193824075462905, "Cat_with_Im_in_g2", 0.524818317310062,
       0.0750106163490755, 0.400171066340863, "Fas_with_Im_in_g2",
       0.601730309724747, 0.103177627242353, 0.2950920630329, "Rat_with_Im_in_g2",
       0.649805890539447, 0.199078472262211, 0.151115637198342, "Wmn_with_Del_in_g2",
       0.585897737547374, 0.0976435469991123, 0.316458715453513, "Cat_with_Del_in_g2",
       0.597152052242477, 0.297034075456529, 0.105813872300994, "Fas_with_Del_in_g2",
       0.535320522950734, 0.386463037637324, 0.0782164394119418, "Rat_with_Del_in_g2",
       0.62948120743256, 0.23601124969853, 0.13450754286891, "Cat_with_Wmn_in_g2",
       0.639817264792138, 0.153043223416353, 0.207139511791509, "Fas_with_Wmn_in_g2",
       0.638102922330636, 0.19195757172894, 0.169939505940424, "Rat_with_Wmn_in_g2",
       0.638277318522392, 0.172636888521585, 0.189085792956023, "Fas_with_Cat_in_g2",
       0.588715198426404, 0.309693886345155, 0.10159091522844, "Rat_with_Cat_in_g2",
       0.550198890889783, 0.0911210189727206, 0.358680090137496, "Rat_with_Fas_in_g2",
       0.613170483595848, 0.121296581640259, 0.265532934763893)
  )
})

test_that("Manual Hypotheses Legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Del_with_Im_in_g1&gt;Del_with_Im_in_g2&amp;Del_with_Wmn_in_g1&gt;Del_with_Wmn_in_g2&amp;Del_with_Cat_in_g1&gt;Del_with_Cat_in_g2&amp;Del_with_Fas_in_g1&gt;Del_with_Fas_in_g2&amp;Del_with_Rat_in_g1&gt;Del_with_Rat_in_g2&amp;Im_with_Wmn_in_g1&gt;Im_with_Wmn_in_g2&amp;Im_with_Cat_in_g1&gt;Im_with_Cat_in_g2&amp;Im_with_Fas_in_g1&gt;Im_with_Fas_in_g2&amp;Im_with_Rat_in_g1&gt;Im_with_Rat_in_g2&amp;Wmn_with_Cat_in_g1&gt;Wmn_with_Cat_in_g2&amp;Wmn_with_Fas_in_g1&gt;Wmn_with_Fas_in_g2&amp;Wmn_with_Rat_in_g1&gt;Wmn_with_Rat_in_g2&amp;Cat_with_Fas_in_g1&gt;Cat_with_Fas_in_g2&amp;Cat_with_Rat_in_g1&gt;Cat_with_Rat_in_g2&amp;Fas_with_Rat_in_g1&gt;Fas_with_Rat_in_g2",
       "H1", "Del_with_Im_in_g1=Del_with_Im_in_g2&amp;Del_with_Wmn_in_g1=Del_with_Wmn_in_g2&amp;Del_with_Cat_in_g1=Del_with_Cat_in_g2&amp;Del_with_Fas_in_g1=Del_with_Fas_in_g2&amp;Del_with_Rat_in_g1=Del_with_Rat_in_g2&amp;Im_with_Wmn_in_g1=Im_with_Wmn_in_g2&amp;Im_with_Cat_in_g1=Im_with_Cat_in_g2&amp;Im_with_Fas_in_g1=Im_with_Fas_in_g2&amp;Im_with_Rat_in_g1=Im_with_Rat_in_g2&amp;Wmn_with_Cat_in_g1=Wmn_with_Cat_in_g2&amp;Wmn_with_Fas_in_g1=Wmn_with_Fas_in_g2&amp;Wmn_with_Rat_in_g1=Wmn_with_Rat_in_g2&amp;Cat_with_Fas_in_g1=Cat_with_Fas_in_g2&amp;Cat_with_Rat_in_g1=Cat_with_Rat_in_g2&amp;Fas_with_Rat_in_g1=Fas_with_Rat_in_g2",
       "H2", "complement", "H3")
  )
})

test_that("Evidence Matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 1149249.84191584, 5778.57423239658, "H1", 8.70132814926443e-07,
       1, 0.00502812706309664, "H2", 0.000173053068072341, 198.881211125189,
       1, "H3")
  )
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", 0.999826107043132, "H2", 8.69981504958388e-07, "H3", 0.000173022975362639
      )
  )
})

