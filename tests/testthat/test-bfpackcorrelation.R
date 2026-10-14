context("BFpack Correlation")

options <- list(
  bfType = "fractional",
  ciLevel = 0.95,
  complement = TRUE,
  covariates = NULL,
  estimatesTable = TRUE,
  groupingVariable = "",
  interactionTerms = list(),
  iterationsEstimation = 2000,
  correlationSamplingMethod = "LD",
  nugget = .995,
  standardize = FALSE,
  logScale = TRUE,
  manualHypotheses = list(
    list(
      hypothesisText = "sales_with_adverts = airplay_with_adverts = attract_with_adverts",
      includeHypothesis = TRUE,
      priorProbManual = "1",
      value = "#"
    ),
    list(
      hypothesisText = "sales_with_adverts > airplay_with_adverts > attract_with_adverts",
      includeHypothesis = TRUE,
      priorProbManual = "1",
      value = "#2"
    )
  ),
  tablesManualHypothesesComputationBfs = TRUE,
  manualPlots = TRUE,
  muValue = 0,
  plotHeight = 320,
  plotWidth = 480,
  priorPosteriorPlot = FALSE,
  priorPosteriorPlotAdditionalEstimationInfo = TRUE,
  priorPosteriorPlotAdditionalTestingInfo = TRUE,
  priorProbComplement = "1",
  priorProbInteractionNonZero = "1",
  priorProbInteractionZero = "1",
  priorProbMainNonZero = "1",
  priorProbMainZero = "1",
  priorProbStandard = "2",
  priorProbStandard2 = "1",
  priorProbStandard3 = "1",
  seed = 1,
  setSeed = FALSE,
  tablesStandardHypothesesViewBfs = TRUE,
  traceplot = FALSE,
  variables = c("adverts", "sales", "airplay", "attract"),
  variables.types = c("scale", "scale", "scale", "ordinal"),
  variances = "equal"
)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackCorrelation", testthat::test_path("sales.csv"), options, makeTests = F)

test_that("Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("sales_with_adverts", 1.77942563859518e-20, 1, 8.92961235286475e-23,
                                      "airplay_with_adverts", 0.766948560255952, 0.211092073274922,
                                      0.0219593664691263, "attract_with_adverts", 0.813884824859506,
                                      0.159564264441074, 0.02655091069942, "airplay_with_sales", 6.41121035546808e-19,
                                      1, 3.61451462181043e-21, "attract_with_sales", 0.000120557087633689,
                                      0.999878163322489, 1.27958987778277e-06, "attract_with_airplay",
                                      0.308603140108504, 0.685289204304595, 0.00610765558690174))
})

test_that("Posterior Probabilities plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer_postPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-probabilities")
})

test_that("Prior Probabilities plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer_priorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-probabilities")
})

test_that("Estimates Table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("sales_with_adverts", 0.477735792808419, 0.568548538908731, 0.568536812578564,
                                      0.65118339386032, "airplay_with_adverts", -0.0382877935474967,
                                      0.0900218751790515, 0.089600655323082, 0.220152171556741, "attract_with_adverts",
                                      -0.058672006706562, 0.0744079433170593, 0.0735699408188674,
                                      0.211475076254999, "airplay_with_sales", 0.487417447406021,
                                      0.583270983838333, 0.584832395647687, 0.671286339854072, "attract_with_sales",
                                      0.193386143277419, 0.316622922762359, 0.316114275737559, 0.438099554775709,
                                      "attract_with_airplay", 0.0318182003949569, 0.169842607852354,
                                      0.168460989305395, 0.306349129258222))
})

test_that("Manual Hypotheses Legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("sales_with_adverts=airplay_with_adverts=attract_with_adverts",
                                      "H1", "sales_with_adverts&gt;airplay_with_adverts&gt;attract_with_adverts",
                                      "H2", "complement", "H3"))
})

test_that("Evidence Matrix (log BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, -47.581294238802, -45.683831705795, "H1", 47.581294238802,
                                      0, 1.89746253300694, "H2", 45.683831705795, -1.89746253300694,
                                      0, "H3"))
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 1.88377067290424e-21, "H2", 0.869604064811951, "H3", 0.130395935188049
                                 ))
})

test_that("Log BFs: Manual Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-46.3490037655396, -46.3490037655396, 0, -47.7210115076413, -1.09475535702376,
                                      0, -47.4437591225633, 0, "H1", 1.2322904732624, 0, 1.2322904732624,
                                      -0.139717268839292, 0, -1.79175946922805, 0, -0.559468995965659,
                                      "H2", -0.665172059744543, 0, -0.665172059744543, -2.03717980184623,
                                      0, -0.182321556793955, 0, -0.847493616538498, "H3"))
})

test_that("Log Standard Hypotheses: View BFs table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_stdBfTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(46.1685584031542, 50.7700841544505, 0, "sales_with_adverts",
                                      0, 2.86007878548516, 0.596978147871284, "airplay_with_adverts",
                                      0, 2.72960763423386, 0.936224929695982, "attract_with_adverts",
                                      42.584215871333, 47.0693293741044, 0, "airplay_with_sales",
                                      9.71641249738419, 13.5688490952563, 0, "attract_with_sales",
                                      1.49093201057489, 4.72029794667427, 0, "attract_with_airplay"
                                 ))
})

# check posterior distribution plots
options <- list(
  bfType = "fractional",
  ciLevel = 0.95,
  standardize = FALSE,
  complement = TRUE,
  covariates = list(
    types = list(),
    value = list()
  ),
  estimatesTable = FALSE,
  groupingVariable = "facGender",
  interactionTerms = list(),
  iterationsEstimation = 2000,
  correlationSamplingMethod = "LD",
  logScale = FALSE,
  manualHypotheses = list(
    list(
      hypothesisText = "",
      includeHypothesis = FALSE,
      priorProbManual = "1",
      value = "#"
    )
  ),
  manualPlots = FALSE,
  muValue = 0,
  nugget = .995,
  plotHeight = 320,
  plotWidth = 480,
  priorPosteriorPlot = TRUE,
  priorPosteriorPlotAdditionalEstimationInfo = TRUE,
  priorPosteriorPlotAdditionalTestingInfo = TRUE,
  priorProbComplement = "1",
  priorProbInteractionNonZero = "1",
  priorProbInteractionZero = "1",
  priorProbMainNonZero = "1",
  priorProbMainZero = "1",
  priorProbStandard = "2",
  priorProbStandard2 = "1",
  priorProbStandard3 = "1",
  seed = 100,
  tablesManualHypothesesComputationBfs = FALSE,
  tablesStandardHypothesesViewBfs = FALSE,
  traceplot = TRUE,
  variables =  c("contNormal", "contGamma", "contExpon")
  )

set.seed(1)
results <- jaspTools::runAnalysis("bfpackCorrelation", "debug.csv", options, makeTests = F)

test_that("contGamma_with_contNormal_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_in_g1")
})

test_that("contExpon_with_contNormal_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_in_g1")
})

test_that("contExpon_with_contGamma_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_in_g1")
})

test_that("contGamma_with_contNormal_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor4"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_in_g2")
})

test_that("contExpon_with_contNormal_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_in_g2")
})

test_that("contExpon_with_contGamma_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor6"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_in_g2")
})

test_that("contGamma_with_contNormal_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_in_g1-trace")
})

test_that("contExpon_with_contNormal_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_in_g1-trace")
})

test_that("contExpon_with_contGamma_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_in_g1-trace")
})

test_that("contGamma_with_contNormal_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor4"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_in_g2-trace")
})

test_that("contExpon_with_contNormal_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_in_g2-trace")
})

test_that("contExpon_with_contGamma_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor6"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_in_g2-trace")
})

# validation: the grouping variable may have more than two levels, also when the first variable is nominal
options$priorPosteriorPlot <- FALSE
options$traceplot <- FALSE
options$covariates <- list()
options$variables <- c("contBinom", "contNormal")
options$variables.types <- c("nominal", "scale")
options$groupingVariable <- "facFive"
options$groupingVariable.types <- "nominal"

# typed by hand so that the test does not depend on how jaspTools finds the variable types
dataset <- jaspTools:::loadCorrectDataset("debug.csv")[, c("contBinom", "contNormal", "facFive")]
dataset$contBinom <- as.factor(dataset$contBinom)
dataset$facFive   <- as.factor(dataset$facFive)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackCorrelation", dataset, options, encodedDataset = TRUE)

test_that("Grouping variable with more than two levels is accepted", {
  expect_identical(results[["status"]], "complete")
})

test_that("Posterior Probabilities of the Standard Hypotheses table with five groups matches", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal_with_contBinom_in_g1", 0.74047206261738, 0.102327511270414,
                                      0.157200426112206, "contNormal_with_contBinom_in_g2", 0.72216835145521,
                                      0.193334274845392, 0.084497373699397, "contNormal_with_contBinom_in_g3",
                                      0.672879793473952, 0.0667743189101171, 0.260345887615931, "contNormal_with_contBinom_in_g4",
                                      0.73113876915779, 0.176993991458316, 0.0918672393838935, "contNormal_with_contBinom_in_g5",
                                      0.632276301291696, 0.0706055141787461, 0.297118184529558))
})

test_that("A nominal variable with more than two levels is still rejected", {
  options$variables <- c("facFive", "contNormal")
  dataset <- jaspTools:::loadCorrectDataset("debug.csv")[, c("facFive", "contNormal", "facGender")]
  dataset$facFive   <- as.factor(dataset$facFive)
  dataset$facGender <- as.factor(dataset$facGender)
  options$groupingVariable <- "facGender"

  results <- jaspTools::runAnalysis("bfpackCorrelation", dataset, options, encodedDataset = TRUE)

  expect_identical(results[["status"]], "validationError")
  expect_match(results[["results"]][["errorMessage"]], "facFive", fixed = TRUE)
})
