context("BFpack Logistic Regression")

options <-
  list(
    bfType = "adjusted",
    ciLevel = 0.95,
    estimatesTable = TRUE,
    complement = TRUE,
    predictors = c("contNormal", "contcor1"),
    dependent = "contBinom",
    interactionTerms = list(
      list(
        includeInteractionEffect = TRUE,
        value = "contNormal:contcor1"
      )
    ),
    iterationsEstimation = 5000,
    logScale = FALSE,
    manualHypotheses = list(
      list(
        hypothesisText = "contNormal:contcor1>-.5",
        priorProbManual = "1/2",
        includeHypothesis = TRUE,
        value = "#"
      )
    ),
    manualPlots = FALSE,
    priorProbComplement = "1/2",
    seed = 100,
    tablesManualHypothesesComputationBfs = FALSE,
    priorProbStandard = "1",
    priorProbStandard2 = "1",
    priorProbStandard3 = "1",
    tablesStandardHypothesesViewBfs = FALSE,
    excludeIntercept = FALSE,
    standardize = FALSE
  )

debug <- read.csv("https://raw.githubusercontent.com/jasp-stats/jasp-desktop/development/Resources/Data%20Sets/debug.csv")
dt <- debug[, c("contNormal", "contcor1", "contBinom")]
dt$contBinom <- as.factor(dt$contBinom)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackRegressionLogistic", dt, options)

test_that("Coefficients table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", -0.727543388672256, -0.298554865863755, -0.298554865863755,
                                      0.130433656944746, "contNormal", -0.497926596040128, -0.0668697011221928,
                                      -0.0668697011221928, 0.364187193795742, "contcor1", -0.891660779964622,
                                      -0.436993245210254, -0.436993245210254, 0.0176742895441145,
                                      "contNormal:contcor1", -0.913811579222416, -0.414818496726082,
                                      -0.414818496726082, 0.0841745857702528))
})

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal:contcor1&gt;-.5", "H1", "complement", "H2"
                                 ))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 0.663546200745916, 0.0290284776394466, 0.307425321614638,
                                      "contNormal", 0.82681422421017, 0.065905062030784, 0.107280713759046,
                                      "contcor1", 0.45887687439431, 0.0161242542570045, 0.524998871348686,
                                      "contNormal:contcor1", 0.570052104657479, 0.0221941456995523,
                                      0.407753749642969))
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1.71023797194578, "H1", 0.584713949990408, 1, "H2"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 0.631, "H2", 0.369))
})


# ordinal logistic regression
options <-
  list(
    bfType = "adjusted",
    ciLevel = 0.95,
    estimatesTable = TRUE,
    complement = TRUE,
    predictors = c("contNormal", "contcor1"),
    dependent = "facFive",
    interactionTerms = list(
      list(
        includeInteractionEffect = FALSE,
        value = "contNormal:contcor1"
      )
    ),
    iterationsEstimation = 5000,
    logScale = FALSE,
    manualHypotheses = list(
      list(
        hypothesisText = "",
        priorProbManual = "1/2",
        includeHypothesis = FALSE,
        value = "#"
      )
    ),
    manualPlots = FALSE,
    priorProbComplement = "1/2",
    seed = 100,
    tablesManualHypothesesComputationBfs = FALSE,
    priorProbStandard = "1",
    priorProbStandard2 = "1",
    priorProbStandard3 = "1",
    tablesStandardHypothesesViewBfs = FALSE,
    excludeIntercept = FALSE,
    standardize = FALSE
  )

debug <- read.csv("https://raw.githubusercontent.com/jasp-stats/jasp-desktop/development/Resources/Data%20Sets/debug.csv")
dt <- debug[, c("contNormal", "contcor1", "facFive")]
dt$facFive <- as.ordered(dt$facFive)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackRegressionLogistic", dt, options, makeTests = F)

test_that("Posterior Probabilities Testing Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal", 0.779731795276041, 0.175560655299559, 0.0447075494244006,
                                      "contcor1", 0.724337721926559, 0.240287291476167, 0.035374986597274,
                                      "1|2", 6.91628081610565e-07, 9.53357227078238e-09, 0.999999298838346,
                                      "2|3", 0.351077417991326, 0.0113407662232625, 0.637581815785411,
                                      "3|4", 0.457662970558089, 0.526271554833832, 0.0160654746080791,
                                      "4|5", 1.47910426578437e-06, 0.99999850002731, 2.08684242187709e-08
                                 ))
})

test_that("Estimates Table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal", -0.192585964825031, 0.141776347312788, 0.141776347312788,
                                      0.476138659450607, "contcor1", -0.148890844459348, 0.2045612434167,
                                      0.2045612434167, 0.558013331292749, "1|2", -1.92748288886908,
                                      -1.42910742793203, -1.42910742793203, -0.930731966994982, "2|3",
                                      -0.843460433206645, -0.437167867388325, -0.437167867388325,
                                      -0.0308753015700043, "3|4", -0.0152231933603283, 0.390192870898498,
                                      0.390192870898498, 0.795608935157324, "4|5", 0.893167735353396,
                                      1.39001406872788, 1.39001406872788, 1.88686040210237))
})
