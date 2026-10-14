# Helpers for the tests built from the .jasp files in tests/testthat/jaspfiles/.

# jaspTools does not apply inst/Upgrades.qml, so options saved by older module versions arrive with
# their old names. This mirrors the renames of Upgrades.qml (and the column types stored next to them)
# and adds options that did not exist yet.
.upgradeJaspFileOptions <- function(options) {

  renamed <- list(
    list(from = "manualHypothesisBfTable",   to = "tablesManualHypothesesComputationBfs"),
    list(from = "standardHypothesisBfTable", to = "tablesStandardHypothesesViewBfs"),
    list(from = "covariates", to = "predictors", analyses = c("bfpackRegressionLinear", "bfpackRegressionLogistic")),
    list(from = "pairs",      to = "pair",       analyses = "bfpackTTestPairedSamples")
  )

  for (rename in renamed) {
    if (!is.null(rename[["analyses"]]) && !attr(options, "analysisName") %in% rename[["analyses"]])
      next

    for (suffix in c("", ".types")) {
      from <- paste0(rename[["from"]], suffix)
      to   <- paste0(rename[["to"]],   suffix)
      if (!is.null(options[[from]]) && is.null(options[[to]])) {
        options[[to]]   <- options[[from]]
        options[[from]] <- NULL
      }
    }
  }

  # before this option existed BFpack::cor_test used its default, LKJ
  if (attr(options, "analysisName") == "bfpackCorrelation" && is.null(options[["correlationSamplingMethod"]]))
    options[["correlationSamplingMethod"]] <- "LKJ"

  return(options)
}

# Runs analysis `index` stored in a .jasp file and returns the jaspTools results.
# `file` is a file name in tests/testthat/jaspfiles/other/ or a full path.
#
# Column names are NOT encoded: under jaspTools decodeColNames() is the identity, so hypotheses typed
# with the original names would never match jaspColumnN. The dataset is typed via
# encodeOptionsAndDataset() and the original names are restored afterwards.
runJaspFileAnalysis <- function(file, index = 1) {

  jaspFile   <- if (file.exists(file)) file else testthat::test_path("jaspfiles", "other", file)
  allOptions <- jaspTools::analysisOptions(jaspFile)
  if (!is.null(attr(allOptions, "analysisName")))
    allOptions <- list(allOptions)

  options  <- .upgradeJaspFileOptions(allOptions[[index]])
  analysis <- attr(options, "analysisName")
  dataset  <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  encoded <- jaspTools:::encodeOptionsAndDataset(options, dataset)
  map     <- encoded[["encodingMap"]]
  names(encoded[["dataset"]]) <- map[["original"]][match(names(encoded[["dataset"]]), map[["encoded"]])]

  set.seed(1)
  jaspTools::runAnalysis(analysis, encoded[["dataset"]], options, encodedDataset = TRUE, view = FALSE)
}
