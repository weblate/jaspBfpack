# Helpers for the tests built from the .jasp files in tests/testthat/jaspfiles/.

# Options saved by older module versions use names that were renamed later and lack options that
# were added since. Map them to the current names/defaults so the saved settings are still tested.
.upgradeJaspFileOptions <- function(options) {

  renamed <- c(
    manualHypothesisBfTable   = "tablesManualHypothesesComputationBfs",
    standardHypothesisBfTable = "tablesStandardHypothesesViewBfs"
  )

  for (old in intersect(names(renamed), names(options))) {
    options[[renamed[[old]]]] <- options[[old]]
    options[[old]]            <- NULL
  }

  # before this option existed BFpack::cor_test used its default, LKJ
  if (is.null(options[["correlationSamplingMethod"]]))
    options[["correlationSamplingMethod"]] <- "LKJ"

  return(options)
}

# Runs analysis `index` stored in a .jasp file from tests/testthat/jaspfiles/other/.
#
# Column names are NOT encoded: under jaspTools decodeColNames() is the identity, so hypotheses typed
# with the original names would never match jaspColumnN. The dataset is typed via
# encodeOptionsAndDataset() and the original names are restored afterwards.
runJaspFileAnalysis <- function(file, index = 1) {

  jaspFile   <- testthat::test_path("jaspfiles", "other", file)
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
  jaspTools::runAnalysis(analysis, encoded[["dataset"]], options, encodedDataset = TRUE)
}
