############ BASICS ##########

# Clean the input for the order constraints
.bfpackCleanModelInput <- function(input) {
  return(gsub("\n+", ";", input))
}

# Add the BFpack citations
.bfpackGetCitations <- function() {
  citations <- c(
    "Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. Journal of Statistical Software, 100(18), 1-63. https://doi.org/10.18637/jss.v100.i18",
  )
  return(citations)
}

# Create a container for the results
.bfpackCreateContainer <- function(jaspResults, deps) {
  if (is.null(jaspResults[["bfpackContainer"]])) {
    jaspResults[["bfpackContainer"]] <- createJaspContainer()
    jaspResults[["bfpackContainer"]]$dependOn(options = deps)
    jaspResults[["bfpackContainer"]]$position <- 1
  }
  return(jaspResults[["bfpackContainer"]])
}


# handle listwise deletion and standardization
.bfpackHandleData <- function(dataset, options, type = "", ready) {

  if (!ready) return()

  if (type == "correlation") {
    variables <- unlist(options[["variables"]])
    dataset   <- dataset[, c(variables, setdiff(colnames(dataset), variables))]
  }

  dataset <- excludeNaListwise(dataset)

  if (options[["standardize"]]) {
    cindex <- which(sapply(dataset, is.numeric))

    if (type == "tTestOneSample") {
      dataset[, cindex] <- scale(dataset[, cindex], center = FALSE)
    } else {
      dataset[, cindex] <- scale(dataset[, cindex])
    }
  }

  return(dataset)
}


.bfpackUnwrapInteractions <- function(options) {

  iaTerms <- options[["interactionTerms"]]
  iaTrue <- lapply(iaTerms, function(x) {
    if (x[["includeInteractionEffect"]]) x[["value"]] else NULL
  })

  iaTrue <- unlist(iaTrue[which(!sapply(iaTrue, is.null))])
  if (length(iaTrue) > 0) {
    iaString <- paste0(iaTrue, collapse = "+")
  } else {
    iaString <- NULL
  }
  return(iaString)
}

# function from Joris (BFpack) to get variances interval bounds
.bfpackBartlettHelper <- function(x, ciLevel) {
  # Step 1: Obtain parameter estimates
  get_est <- BFpack:::get_estimates.bartlett_htest(x)
  # Step 3: Extract estimated parameter (possibly variance)
  s2 <- get_est$estimate
  # Step 4: Extract sample sizes
  n <- c(x$n)
  # Step 5: Calculate values of 'b'
  b <- 2 / n
  # Step 6: Assign the length of 'n' to 'J'
  J <- length(n)
  # Step 7: Extract names of coefficients
  names_coef <- names(get_est$estimate)
  # Step 8: Calculate scale parameter for the inverse gamma distribution
  scale.post_group <- s2 * (n - 1) / 2
  # Step 9: Calculate shape parameter for the inverse gamma distribution
  shape.post_group <- (n - 1) / 2
  # Step 10: Create a matrix to store posterior estimates
  lower <- (1 - ciLevel) / 2
  upper <- 1 - lower
  postestimates <- cbind(
    NA,
    extraDistr::qinvgamma(0.5, alpha = shape.post_group, beta = scale.post_group),
    extraDistr::qinvgamma(lower, alpha = shape.post_group, beta = scale.post_group),
    extraDistr::qinvgamma(upper, alpha = shape.post_group, beta = scale.post_group)
  )
  # Step 11: Identify groups with shape parameters greater than 1
  which.means <- which(shape.post_group > 1)
  # Step 12: Calculate mean for groups with shape parameters > 1
  postestimates[which.means, 1] <- scale.post_group[which.means] / (shape.post_group[which.means] - 1)
  # Step 13: Assign coefficient names as row names
  row.names(postestimates) <- names_coef
  # Step 14: Assign column names to the matrix
  colnames(postestimates) <- c("mean", "median", "lower", "upper")
  postestimates <- postestimates[, c("lower", "upper")]
  # Print the resulting matrix
  return(postestimates)

}

# another function from Joris to get the ciLevel bounds for the mv t-test
.bfpackMultiTTestHelper <- function(x, ciLevel) {
  x.lm <- x
  class(x.lm) <- c("mlm","lm")
  P <- ncol(x.lm$residuals)
  N <- nrow(x.lm$residuals)
  K <- length(x.lm$coefficients)/P
  Xmat <- model.matrix(x.lm)
  Ymat <- model.matrix(x.lm)%*%x.lm$coefficients + x.lm$residuals
  tXXi <- solve(t(Xmat)%*%Xmat)
  BetaHat <- tXXi%*%t(Xmat)%*%Ymat
  S <- t(Ymat - Xmat%*%BetaHat)%*%(Ymat - Xmat%*%BetaHat)
  dfN <- N-K-P+1
  ScaleN <- kronecker(S,tXXi)/(N-K-P+1)
  meanN <- as.matrix(c(BetaHat))
  names_coef1 <- row.names(x$coefficients)
  names_coef2 <- colnames(x$coefficients)
  names_coef <- unlist(lapply(1:P,function(p){
    lapply(1:K,function(k){
      paste0(names_coef1[k],"_on_",names_coef2[p])
    })
  }))
  row.names(meanN) <- names_coef
  lower <- (1 - ciLevel) / 2
  upper <- 1 - lower
  postestimates <- cbind(meanN,meanN,
                         t(matrix(unlist(lapply(1:length(meanN),function(coef){
                           ub <- qt(p=upper,df=dfN)*sqrt(ScaleN[coef,coef])+meanN[coef,1]
                           lb <- qt(p=lower,df=dfN)*sqrt(ScaleN[coef,coef])+meanN[coef,1]
                           return(c(lb,ub))
                         })),nrow=2))
  )
  row.names(postestimates) <- names_coef
  colnames(postestimates) <- c("mean", "median", "lower", "upper")
  postestimates <- postestimates[, c("lower", "upper")]
  return(postestimates)
}

####### CHECKS #######
# this function needs updating when there is a new analysis added
# Check if current options allow for analysis
.bfpackOptionsReady <- function(options, type) {

  ready <- switch(type,
    "tTestIndependentSamples" = options[["variables"]] != "" && options[["groupingVariable"]] != "",
    "tTestPairedSamples" = sum(unlist(options[["pair"]]) != "") > 1, # only allow 2 variables
    "tTestOneSample" = options[["variables"]] != "",
    "anova" = length(unlist(options[["dependent"]])) > 0 && length(unlist(options[["fixedFactors"]])) > 0,
    "regression" = sum(unlist(options[["dependent"]]) != "") > 0 && length(unlist(options[["predictors"]])) > 0,
    "correlation" = length(unlist(options[["variables"]])) > 1,
    "variances" = options[["variables"]] != "" && options[["groupingVariable"]] != "",
    "regressionLogistic" = options[["dependent"]] != "" && length(unlist(options[["predictors"]])) > 0,
    "tTestMultiSamples" = length(unlist(options[["variables"]])) > 1
    )

  return(ready)
}

# this function needs updating when there is a new analysis added
# Check if current data allow for analysis
.bfpackDataReady <- function(dataset, options, type, ready) {

  if (!ready) return()

  # the grouping variable is not checked here: it may have any number of levels (correlation and variances use
  # all of them, the independent samples t-test uses the first two and adds a footnote)
  vars <- c(options[["variables"]],
            options[["dependent"]],
            options[["fixedFactors"]],
            options[["covariates"]],
            options[["predictors"]],
            unlist(options[["pair"]]))
  vars <- vars[vars != ""]
  varsTypes <- c(options[["variables.types"]],
                 options[["dependent.types"]],
                 options[["fixedFactors.types"]],
                 options[["covariates.types"]],
                 options[["predictors.types"]],
                 unlist(options[["pair.types"]]))

  factors <- vars[varsTypes == "nominal"]
  orders <- vars[varsTypes == "ordinal"]
  scales <- vars[varsTypes == "scale"]
  if (length(factors) > 0) {
    .hasErrors(dataset,
               type = "factorLevels",
               factorLevels.target = factors,
               factorLevels.amount = '!= 2',
               exitAnalysisIfErrors = TRUE
    )

    levs <- lapply(factors, function(v) levels(as.factor(dataset[[v]])))
    hasSpaces <- any(grepl("\\s", unlist(levs), perl = TRUE))
    if (hasSpaces) {
      jaspBase:::.quitAnalysis(gettext("BFpack does not accept factor levels that contain spaces. Please remove the spaces from your factor levels to continue."))
    }

  }

  nonfactors <- c(orders, scales)
  if (length(nonfactors) > 0) {
    if (type == "regression") {
      ttpy <- c("infinity", "variance", "observations", "varCovData")
    } else {
      ttpy <- c("infinity", "variance", "observations")
    }
    .hasErrors(dataset,
      type = ttpy,
      all.target = nonfactors, observations.amount = "< 3",
      varCovData.corFun = stats::cov,
      exitAnalysisIfErrors = TRUE
    )
  }

 if (type == "tTestPairedSamples" && sum(unlist(options[["pair"]]) != "") > 2) {
    jaspBase:::.quitAnalysis(gettext("Only two variables can be specified for the paired samples t-test."))
  }

  return()
}

###### COMPUTE RESULTS ######
# this function needs updating when there is a new analysis added
# perform the parameter estimation and also return the estimates to the JASP GUI
.bfpackGetParameterEstimates <- function(dataset, options, bfpackContainer, ready, type, jaspResults) {

  if (!is.null(bfpackContainer[["estimatesState"]])) {
    return()
  }

  if (!ready) return()

  # decode the colnames otherwise bfpack fails when trying to match hypotheses and estimate names
  colnames(dataset) <- decodeColNames(colnames(dataset))

  if (bfpackContainer$getError()) {
    return()
  }

  callString <- switch(type,
                       "tTestIndependentSamples" = "BFpack:::get_estimates.t_test",
                       "tTestPairedSamples" = "BFpack:::get_estimates.t_test",
                       "tTestOneSample" = "BFpack:::get_estimates.t_test",
                       "anova" = "bain::get_estimates",
                       "regression" = "BFpack:::get_estimates.lm",
                       "correlation" = "BFpack:::get_estimates.cor_test",
                       "variances" = "BFpack:::get_estimates.bartlett_htest",
                       "tTestMultiSamples" = "BFpack:::get_estimates.mvt_test",
                       NULL
                       )

  # special case logistic regression
  if (type == "regressionLogistic") {
    if (is.ordered(dataset[, decodeColNames(options[["dependent"]])])) {
      callString <- "BFpack:::get_estimates.polr"
      polr <- TRUE
    } else {
      callString <- "BFpack:::get_estimates.glm"
      polr <- FALSE
    }
  }


  # special dependency ciLevel, because for some cor, reg, aov when the ciLevel is changed
  # we can just use the fitted object and change the interval, but for t-test we need to change it
  # in the t-test call (or at least it is easiest)
  deps <- switch(type,
                 "tTestIndependentSamples" = c("ciLevel", "muValue", "variances"),
                 "tTestPairedSamples" = c("ciLevel", "muValue"),
                 "tTestOneSample" = c("ciLevel", "muValue"),
                 "anova" = c("interactionTerms", "includeInteractionEffect"),
                 "regression" = c("interactionTerms", "includeInteractionEffect"),
                 "regressionLogistic"= c("interactionTerms", "includeInteractionEffect"),
                 "tTestMultiSamples" = "testValues",
                 NULL)

  .setSeedJASP(options)
  # estimate the correlation
  if (type == "correlation") {

    covariates <- unlist(options[["covariates"]])
    covariates <- decodeColNames(covariates)

    if (length(covariates) > 0) {
      form <- eval(parse(text = paste0("~", paste0(covariates, collapse = "+"))))
      # weirdly BFpack always requires the covariate(s) to be in the last columns
      cIndex <- which(colnames(dataset) == covariates)
      notcIndex <- which(colnames(dataset) != covariates)
      dataset <- dataset[, c(notcIndex, cIndex)]
    } else {
      form <- NULL
    }

    if (options[["groupingVariable"]] == "") {
      result <- try(BFpack::cor_test(dataset,
                                     formula = form,
                                     iter = options[["iterationsEstimation"]],
                                     nugget.scale = options[["nugget"]],
                                     method = options[["correlationSamplingMethod"]]))

    } else {
      groupName <- decodeColNames(options[["groupingVariable"]])
      levs <- levels(dataset[[groupName]])
      dataNames <- vector("character", 0L)
      if (length(covariates) > 0) {
        select <- c(decodeColNames(options[["variables"]]), covariates)
      } else {
        select <- c(decodeColNames(options[["variables"]]))
      }
      for (i in 1:length(levs)) {
        dtmp <- subset(dataset,
                       subset = eval(parse(text = groupName)) == levs[i],
                       select = select)
        # create the data frames in the environment
        assign(paste0("dtGroup", i), dtmp)
        dataNames <- c(dataNames, paste0("dtGroup", i))
      }
      dataString <- paste0(dataNames, collapse = ",")
      # this is a bit hacky, because cor_test takes multiple data frames names separated by commas,
      # but we do not know how many...
      cor_test_call <- paste("try(BFpack::cor_test(", dataString, ", formula = ", paste0(form, collapse = ""),
                             ", iter = ", options[["iterationsEstimation"]], ", nugget.scale = ", options[["nugget"]],
                             ", method = '", options[["correlationSamplingMethod"]], "'))",
                             sep = "")

      result <- eval(parse(text = cor_test_call))
    }

  # regression
  } else if (type %in%  c("regression", "regressionLogistic")) {

    dependent <- decodeColNames(unlist(options[["dependent"]]))
    predictors <- decodeColNames(unlist(options[["predictors"]]))
    ncov <- length(predictors)
    covariateString <- paste0(predictors, collapse = "+")
    # handle the interactions
    iastring <- .bfpackUnwrapInteractions(options)
    if (!is.null(iastring)) {
      covariateString <- paste0(covariateString, "+", iastring)
    }

    if (length(dependent) > 1) { # means  multivariate linear regression
      depString <- paste0("cbind(", paste0(dependent, collapse = ","), ")")
    } else {
      depString <- dependent
    }

    if (options[["excludeIntercept"]]) {
      covariateString <- paste0(covariateString, "-1")
    }

    formula <- as.formula(paste0(depString, "~", covariateString))

    if (type == "regression") {
      result <- try(lm(formula, data = dataset))
    } else if (type == "regressionLogistic") {
      if (polr) {
        result <- try(MASS::polr(formula, data = dataset, Hess = TRUE))
      } else {
        result <- try(glm(formula, data = dataset, family = "binomial"))
      }
    }

  } else if (type == "anova") {

    dependent <- decodeColNames(options[["dependent"]])
    factorVar <- decodeColNames(options[["fixedFactors"]])
    covariates <- decodeColNames(options[["covariates"]])

    # treat the independent variables
    istring <- paste0(factorVar, collapse = " + ")
    if (length(covariates) > 0) { # ANCOVA
      covString <- paste0(covariates, collapse = " + ")
      istring <- paste0(istring, " + ", covString)
    }

    # handle the interactions
    iastring <- .bfpackUnwrapInteractions(options)
    if (!is.null(iastring)) {
      istring <- paste0(istring, "+", iastring)
    }

    if (options[["excludeIntercept"]]) {
      istring <- paste0(istring, "-1")
    }

    if (length(dependent) == 1) { # ANOVA

      formula <- as.formula(paste0(dependent, "~", istring))
      result <- try(aov(formula, data = dataset))

    } else { # manova
      formula <- as.formula(paste0("cbind(", paste0(dependent, collapse = ","), ") ~ ", istring))
      result <- try(manova(formula, data = dataset))
    }

  } else if (type == "variances") {

    variable <- decodeColNames(options[["variables"]])
    grouping <- decodeColNames(options[["groupingVariable"]])

    result <- try(BFpack::bartlett_test(dataset[, variable], dataset[, grouping]))

  } else if (type == "tTestIndependentSamples") {

    variable <- decodeColNames(options[["variables"]])
    grouping <- decodeColNames(options[["groupingVariable"]])
    levels <- levels(dataset[[grouping]])
    # take only the first two levels, give a table footnote if there are more than two levels
    g1 <- levels[1]
    g2 <- levels[2]
    group1 <- dataset[dataset[[grouping]] == g1, variable]
    group2 <- dataset[dataset[[grouping]] == g2, variable]

    # since the bain package does not allow anything to be pased but a boolean for var.equal, we need to do this:
    if (options[["variances"]] == "equal") {
      result <- try(bain::t_test(x = group1, y = group2,
                                 paired = FALSE,
                                 var.equal = TRUE,
                                 conf.level = options[["ciLevel"]],
                                 mu = options[["muValue"]]))
    } else {
      result <- try(bain::t_test(x = group1, y = group2,
                                 paired = FALSE,
                                 var.equal = FALSE,
                                 conf.level = options[["ciLevel"]],
                                 mu = options[["muValue"]]))
    }


  } else if (type == "tTestOneSample") {

    variable <- decodeColNames(options[["variables"]])
    result <- try(bain::t_test(x = dataset[, variable], paired = FALSE, var.equal = FALSE,
                               conf.level = options[["ciLevel"]],
                               mu = options[["muValue"]]))

  } else if (type == "tTestPairedSamples") {
    variables <- decodeColNames(unlist(unlist(options[["pair"]])))
    result <- try(bain::t_test(x = dataset[, variables[1]], y = dataset[, variables[2]],
                               paired = TRUE, var.equal = FALSE,
                               conf.level = options[["ciLevel"]],
                               mu = options[["muValue"]]))

  } else if (type == "tTestMultiSamples") {

    testValues <- sapply(options[["testValues"]], function(x) x[["testValue"]])
    variables <- decodeColNames(options[["variables"]])
    result <- try(BFpack::mvt_test(X = dataset[, variables], conf.level = options[["ciLevel"]],
                  null = testValues))

  }

  if (isTryError(result)) {
    bfpackContainer$setError(gettextf("The parameter estimation failed. Error message: %1$s", jaspBase::.extractErrorMessage(result)))
    return()
  }

  # extract estimate names BEFORE saving state, so a failure here does not orphan estimatesState
  estimateNames <- try(eval(parse(text = callString))(result))
  if (isTryError(estimateNames)) {
    bfpackContainer$setError(gettextf("Could not extract parameter names. Error message: %1$s", jaspBase::.extractErrorMessage(estimateNames)))
    return()
  }
  # for manova, $estimate can be a matrix; for others a named vector
  est <- estimateNames$estimate
  if (is.matrix(est)) {
    # matrix: rows = factor levels, cols = DVs. Column-major flattening matches bain convention
    rn <- rownames(est)
    cn <- colnames(est)
    estimateNames <- as.list(as.vector(outer(rn, cn, paste, sep = "_on_")))
  } else {
    estimateNames <- as.list(names(est))
  }

  # save in jaspResults, which is where bfpackContainer is stored.
  # this way we do not have to estimate the parameters twice
  estimatesState <- createJaspState(result)
  estimatesState$dependOn(deps) # are there any new dependencies not already covered in the container?
  bfpackContainer[["estimatesState"]] <- estimatesState

  # new dependencies for the qml source since it is not part of bfpackContainer but jaspResults
  deps2 <- switch(type,
                 "tTestIndependentSamples" = c("variables", "groupingVariable"),
                 "tTestPairedSamples"      = "pair",
                 "tTestOneSample"          = "variables",
                 "anova"                   = c("dependent", "fixedFactors", "covariates",
                                               "interactionTerms", "includeInteractionEffect", "excludeIntercept"),
                 "regression"              = c("dependent", "predictors",
                                               "interactionTerms", "includeInteractionEffect", "excludeIntercept"),
                 "correlation"             = c("variables", "groupingVariable"),
                 "variances"               = c("variables", "groupingVariable"),
                 "regressionLogistic"      = c("dependent", "predictors",
                                               "interactionTerms", "includeInteractionEffect", "excludeIntercept"))

  namesForQml <- createJaspQmlSource("estimateNamesForQml", estimateNames)
  namesForQml$dependOn(deps2)
  # apparently the source only works directly with jaspResults
  jaspResults[["estimateNamesForQml"]] <- namesForQml

  return()
}

# compute the posteriors and BFs
.bfpackComputeResults <- function(dataset, options, bfpackContainer, ready, type) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["resultsState"]])) return()
  if (!ready) return()

  # create a container because both the results and the tables depending on them have the same dependencies
  deps <- c("complement", "logScale", "manualHypotheses", "priorProbManual",
            "priorProbStandard", "priorProbStandard2", "priorProbStandard3",
            "priorProbComplement", "seed", "setSeed", "bfType", "includeHypothesis", "ciLevel",
            "interactionTerms", "includeInteractionEffect", "priorProbMainZero",
            "priorProbMainNonZero", "priorProbInteractionZero", "priorProbInteractionNonZero",
            "iterationsBayesFactor")

  resultsContainer <- createJaspContainer()
  resultsContainer$dependOn(optionsFromObject = bfpackContainer[["estimatesState"]], options = deps)

  bfpackContainer[["resultsContainer"]] <- resultsContainer

  if (!is.null(bfpackContainer[["estimatesState"]])) {
    estimates <- bfpackContainer[["estimatesState"]]$object

    # standard hypotheses priors
    if (type %in% c("variances", "tTestMultiSamples")) {
      standPrior <- sapply(parse(text = c(options[["priorProbStandard"]], options[["priorProbStandard2"]])), eval)
    } else if (type == "anova") {
      standPrior <- list(sapply(parse(text = c(options[["priorProbStandard"]], options[["priorProbStandard2"]],
                                          options[["priorProbStandard3"]])), eval),
                         sapply(parse(text = c(options[["priorProbMainZero"]],
                                               options[["priorProbMainNonZero"]])), eval),
                         sapply(parse(text = c(options[["priorProbInteractionZero"]],
                                               options[["priorProbInteractionNonZero"]])), eval)
                         )
    } else {
      standPrior <- sapply(parse(text = c(options[["priorProbStandard"]], options[["priorProbStandard2"]],
                                          options[["priorProbStandard3"]])), eval)
    }

    # check if there are manual hypotheses
    manualHypInclude <- sapply(options[["manualHypotheses"]], function(x) x[["includeHypothesis"]])
    manualHyp <- sapply(options[["manualHypotheses"]], function(x) trimws(x[["hypothesisText"]]))
    manualPrior <- sapply(options[["manualHypotheses"]], function(x) x[["priorProbManual"]])

    # keep the hypotheses that are included
    manualHyp <- manualHyp[manualHypInclude]
    if (length(manualHyp) == 0 || all(manualHyp == "")) {
      manualHyp <- NULL
      manualPrior <- NULL
    } else {
      manualPrior <- manualPrior[manualHypInclude]
      manualHyp <- paste(manualHyp, collapse = ";")
      if (options[["complement"]]) manualPrior <- c(manualPrior, options[["priorProbComplement"]])
      # convert the prior character values to numeric:
      manualPrior <- sapply(manualPrior, function(x) eval(parse(text=x)))

      # special treatment for variances, see if levels of the grouping variable start with a numeric character, which would crash BFpack
      findex <- which(sapply(dataset, is.factor))
      if (length(findex > 0)) {
        if (type == "variances") {
          # there is only one factor for variances
          levs <- levels(dataset[, findex])
          if (any(grepl("^[0-9]", levs))) {
            jaspBase:::.quitAnalysis(gettext("BFpack does not accept factor levels that start with a number. Please remove the numbers from your factor levels to continue."))
          }
        }
      }

    }

    # BF.type depends in the analysis as well
    # seems that except for the correlation and variance, all other models have the adjusted bftype option
    if (!is.null(options[["bfType"]])) {
      if (options[["bfType"]] == "fractional") {
        bftype <- "FBF"
      } else {
        bftype <- "AFBF"
      }
    }

    if (type %in% c("tTestIndependentSamples", "anova", "tTestMultiSample"))
      iterations <- options[["iterationsBayesFactor"]]
    else
      iterations <- NULL

    .setSeedJASP(options)

    results <- try(BFpack::BF(estimates, hypothesis = manualHyp,
                             complement = options[["complement"]],
                             prior.hyp.conf = manualPrior,
                             prior.hyp.explo = standPrior,
                             log = options[["logScale"]],
                             BF.type = bftype,
                             cov.prob = options[["ciLevel"]],
                             iter = iterations))

    if (isTryError(results)) {

      if (grepl("parse_hyp$hyp_mat", results, fixed = TRUE) || grepl("pattern", results, fixed = TRUE)) {
        bfpackContainer$setError(gettext("BFpack failed because of an issue with the specification of the manual hypotheses. Please check that you specified them correctly. You should then refresh the analysis."))
      } else {
        bfpackContainer$setError(gettextf("BFpack failed with the following error message: %1$s", jaspBase::.extractErrorMessage(results)))

      }

    }

    # now saving the results

    resultsState <- createJaspState(results)
    resultsContainer[["resultsState"]] <- resultsState
  }

  return()
}

####### TABLES #######
# table for the posterior probabilities of the parameter estimates
.bfpackPosteriorParameterTable <- function(options, bfpackContainer, type, dataset, position) {

  # the parameterTable does go into the outer container given it does not depend on the options for the
  # inner container
  if (!is.null(bfpackContainer[["parameterTable"]])) return()

  parameterTable <- createJaspTable(gettext("Posterior Probabilities of the Standard Hypotheses"))
  parameterTable$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]],
                          options = c("priorProbStandard", "priorProbStandard2", "priorProbStandard3"))
  parameterTable$position <- position

  if (type %in% c("variances", "tTestMultiSamples")) {

    if (type == "variances") {
      title1 <- gettext("Equal Variances")
      title2 <- gettext("Unequal Variances")
    } else if (type == "tTestMultiSamples") {
      # testValues <- sapply(options[["testValues"]] function(x) x[["testValue"]]))
      title1 <- gettext("Pr(H0|Data)")
      title2 <- gettext("Pr(H±|Data)")
    }
    parameterTable$addColumnInfo(name = "equal", type = "number", title = title1)
    parameterTable$addColumnInfo(name = "unequal", type = "number", title = title2)

  } else {
    title1 <- gettext("Pr(H0|Data)")
    title2 <- gettext("Pr(H-|Data)")
    title3 <- gettext("Pr(H+|Data)")

    if (type == "correlation" && options[["groupingVariable"]] != "") {
      groupName <- options[["groupingVariable"]]
      levs <- levels(dataset[[groupName]])
      footnote <- ""
      for (i in 1:length(levs)) {
        footnote <- gettextf("%1$sGroup %2$s corresponds to level %3$s in variable %4$s. ", footnote, paste0("g", i), levs[i], groupName)
      }
      parameterTable$addFootnote(footnote)
    }

    parameterTable$addColumnInfo(name = "coefficient", type = "string", title = "")
    parameterTable$addColumnInfo(name = "equal", type = "number", title = title1)
    parameterTable$addColumnInfo(name = "smaller", type = "number", title = title2)
    parameterTable$addColumnInfo(name = "larger", type = "number", title = title3)
  }


  # assigning the table to the container here means we already display an empty table even if it is not yet
  # filled with data
  bfpackContainer[["parameterTable"]] <- parameterTable

  if (!bfpackContainer$getError()) {
    parPhp <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_exploratory
    if (!is.null(parPhp)) {
      if (type %in% c("variances", "tTestMultiSamples")) {
        dtFill <- data.frame(equal = parPhp[1], unequal = parPhp[2])
      } else {
        dtFill <- data.frame(coefficient = rownames(parPhp))
        dtFill[, c("equal", "smaller", "larger")] <- parPhp
      }
      parameterTable$setData(dtFill)

    }
  }

  # standard hypotheses priors
  if (type %in% c("variances", "tTestMultiSamples")) {
    standPrior <- sapply(parse(text = c(options[["priorProbStandard"]], options[["priorProbStandard2"]])), eval)
    footnoteText <- gettext("Prior probabilities of hypotheses H0 and H±:")
  } else {
    standPrior <- sapply(parse(text = c(options[["priorProbStandard"]], options[["priorProbStandard2"]],
                                        options[["priorProbStandard3"]])), eval)
    footnoteText <- gettext("Prior probabilities of hypotheses H0, H- and H+: ")
  }
  standPrior <- standPrior/sum(standPrior)

  # print the prior probs as a footnote
  parameterTable$addFootnote(paste0(footnoteText, paste0(sprintf("%.3f", standPrior), collapse = ", ")))

  if (type == "correlation") {
    corResultRhat <- bfpackContainer[["estimatesState"]][["object"]][["Rhat_gelmanrubin"]]
    psrf <- corResultRhat[["psrf"]]
    warns <- which(psrf[, "Point est."] > 1.05)
    if (length(warns) > 0) {
      parameterTable$addFootnote(gettextf("R-hat values for the posterior samples of the correlation coefficients are larger than 1.05.
                                           This indicates that the chains have not converged well.
                                           If you used the 'LD' sampling method try decreasing the nugget parameter or running the chains for more iterations.
                                           The following variables have R-hat > 1.05: %1$s.",
                           paste0(rownames(psrf)[warns], collapse = ", ")))
    }
    if (!is.null(corResultRhat[["mpsrf"]])) {
      if (corResultRhat[["mpsrf"]] > 1.05) {
        parameterTable$addFootnote(gettext("The multivariate R-hat value is larger than 1.05, indicating that the chains have not converged well.
                              If you used the 'LD' sampling method try decreasing the nugget parameter or running the chains for more iterations."))
      }
    }
  }

  if (type == "tTestIndependentSamples") {
    levels <- levels(dataset[[options[["groupingVariable"]]]])
    if (length(levels) > 2) {
      parameterTable$addFootnote(gettext("The number of factor levels in the grouping is greater than 2.
                                         Only the first two levels were used."))
    }
  }

  return()
}


# table for the posterior probabilities of the effects for anova models
.bfpackMainEffectsTable <- function(options, bfpackContainer, type, position) {

  # the mainEffectsTable does go into the outer container given it does not depend on the options for the
  # inner container
  if (!is.null(bfpackContainer[["mainEffectsTable"]])) return()

  mainEffectsTable <- createJaspTable(gettext("Posterior Probabilities for Main Effects"))
  mainEffectsTable$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]],
                            options = c("priorProbMainZero", "priorProbMainNonZero"))
  mainEffectsTable$position <- position

  mainEffectsTable$addColumnInfo(name = "coefficient", type = "string", title = "")
  mainEffectsTable$addColumnInfo(name = "noEffect", type = "number", title = gettext("Pr(No effect)"))
  mainEffectsTable$addColumnInfo(name = "fullModel", type = "number", title = gettext("Pr(Full Model)"))

  bfpackContainer[["mainEffectsTable"]] <- mainEffectsTable

  if (!bfpackContainer$getError()) {
    php <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_main
    if (!is.null(php)) {
      dtFill <- data.frame(coefficient = rownames(php))
      dtFill[, c("noEffect", "fullModel")] <- php
      mainEffectsTable$setData(dtFill)
    }
  }

  # standard main effects priors
  standPrior <- sapply(parse(text = c(options[["priorProbMainZero"]], options[["priorProbMainNonZero"]])), eval)
  standPrior <- standPrior/sum(standPrior)
  # print the prior probs as a footnote
  mainEffectsTable$addFootnote(gettextf("Prior probabilities of the main effects: %1$s.", paste0(sprintf("%.3f", standPrior), collapse = ", ")))

  return()
}

# table for the posterior probabilities of the effects for anova models
.bfpackInteractionEffectsTable <- function(options, bfpackContainer, type, position) {

  # the iaEffectsTable does go into the outer container given it does not depend on the options for the
  # inner container
  if (!is.null(bfpackContainer[["iaEffectsTable"]])) return()

  iaEffectsTable <- createJaspTable(gettext("Posterior Probabilities for Interaction Effects"))
  iaEffectsTable$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]],
                          options = c("priorProbInteractionZero", "priorProbInteractionNonZero"))
  iaEffectsTable$position <- position

  iaEffectsTable$addColumnInfo(name = "coefficient", type = "string", title = "")
  iaEffectsTable$addColumnInfo(name = "noEffect", type = "number", title = gettext("Pr(No effect)"))
  iaEffectsTable$addColumnInfo(name = "fullModel", type = "number", title = gettext("Pr(Full model)"))

  if (!bfpackContainer$getError()) {
    php <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_interaction

    if (!is.null(php)) {

      dtFill <- data.frame(coefficient = rownames(php))
      dtFill[, c("noEffect", "fullModel")] <- php
      iaEffectsTable$setData(dtFill)

      # table is only printed if there are interaction results
      bfpackContainer[["iaEffectsTable"]] <- iaEffectsTable
      # standard prior probs
      standPrior <- sapply(parse(text = c(options[["priorProbInteractionZero"]], options[["priorProbInteractionNonZero"]])), eval)
      standPrior <- standPrior/sum(standPrior)
      # print the prior probs as a footnote
      iaEffectsTable$addFootnote(gettextf("Prior probabilities of the interaction effects: %1$s.", paste0(sprintf("%.3f", standPrior), collapse = ", ")))
    }
  }

  return()
}


# Create a legend containing the order constrained hypotheses
.bfpackLegendTable <- function(options, type, bfpackContainer, position) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["legendTable"]])) return()

  legendTable <- createJaspTable(gettext("Manual Hypotheses Legend"))

  legendTable$dependOn("manualHypotheses")
  legendTable$position <- position
  legendTable$addColumnInfo(name = "number", type = "string", title = "")
  legendTable$addColumnInfo(name = "hypothesis", type = "string", title = gettext("Hypothesis"))

  if (!bfpackContainer$getError()) {
    # if there is a string in the hypotheses field but the suer forgot to check the include box we want an empty table
    # with a footnote
    manualHyp <- sapply(options[["manualHypotheses"]], function(x) x[["hypothesisText"]])
    manualHypInclude <- sapply(options[["manualHypotheses"]], function(x) x[["includeHypothesis"]])
    if (paste0(manualHyp, collapse = "") != "" && !any(manualHypInclude)) {
      legendTable$addFootnote(gettext("Check the 'Include' box to test the hypothesis."))
      # putting the assignment to the container here means the table is only displayed if it is filled with data
      bfpackContainer[["resultsContainer"]][["legendTable"]] <- legendTable
    }

    hypos <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$hypotheses
    if (!is.null(hypos)) {
      for (i in seq_len(length(hypos))) {
        row <- list(number = gettextf("H%i", i), hypothesis = hypos[i])
        legendTable$addRows(row)
      }

      bfpackContainer[["resultsContainer"]][["legendTable"]] <- legendTable
    }
  }


  return()
}


# table for the posterior probabilities of the parameter estimates
.bfpackMatrixTable <- function(options, bfpackContainer, type, position) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["matrixTable"]])) return()

  tbTitle <- ifelse(options[["logScale"]], gettext("Evidence Matrix (log BFs)"), gettext("Evidence Matrix (BFs)"))
  matrixTable <- createJaspTable(tbTitle)
  matrixTable$position <- position
  # matrixTable$dependOn()

  matrixTable$addColumnInfo(name = "hypothesis", title = "", type = "string")
  matrixTable$addColumnInfo(name = "H1", title = gettext("H1"), type = "number")
  matrixTable$addFootnote(gettext("The BFs are to be interpreted as the hypotheses in rows over the hypotheses in columns."))

  if (!bfpackContainer$getError()) {
    bfMatrix <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$BFmatrix_confirmatory
    if (!is.null(bfMatrix)) {
      if (nrow(bfMatrix) > 1) {
        for (i in 2:nrow(bfMatrix)) {
          matrixTable$addColumnInfo(name = paste0("H", i), title = gettextf("H%i", i), type = "number")
        }
      }
      for (i in seq_len(nrow(bfMatrix))) {
        tmp <- list(hypothesis = gettextf("H%i", i))
        for (j in seq_len(ncol(bfMatrix))) {
          tmp[[paste0("H", j)]] <- bfMatrix[i, j]
        }
        row <- tmp
        matrixTable$addRows(row)
      }

      bfpackContainer[["resultsContainer"]][["matrixTable"]] <- matrixTable
    }

  }

  return()
}



# create the table containing the posterior probabilities for the manual hypotheses
.bfpackPosteriorHypothesesTable <- function(options, bfpackContainer, type, position) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["postTable"]])) return()

  postTable <- createJaspTable(gettext("Posterior Model Probability"))
  postTable$position <- position

  postTable$addColumnInfo(name = "hypothesis", title = "", type = "string")
  postTable$addColumnInfo(name = "prob", title = gettext("P(H|Data)"), type = "number")

  if (!bfpackContainer$getError()) {
    php <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_confirmatory
    if (!is.null(php)) {
      for (i in seq_len(length(php))) {
        row <- list(hypothesis = gettextf("H%i", i), prob = php[i])
        postTable$addRows(row)
      }

      bfpackContainer[["resultsContainer"]][["postTable"]] <- postTable
    }

  }

  return()

}


# manual BF table
.bfpackManualBfTable <- function(options, bfpackContainer, type, position) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["specTable"]]) ||
      !options[["tablesManualHypothesesComputationBfs"]]) return()

  if (!is.null(options[["logScale"]])) {
    if (options[["logScale"]]) {
      title <- gettext("Log BFs: Manual Hypotheses")
    } else {
      title <- gettext("BFs: Manual Hypotheses")
    }
  }
  specTable <- createJaspTable(title)
  specTable$dependOn("tablesManualHypothesesComputationBfs")
  specTable$position <- position

  specTable$addColumnInfo(name = "hypothesis",  title = "",                               type = "string")
  specTable$addColumnInfo(name = "complex=",    title = gettext("Equal-Complex"),         type = "number")
  specTable$addColumnInfo(name = "complex>",    title = gettext("Order-Complex"),         type = "number")
  specTable$addColumnInfo(name = "fit=",        title = gettext("Equal-Fit"),             type = "number")
  specTable$addColumnInfo(name = "fit>",        title = gettext("Order-Fit"),             type = "number")
  specTable$addColumnInfo(name = "BF=",         title = gettext("Equal-BF"),              type = "number")
  specTable$addColumnInfo(name = "BF>",         title = gettext("Order-BF"),              type = "number")
  specTable$addColumnInfo(name = "BF",          title = gettext("BF"),                    type = "number")
  specTable$addColumnInfo(name = "PHP",         title = gettext("Posterior Probability"), type = "number")


  bfpackContainer[["resultsContainer"]][["specTable"]] <- specTable

  if (!bfpackContainer$getError()) {
    spec <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$BFtable_confirmatory
    if (!is.null(spec)) {
      if (options[["logScale"]]) {
        spec <- log(spec)
      }
      dtFill <- data.frame(hypothesis = paste0(gettext("H"), seq(1:nrow(spec))))
      dtFill[, c("complex=", "complex>", "fit=", "fit>", "BF=", "BF>", "BF", "PHP")] <- spec[, 1:8]
      specTable$setData(dtFill)
    }

  }

  return()
}


.bfpackEstimatesTable <- function(options, bfpackContainer, type, position = 1) {
  if (!is.null(bfpackContainer[["resultsContainer"]][["estimatesTable"]]) ||
      !options[["estimatesTable"]]) return()

  estimatesTable <- createJaspTable(gettext("Estimates Table"))
  estimatesTable$position <- position
  estimatesTable$dependOn("estimatesTable")
  bfpackContainer[["resultsContainer"]][["estimatesTable"]] <- estimatesTable

  interval <- gettextf("%s%% CI", format(100 * options[["ciLevel"]], digits = 3, drop0trailing = TRUE))
  intervalLow <- gettextf("%s Lower Bound", interval)
  intervalUp <- gettextf("%s Upper Bound", interval)


  estimatesTable$addColumnInfo(name = "coefficient", title = "", type = "string")
  estimatesTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
  estimatesTable$addColumnInfo(name = "median", title = gettext("Median"), type = "number")
  estimatesTable$addColumnInfo(name = "lower", title = intervalLow, type = "number")
  estimatesTable$addColumnInfo(name = "upper", title = intervalUp, type = "number")
  # estimatesTable$addColumnInfo(name = "pLarger0", title = gettext("P(>0)"), type = "number")

  if (!bfpackContainer$getError()) {
    result <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object
    estimates <- result$estimates
    #### TODO: estimates is NULL for independent TTest
    if (!is.null(estimates)) {
      dtFill <- data.frame(coefficient = rownames(estimates))
      dtFill[, c("mean", "median", "lower", "upper")] <- estimates[, 1:4]
      # dtFill[, c("mean", "median", "lower", "upper", "pLarger0")] <- estimates #for the future

      estimatesTable$setData(dtFill)
      footnt <- switch(type,
                       "correlation" = gettext("The uncertainty interval is a central credible interval."),
                       "anova" = gettext("The uncertainty interval is a frequentist confidence interval."),
                       "variances" = gettext("The uncertainty interval is a frequentist confidence interval."),
                       "regressionLogistic" = gettext("The uncertainty interval is a frequentist confidence interval."),
                       gettext("The uncertainty interval is a frequentist confidence interval as well as a credible interval with a noninformative Jeffrey's prior."))

      estimatesTable$addFootnote(footnt)
    }
  }

  return()
}

# standard BF table
.bfpackStandardBfTable <- function(options, bfpackContainer, type, position) {

  if (!is.null(bfpackContainer[["stdBfTable"]]) ||
      !options[["tablesStandardHypothesesViewBfs"]]) return()

  if (bfpackContainer$getError()) return()

  if (!is.null(options[["logScale"]])) {
    if (options[["logScale"]]) {
      title <- gettext("Log Standard Hypotheses: View BFs")
    } else {
      title <- gettext("Standard Hypotheses: View BFs")
    }
  }
  stdBfTable <- createJaspTable(title)
  stdBfTable$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]], options = "tablesStandardHypothesesViewBfs")
  stdBfTable$position <- position
  bfpackContainer[["stdBfTable"]] <- stdBfTable

  bfs <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$BFtu_exploratory
  if (is.null(bfs)) return()

  if (type %in% c("variances", "tTestMultiSamples")) {
    title1 <- gettext("H0 vs. H±)")
    title2 <- gettext("H± vs. H0")
    stdBfTable$addColumnInfo(name = "bf1", title = title1, type = "number")
    stdBfTable$addColumnInfo(name = "bf2", title = title2, type = "number")
    stdBfTable$setData(data.frame(bf1 = bfs[1], bf2 = 1/bfs[1]))

  } else {

    stdBfTable$addColumnInfo(name = "coefficient", title = "", type = "string")
    stdBfTable$addColumnInfo(name = "bf0", title = gettext("Best vs. H0"), type = "number")
    stdBfTable$addColumnInfo(name = "bf1", title = gettext("Best vs. H-"), type = "number")
    stdBfTable$addColumnInfo(name = "bf2", title = gettext("Best vs. H+"), type = "number")

    if (type == "tTestIndependentSamples") {
      dtFill <- data.frame(coefficient = gettext("difference"))
      bfs <- as.matrix(bfs)
    } else {
      dtFill <- data.frame(coefficient = rownames(bfs))
    }

    # For log BFs, evidence ratios become differences on the log scale.
    comparisonFun <- if (isTRUE(options[["logScale"]])) {
      function(x) max(x) - x
    } else {
      function(x) max(x) / x
    }

    # we do the best performing hypothesis vs. the others
    bfs <- t(apply(bfs, 1, comparisonFun))
    out <- as.data.frame(bfs)
    outDf <- sapply(out, function(x) {
    	x[x == 1] <- NA
    	x
    })

    dtFill[, c("bf0", "bf1", "bf2")] <- outDf
    stdBfTable$setData(dtFill)
    stdBfTable$addFootnote(gettext("If a cell is empty the hypothesis in that column is the best performing hypothesis."))
  }


  return()
}

####### PLOTS ########

.bfpackPriorPosteriorProbabilityPlot <- function(options, bfpackContainer, type, position = 7) {
  if (!is.null(bfpackContainer[["probabilitiesPlotContainer"]]) || !options[["manualPlots"]]) {
    return()
  }

  probabilitiesPlotContainer <- createJaspContainer(gettext("Prior and Posterior Probabilities"))
  probabilitiesPlotContainer$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]], options = "manualPlots")
  bfpackContainer[["probabilitiesPlotContainer"]] <- probabilitiesPlotContainer

  result <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object

  if (!bfpackContainer$getError() && !is.null(result$PHP_confirmatory)) {
    post <- result$PHP_confirmatory
    prior <- result$prior.hyp.conf

    priorPlot <- .plotHelper(prior, gettext("Prior Probabilities"))
    priorPlot$position <- position + 0.1
    probabilitiesPlotContainer[["priorPlot"]] <- priorPlot

    postPlot <- .plotHelper(post, gettext("Posterior Probabilities"))
    postPlot$position <- position + 0.2
    probabilitiesPlotContainer[["postPlot"]] <- postPlot

  } else {
    p <- ggplot2::ggplot() +
      jaspGraphs::getEmptyTheme()
    probabilitiesPlotContainer[["priorPlot"]] <- createJaspPlot(p, title = gettext("Prior Probabilities"),
                                                                width = 250, height = 250)
    probabilitiesPlotContainer[["postPlot"]]  <- createJaspPlot(p, title = gettext("Posterior Probabilities"),
                                                                width = 250, height = 250)
  }

}

.plotHelper <- function(probs, name) {

  dat <- data.frame(y = probs, group = paste0(gettext("H"), seq_len(length(probs))))

  pl <- ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x = "", y = y, fill = group)) +
    ggplot2::geom_bar(width = 1, stat = "identity", show.legend = TRUE, color = "black", linewidth = 1.5) +
    ggplot2::coord_polar(direction = -1, theta = "y") +
    jaspGraphs::getEmptyTheme() +
    ggplot2::scale_fill_discrete(name = gettext("Hypothesis")) +
    ggplot2::theme(legend.key.size = ggplot2::unit(1, 'cm'), #change legend key size
          legend.key.height = ggplot2::unit(1, 'cm'), #change legend key height
          legend.key.width = ggplot2::unit(1, 'cm'), #change legend key width
          legend.title = ggplot2::element_text(size=14), #change legend title font size
          legend.text = ggplot2::element_text(size=14)) #change legend text font size

  out <- createJaspPlot(pl, title = name, width = 250, height = 250)

  return(out)
}


# create the posterior distribution plots for correlation
.bfpackPosteriorDistributionPlot <- function(options, bfpackContainer, type, position = 8) {

  if (!is.null(bfpackContainer[["posteriorPlotContainer"]]) || !options[["priorPosteriorPlot"]]) {
    return()
  }

  posteriorPlotContainer <- createJaspContainer(gettext("Posterior Distribution"))
  posteriorPlotContainer$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]],
                                  options = c("priorPosteriorPlot", "priorPosteriorPlotAdditionalEstimationInfo",
                                              "priorPosteriorPlotAdditionalTestingInfo"))
  bfpackContainer[["posteriorPlotContainer"]] <- posteriorPlotContainer

  result <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object

  if (!bfpackContainer$getError() && !is.null(result)) {
    allDraws <- result$model$corrdraws
    dd <- dim(allDraws[[1]])
    numVars <- dd[2]
    iter <- dd[1]
    # prior
    seq1 <- seq(-1, 1, length = 2^10)
    P <- numVars # say
    xPri <- seq1
    yPri <- dbeta(seq1 / 2 + .5, P / 2, P / 2) / 2
    yPri0 <- dbeta(0 / 2 + .5, P / 2, P / 2) / 2


    corNames <- rownames(result$estimates)
    z <- 1
    for (l in 1:length(allDraws)) { # for multiple groups the list is longer than 1
      for (j in 1:(numVars - 1)) {
        for (i in (j + 1):numVars) {
          postSamp <- allDraws[[l]][, i, j]
          postDens <- density(postSamp, n = 2^10, bw = "SJ")
          postY0 <- approx(postDens$x, postDens$y, xout = 0)$y

          dfLines <- data.frame(x = c(xPri, postDens$x), y = c(yPri, postDens$y), g = c(rep(gettext("Prior"), length(xPri)), rep(gettext("Posterior"), length(postDens$x))))
          dfPoints <- data.frame(x = c(0, 0), y = c(yPri0, postY0), g = c("Prior", "Posterior"))
          BF0u <- result$BFtu_exploratory[z, "BF0u"]
          cri <- result$estimates[z, 3:4]
          criTxt <- gettextf("%s%% CI ", format(100 * options[["ciLevel"]]))
          med <- result$estimates[z, "median"]

          dfPointsVal <- if (options[["priorPosteriorPlotAdditionalTestingInfo"]]) dfPoints else NULL
          BFVal <- if (options[["priorPosteriorPlotAdditionalTestingInfo"]]) BF0u else NULL
          CRIVal <- if (options[["priorPosteriorPlotAdditionalEstimationInfo"]]) cri else NULL
          medianVal <- if (options[["priorPosteriorPlotAdditionalEstimationInfo"]]) med else NULL

          plt <- jaspGraphs::PlotPriorAndPosterior(
            dfLines = dfLines,
            dfPoints = dfPointsVal,
            BF = BFVal,
            CRI = CRIVal,
            CRItxt = criTxt,
            median = medianVal,
            xName = gettext("ρ"),
            hypothesis = "equal",
            bfType = "BF01",
            pizzaTxt = c("data | H0", "data | Hu"),
            bfSubscripts = c("BF0u", "BFu0")
          )

          height <- if (!options[["priorPosteriorPlotAdditionalTestingInfo"]] && !options[["priorPosteriorPlotAdditionalEstimationInfo"]]) 400 else 460
          postPlot <- createJaspPlot(plt, title = corNames[z], width = 560, height = height)
          postPlot$position <- position + z * 0.1
          posteriorPlotContainer[[paste0("cor", z)]] <- postPlot
          z <- z + 1
        }
      }
    }
  }

  return()
}


# create the traceplot for correlation
.bfpackTraceplot <- function(options, bfpackContainer, type, position = 9) {
  if (!is.null(bfpackContainer[["traceplotContainer"]]) || !options[["traceplot"]]) {
    return()
  }

  traceplotContainer <- createJaspContainer(gettext("Traceplot"))
  traceplotContainer$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]], options = "traceplot")
  bfpackContainer[["traceplotContainer"]] <- traceplotContainer

  result <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object

  if (!bfpackContainer$getError() && !is.null(result)) {
    allDraws <- result$model$corrdraws
    dd <- dim(allDraws[[1]])
    numVars <- dd[2]
    iter <- dd[1]
    corNames <- rownames(result$estimates)
    z <- 1
    for (l in 1:length(allDraws)) { # for multiple groups the list is longer than 1
      for (j in 1:(numVars - 1)) {
        for (i in (j + 1):numVars) {
          post <- allDraws[[l]][, i, j]
          postPlot <- .makeSingleTraceplot(post)
          postPlot$title <- corNames[z]
          postPlot$position <- position + z * 0.1
          traceplotContainer[[paste0("cor", z)]] <- postPlot
          z <- z + 1
        }
      }
    }
  }

}

.makeSingleTraceplot <- function(samples) {

  dv <- cbind(samples, seq(1, length(samples)))
  dat <- data.frame(dv)
  colnames(dat) <- c("Value", "Iterations")
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$Iterations)

  g <- ggplot2::ggplot(dat, ggplot2::aes(x = Iterations, y = Value)) +
    ggplot2::geom_line() +
    ggplot2::ylab(gettext("x")) +
    ggplot2::scale_x_continuous(name = gettext("Iterations"),
                                expand = ggplot2::expansion(mult = c(0.05, 0.1)))
  g <- g + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
  g <- createJaspPlot(g, width = 580)
  return(g)
}

##### Helpers #####

.credInterval <- function(draws, level) {
  # the credi interval:
  bounds <- apply(draws, c(2, 3), function(x) {
    coda::HPDinterval(coda::as.mcmc(x), prob = level)
  })
  boundsLow <- bounds[1, , ]
  boundsUp <- bounds[2, , ]
  # kind of hope this structure never changes so the elements are always in the correct order
  boundsLow <- boundsLow[lower.tri(boundsLow)]
  boundsUp <- boundsUp[lower.tri(boundsUp)]

  return(list(low = boundsLow, up = boundsUp))
}
