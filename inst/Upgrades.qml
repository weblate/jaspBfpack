import QtQuick
import JASP.Module

// Several options were renamed without a version bump (module versions 0.19.x - 0.95.5), so analyses saved by
// the same module version can have either the old or the new name. All renames are therefore conditional:
// they only run if the old name is present and the new name is not.
//
// Renamed options (the saved value, including the stored column types, is kept):
//   manualHypothesisBfTable   -> tablesManualHypothesesComputationBfs	(all analyses)
//   standardHypothesisBfTable -> tablesStandardHypothesesViewBfs		(all analyses)
//   covariates                -> predictors							(bfpackRegressionLinear, bfpackRegressionLogistic)
//   pairs                     -> pair									(bfpackTTestPairedSamples)
//
// New options correlationSamplingMethod and excludeIntercept need no upgrade: their defaults reproduce the
// previous behaviour (LKJ sampler; intercept excluded for the ANOVA only).
Upgrades
{
	Upgrade
	{
		functionName:	"bfpackTTestIndependentSamples"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeRename
		{
			from:		"manualHypothesisBfTable"
			to:			"tablesManualHypothesesComputationBfs"
			condition:	function(options) { return options["manualHypothesisBfTable"] !== undefined && options["tablesManualHypothesesComputationBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"standardHypothesisBfTable"
			to:			"tablesStandardHypothesesViewBfs"
			condition:	function(options) { return options["standardHypothesisBfTable"] !== undefined && options["tablesStandardHypothesesViewBfs"] === undefined; }
		}
	}

	Upgrade
	{
		functionName:	"bfpackTTestPairedSamples"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeRename
		{
			from:		"manualHypothesisBfTable"
			to:			"tablesManualHypothesesComputationBfs"
			condition:	function(options) { return options["manualHypothesisBfTable"] !== undefined && options["tablesManualHypothesesComputationBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"standardHypothesisBfTable"
			to:			"tablesStandardHypothesesViewBfs"
			condition:	function(options) { return options["standardHypothesisBfTable"] !== undefined && options["tablesStandardHypothesesViewBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"pairs"
			to:			"pair"
			condition:	function(options) { return options["pairs"] !== undefined && options["pair"] === undefined; }
		}
	}

	Upgrade
	{
		functionName:	"bfpackTTestOneSample"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeRename
		{
			from:		"manualHypothesisBfTable"
			to:			"tablesManualHypothesesComputationBfs"
			condition:	function(options) { return options["manualHypothesisBfTable"] !== undefined && options["tablesManualHypothesesComputationBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"standardHypothesisBfTable"
			to:			"tablesStandardHypothesesViewBfs"
			condition:	function(options) { return options["standardHypothesisBfTable"] !== undefined && options["tablesStandardHypothesesViewBfs"] === undefined; }
		}
	}

	Upgrade
	{
		functionName:	"bfpackTTestMultiSample"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeRename
		{
			from:		"manualHypothesisBfTable"
			to:			"tablesManualHypothesesComputationBfs"
			condition:	function(options) { return options["manualHypothesisBfTable"] !== undefined && options["tablesManualHypothesesComputationBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"standardHypothesisBfTable"
			to:			"tablesStandardHypothesesViewBfs"
			condition:	function(options) { return options["standardHypothesisBfTable"] !== undefined && options["tablesStandardHypothesesViewBfs"] === undefined; }
		}
	}

	Upgrade
	{
		functionName:	"bfpackAnova"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeRename
		{
			from:		"manualHypothesisBfTable"
			to:			"tablesManualHypothesesComputationBfs"
			condition:	function(options) { return options["manualHypothesisBfTable"] !== undefined && options["tablesManualHypothesesComputationBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"standardHypothesisBfTable"
			to:			"tablesStandardHypothesesViewBfs"
			condition:	function(options) { return options["standardHypothesisBfTable"] !== undefined && options["tablesStandardHypothesesViewBfs"] === undefined; }
		}
	}

	Upgrade
	{
		functionName:	"bfpackRegressionLinear"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeRename
		{
			from:		"manualHypothesisBfTable"
			to:			"tablesManualHypothesesComputationBfs"
			condition:	function(options) { return options["manualHypothesisBfTable"] !== undefined && options["tablesManualHypothesesComputationBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"standardHypothesisBfTable"
			to:			"tablesStandardHypothesesViewBfs"
			condition:	function(options) { return options["standardHypothesisBfTable"] !== undefined && options["tablesStandardHypothesesViewBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"covariates"
			to:			"predictors"
			condition:	function(options) { return options["covariates"] !== undefined && options["predictors"] === undefined; }
		}
	}

	Upgrade
	{
		functionName:	"bfpackRegressionLogistic"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeRename
		{
			from:		"manualHypothesisBfTable"
			to:			"tablesManualHypothesesComputationBfs"
			condition:	function(options) { return options["manualHypothesisBfTable"] !== undefined && options["tablesManualHypothesesComputationBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"standardHypothesisBfTable"
			to:			"tablesStandardHypothesesViewBfs"
			condition:	function(options) { return options["standardHypothesisBfTable"] !== undefined && options["tablesStandardHypothesesViewBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"covariates"
			to:			"predictors"
			condition:	function(options) { return options["covariates"] !== undefined && options["predictors"] === undefined; }
		}
	}

	Upgrade
	{
		functionName:	"bfpackCorrelation"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeRename
		{
			from:		"manualHypothesisBfTable"
			to:			"tablesManualHypothesesComputationBfs"
			condition:	function(options) { return options["manualHypothesisBfTable"] !== undefined && options["tablesManualHypothesesComputationBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"standardHypothesisBfTable"
			to:			"tablesStandardHypothesesViewBfs"
			condition:	function(options) { return options["standardHypothesisBfTable"] !== undefined && options["tablesStandardHypothesesViewBfs"] === undefined; }
		}
	}

	Upgrade
	{
		functionName:	"bfpackVariances"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeRename
		{
			from:		"manualHypothesisBfTable"
			to:			"tablesManualHypothesesComputationBfs"
			condition:	function(options) { return options["manualHypothesisBfTable"] !== undefined && options["tablesManualHypothesesComputationBfs"] === undefined; }
		}

		ChangeRename
		{
			from:		"standardHypothesisBfTable"
			to:			"tablesStandardHypothesesViewBfs"
			condition:	function(options) { return options["standardHypothesisBfTable"] !== undefined && options["tablesStandardHypothesesViewBfs"] === undefined; }
		}
	}
}
