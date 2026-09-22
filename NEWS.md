## staggR 0.1.0

* Initial CRAN submission.

## staggR 0.1.1

* Excluded non-package directories from source bundle.
* Fixed S3 method registration for `print.summary.sdid_mdl`.

## staggR 0.2.0

* Add `detrend()` and `detrend_factor()` functions to facilitate trend-adjusted staggered DiD modeling

## staggR 0.2.1

* Fixed bug (Issue 6) causing `sdid()` to throw errors if the specified `time_var` is a Date data type
* Fixed bug (Issue 7) where the normal distribution was used instead of the t distribution to calculate p-values in `ave_coeff()`
* Fixed bug (Issue 8) causing ambiguous, unhelpful error message when the referent group is left unspecified in calls to `sdid()`
* Fixed bug (Issue 9) causing results of `summary.sdid()` to be unpredictable when one or more coefficients are NA
* Fixed bug (Issue 10) causing `sdid()` to throw an error if there are other columns in the data frame whose names contain the name of the cohort variable
* Fixed bug (Issue 11) causing incorrect asterisk indicators of statistical significance from `ave_coeff()`
* Added functionality to `ave_coeff()` that facilitates producing event-study and calendar time summaries from a `sdid` object
