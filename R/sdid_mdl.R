#' Constructor for a sdid_mdl object
#'
#' @param mdl A `lm` object containing the fitted model
#' @param formula A list object containing the supplied and fitted formulas
#' @param vcov A variance-covariance matrix calculated using the supplied vcov
#' estimator
#' @param tsi A data frame containing a time-since-intervention crosswalk for
#' each observed time period and cohort
#' @param obs_cnt A data frame containing the count of observations for each
#' interaction term in the model
#' @param cohort A list object containing elements `var` (the name of the column
#' that contains cohort labels in the data frame used to fit the model), `ref`
#' (the value of the cohor referent level), and `time_refs` (referent time
#' period levels for each cohort)
#' @param time A list object containing elements `var` (the name of the column
#' that contains time period labels in the data frame used to fit the model) and
#' `ref` (the value of the time period referent level).
#' @param intervention_var Name of the column denoting the time period during
#' which each cohort implemented the intervention
#' @param covariates A character vector containing all covariates (other than
#' cohort and time period) used to adjust the model
#'
#' @return An object of class "sdid_mdl"
#' @noRd

new_sdid <- function(mdl, formula, vcov = NULL, tsi, obs_cnt, cohort, time, intervention_var, covariates) {
  if(!all(!vapply(cohort[c("var", "ref", "time_refs")], is.null, logical(1)))) {
    stop("cohort must contain a list with elements var, ref, and time_refs.")
  }

  if(!all(!vapply(time[c("var", "ref")], is.null, logical(1)))) {
    stop("time must contain a list with elements var and ref.")
  }

  structure(
    list(
      mdl = mdl,
      formula = formula,
      vcov = vcov,
      tsi = tsi,
      obs_cnt = obs_cnt,
      cohort = cohort,
      time = time,
      intervention_var = intervention_var,
      covariates = covariates
    ),
    class = "sdid_mdl"
  )
}
