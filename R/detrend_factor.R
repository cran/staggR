#' Calculate aggregated variance to provide denominator for trend-adjusting outcomes
#'
#' @param sdid sdid object containing the model to summarize.
#' @param df A data frame containing the variables in the model.
#' @return numeric
#' @export detrend_factor
#' @examples
#' # First fit a model using the original outcome, with or without covariate adjustments.
#' sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
#'                   df = hosp,
#'                   intervention_var  = "intervention_yr")
#'
#' # Retrieve an estimate for the pre-intervention period
#' raw_pre_intervention_trend <-
#'   ave_coeff(sdid = sdid_hosp,
#'             coefs = select_period(sdid = sdid_hosp, period = "pre"))[["est"]]
#'
#' # Normalize the pre-intervention estimate by de-trending factor
#' beta_detrend <-
#'   raw_pre_intervention_trend /
#'   detrend_factor(sdid = sdid_hosp,
#'                  df = hosp)

detrend_factor <- function(sdid, df){
  # Trim prefixes from cohort and time columns in observation counts from the sdid object
  # so it will merge to the tsis from the same sdid object
  obs_cnt <- sdid$obs_cnt
  obs_cnt$cohort <- sub(paste0(sdid$cohort$var, "_"), "", obs_cnt$cohort)
  obs_cnt$time <- sub(paste0(sdid$time$var, "_"), "", obs_cnt$time)

  # Merge tsis to observation counts
  reg_sigma <- merge(sdid$tsi[!is.na(sdid$tsi$tsi),],
                     obs_cnt, by = c("cohort", "time"),
                     all.x = TRUE)

  # Restrict to the pre-intervention period
  reg_sigma <- reg_sigma[reg_sigma$tsi < 0, ]

  # Sum observations across cohorts by TSI
  reg_sigma <- stats::aggregate(n_obs ~ tsi,
                                data = reg_sigma,
                                FUN = sum)

  # Compute sigma for each TSI
  reg_sigma$sigma <- reg_sigma$n_obs / sum(reg_sigma$n_obs, na.rm = TRUE)

  ## Calculate sum of shares and tsi and return
  return(sum(reg_sigma$sigma * (abs(reg_sigma$tsi) - 1), na.rm = TRUE))
}
