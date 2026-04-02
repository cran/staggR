#' Adjust outcomes for differential trends
#'
#' @param sdid sdid object containing the model to summarize.
#' @param df A data frame containing the variables in the model.
#' @return data.frame
#' @export detrend
#' @examples
#' # First fit a model using the original outcome, with or without covariate adjustments.
#' sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
#'                   df = hosp,
#'                   intervention_var  = "intervention_yr")
#'
#' # Calculate de-trending adjustments
#' hosp_det <- detrend(sdid = sdid_hosp,
#'                     df = hosp)
#'
#' # Then refit the same model, substituting the _detrended version of the outcome
#' sdid_hosp_det <- sdid(hospitalized_detrended ~ cohort + yr + age + sex + comorb,
#'                       df = hosp_det,
#'                       intervention_var = "intervention_yr")

detrend <- function(sdid, df) {
  df <- as.data.frame(df)
  cn_df <- colnames(df)
  outcome <- sdid$formula$supplied[[2]]

  # Retrieve beta for the pre-intervention period
  raw_pre_intervention_trend <-
    ave_coeff(sdid = sdid,
              coefs = select_period(sdid = sdid, period = "pre"))[["est"]]

  # Normalize beta by de-trending factor
  beta_detrend <-
    raw_pre_intervention_trend /
    detrend_factor(sdid = sdid,
                   df = df)

  # Merge TSIs to df
  df$cohort <- df[[sdid$cohort$var]]
  df$time <- df[[sdid$time$var]]

  df <- merge(x = df, y = sdid$tsi, all.x = TRUE, by = c("cohort", "time"))

  # De-trend relative to time since intervention
  df[[paste0(outcome, "_detrended")]] <-
    df[[outcome]] +
    ifelse(df[[sdid$cohort$var]] == sdid$cohort$ref,
           yes = 0,
           no = beta_detrend*(df$tsi+1))

  return(df[, c(cn_df, paste0(outcome, "_detrended"))])
}
