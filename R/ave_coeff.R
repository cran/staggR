#' Aggregate a specified set of terms and corresponding standard errors from an sdid model object
#'
#' @param sdid sdid object containing the model to summarize
#' @param coefs Character vector containing the names of coefficients to
#' aggregate. Can be specified using `select_period()` or `select_terms()`.
#' @return data.frame
#' @export ave_coeff
#' @examples
#' # First fit a model to generate a sdid object
#' sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
#'                   df = hosp,
#'                   intervention_var  = "intervention_yr")
#'
#' # Then request an average of a specified set of coefficients. Here we use the
#' # select_period() convenience function to automatically select all
#' # coefficients representing the post-intervention period.
#' ave_coeff(sdid_hosp, coefs = select_period(sdid_hosp, period = "post"))
#'
#' # We could also specify the coefficients manually. Here we request the
#' # average effect for Cohort 5 in the post-intervention period.
#' ave_coeff(sdid_hosp, coefs = c("cohort_5:yr_2015", "cohort_5:yr_2016",
#'                                "cohort_5:yr_2017", "cohort_5:yr_2018",
#'                                "cohort_5:yr_2019", "cohort_5:yr_2020"))

ave_coeff <- function(sdid, coefs) {

  # Make sure coefs is not null
  if(is.null(coefs)) {
    stop("Must specify `coefs`.")
  }

  # Make sure the specified coefs exist in the model
  if(!all(coefs %in% names(sdid$mdl$coefficients))) {
    stop("One or more specified coefs (",
         paste(coefs, collapse = ", "),
         ") do not exist in the supplied sdid model object.")
  }

  # Step 1: Calculate population fractions and extract estimates

  ## Number of observations for each specified coeff
  n_obs <- sapply(coefs, function(coeff) {
    if(grepl(pattern = ":", x = coeff)) {
      coeff_parts <- unlist(strsplit(coeff, ":"))
      rtn_cnt <- sdid$obs_cnt[sdid$obs_cnt$cohort == coeff_parts[[1]] &
                                sdid$obs_cnt$time == coeff_parts[[2]], "n_obs"]
    } else {
      rtn_cnt <- sum(sdid$obs_cnt[sdid$obs_cnt$cohort == coeff |
                                    sdid$obs_cnt$time == coeff,
                                  "n_obs"])
    }
    return(rtn_cnt)
  })

  ## Pct of population for each coefficient
  ave_pct <- n_obs/sum(n_obs)

  ## Extract estimates selected for averaging
  select_est <- sapply(coefs, function(x) sdid$mdl$coef[names(sdid$mdl$coef)== x])


  # Step 2: Calculate average estimate and corresponding SE, p-value, CI

  ## Calculate weighted estimate
  ave_est <- sum(ave_pct * select_est) #One weighted estimate

  ## Extract relevant part of variance-covariance matrix
  select_vcv <- sdid$vcov[coefs, coefs]

  ## Get SE of average estimate and convert from a matrix to a vector
  ave_se <- as.vector(sqrt(t(as.matrix(ave_pct)) %*% select_vcv %*% as.matrix(ave_pct)))

  ## Extract degrees of freedom, which we use for our P value
  df <- sdid$mdl$df.residual

  ## Calculate p-value, assuming normal distribution
  ave_pval <- stats::qnorm(0.975)*stats::pt(abs(ave_est/ave_se), df, lower=FALSE)

  ## Make into a df table
  ave_res <- data.frame(est  = ave_est,
                        se   = ave_se ,
                        pval = ave_pval,
                        sign = ifelse(ave_pval < 0.01, "***",
                                      ifelse(ave_pval < 0.05, "**",
                                             ifelse(ave_pval < 0.1,  "*", ""))),
                        lb   = (ave_est - 1.96*ave_se),
                        ub   = (ave_est + 1.96*ave_se),
                        n    = sum(n_obs))
  return(ave_res)
}
