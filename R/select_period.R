#' Retrieve a list of interaction terms from a sdid model representing the pre-intervention period
#'
#' @param sdid A sdid object
#' @param period One of 'pre' or 'post', to return the pre-intervention or
#' post-intervention coefficients respectively
#' @param cohorts A character vector containing cohort levels to include in the
#' term selection. If `cohorts` is omitted, all available cohorts will be
#' selected
#'
#' @return character vector
#' @export select_period
#' @examples
#' # Fit a staggered difference-in-differences model
#' sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
#'                   df = hosp,
#'                   intervention_var  = "intervention_yr")
#'
#' # Select coefficients corresponding to the PRE-intervention period for cohort 5
#' coef_selection_pre <- select_period(sdid_hosp,
#'                                 period = "pre",
#'                                 cohorts = "5")
#' coef_selection_pre
#'
#' # Pass the set of coefficients to `ave_coeff` to aggregate the effect of the
#' # intervention
#' ave_coeff(sdid_hosp, coefs = coef_selection_pre)
#'
#' # Select coefficients corresponding to the POST-intervention period for cohort 5
#' coef_selection_post <- select_period(sdid_hosp,
#'                                      period = "post",
#'                                      cohorts = "5")
#' coef_selection_post
#'
#' # Pass the set of coefficients to `ave_coeff` to aggregate the effect of the
#' # intervention
#' ave_coeff(sdid_hosp, coefs = coef_selection_post)


select_period <- function(sdid, period = "post", cohorts = NULL) {
  # Validate that period is "pre" or "post"
  if(!period %in% c("pre", "post")) {
    stop("`period` must be set to \"pre\" or \"post\".")
  }

  # Validate that cohorts contains valid cohort levels
  if(!all(cohorts %in% unique(sdid$tsi$cohort))) {
    stop(paste0("One or more supplied values for cohorts (",
                paste(cohorts, collapse = ", "),
                ") are invalid cohort levels.\n",
                "Must match one or more of {",
                paste(unique(sdid$tsi$cohort), collapse = ", "),
                "}."))
  }

  # If the user didn't specify cohorts, set it to include all cohorts except
  # the referent
  if(is.null(cohorts)) {
    cohorts <- unique(sdid$tsi[sdid$tsi[["cohort"]] != sdid$cohort$ref, "cohort"])
  }

  # Restrict tsi dataset to the specified cohorts
  tsi <- sdid$tsi[sdid$tsi$cohort %in% cohorts, ]

  # Restrict tsi dataset to just the pre-intervention period
  if(period == "pre") {
    tsi <- tsi[tsi$tsi < 0, ]
  } else if(period == "post") {
    tsi <- tsi[tsi$tsi >= 0,]
  }

  # Exclude referent time periods
  for(cohort_lvl in unique(tsi$cohort)) {
    tsi$time_ref[tsi$cohort == cohort_lvl] <- sdid$cohort$time_refs[[as.character(cohort_lvl)]]
  }
  tsi <- tsi[tsi$time != tsi$time_ref,]

  # Now retrieve the values of the relevant interaction terms
  tsi$coefs <- with(tsi,
                    paste0(sdid$cohort$var, "_", cohort,
                           ":",
                           sdid$time$var, "_", time))

  coefs <- tsi[, "coefs"]

  # Check that all terms appear in the list of coefficients
  if(!all(coefs %in% names(sdid$mdl$coefficients))) {
    stop("Selected terms (",
         paste(coefs, collapse = ", "),
         ") do not all appear in the model's list of coefficients.")
  } else return(coefs)
}
