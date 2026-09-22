#' Retrieve a list of interaction terms from a sdid model representing a specified time-since-intervention
#'
#' @param sdid A sdid object
#' @param tsi Integer indicating how many time periods away from the
#' intervention the function should identify coefficients for
#' @param cohorts A character vector containing cohort levels to include in the
#' term selection. If `cohorts` is omitted, all available cohorts will be
#' selected
#'
#' @return character vector
#' @export select_tsi
#' @examples
#' # Fit a staggered difference-in-differences model
#' sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
#'                   df = hosp,
#'                   intervention_var  = "intervention_yr")
#'
#' # Select coefficients corresponding to 5 time periods before intervention
#' coef_selection_pre5 <- select_tsi(sdid_hosp,
#'                                   tsi = -5)
#' coef_selection_pre5
#'
#' # Select coefficients corresponding to 2 time periods after intervention,
#' # for cohorts 5 and 6 only
#' coef_selection_post2 <- select_tsi(sdid_hosp,
#'                                    tsi = 2,
#'                                    cohorts = c("5", "6"))
#' coef_selection_post2

select_tsi <- function(sdid, tsi = 0, cohorts = NULL) {
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
  valid_tsi <- sdid$tsi[sdid$tsi$cohort %in% cohorts, ]

  # Sanitize cohort and time periods in TSI data
  valid_tsi$cohort <- make.names(valid_tsi$cohort)
  valid_tsi$time <- make.names(valid_tsi$time)

  # Restrict tsi to non-comparison group cohorts
  valid_tsi <- valid_tsi[!is.na(valid_tsi$tsi),]

  # Exclude referent time periods
  for(cohort_lvl in unique(valid_tsi$cohort)) {
    valid_tsi$time_ref[valid_tsi$cohort == cohort_lvl] <- sdid$cohort$time_refs[[as.character(cohort_lvl)]]
  }
  valid_tsi <- valid_tsi[valid_tsi$time != valid_tsi$time_ref,]

  # Restrict to cohort-time period combinations for the specified TSI
  valid_tsi <- valid_tsi[valid_tsi$tsi == tsi,]

  # Throw an error if there are no remaining rows in valid_tsi
  if(nrow(valid_tsi) == 0) {
    stop("There are no valid coefficients for TSI ", tsi, ".\n",
         "Did you pass the referent time period to the tsi parameter?")
  }

  # Now retrieve the values of the relevant interaction terms
  valid_tsi$coefs <- with(valid_tsi,
                          paste0(sdid$cohort$var, "_", cohort,
                                 ":",
                                 sdid$time$var, "_", time))

  coefs <- valid_tsi[, "coefs"]

  # Check that all terms appear in the list of coefficients
  if(!all(coefs %in% names(sdid$mdl$coefficients))) {
    stop("Selected terms (",
         paste(coefs, collapse = ", "),
         ") do not all appear in the model's list of coefficients.")
  } else return(coefs)
}
