#' Retrieve a list of interaction terms from a sdid model to be passed on for aggregation
#'
#' @param sdid A sdid object
#' @param coefs Optional list of specific terms from `mdl` to be selected
#' @param selection List object containing values for named elements `cohorts`,
#' `times`, and `tsi`. `cohorts` contains a character vector of cohort levels to
#' include in the term selection; `times` contains a character vector of time
#' period levels to include in the term selection; and `tsi` contains a vector
#' of integers representing the number of units of time relative to each
#' cohort's intervention to include in the term selection. If `cohorts` is
#' omitted, all available cohorts will be selected. One of `times` or `tsi` must
#' be specified. If both are specified, `times` is ignored.
#'
#' @return character vector
#' @export select_terms
#' @examples
#' # Fit a staggered difference-in-differences model
#' sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
#'                           df = hosp,
#'                           intervention_var  = "intervention_yr")
#'
#' # Select coefficients corresponding to all intervention cohorts in 2018
#' terms_2018 <- select_terms(sdid = sdid_hosp,
#'                                   selection = list(times = "2018"))
#' terms_2018
#'
#' # Pass the set of coefficients to `ave_coeff` to aggregate the effect of the
#' # intervention
#' ave_coeff(sdid_hosp, coefs = terms_2018)
#'
#' # Select coefficients corresponding to added risk of hospitalization associated with
#' # the intervention in the year 2018, but only for the first two cohorts (5 and 6)
#' terms_2018_cohorts56 <- select_terms(sdid = sdid_hosp,
#'                                      selection = list(cohorts = c("5", "6"),
#'                                                       times = "2018"))
#'
#' # Pass the set of coefficients to `ave_coeff` to aggregate the effect of the
#' # intervention
#' ave_coeff(sdid_hosp, coefs = terms_2018_cohorts56)

select_terms <- function(sdid, coefs = NULL, selection = NULL) {
  # Validate that selection$cohorts contains valid cohort levels
  if(is.null(coefs) & !all(selection$cohorts %in% unique(sdid$tsi$cohort))) {
    stop(paste0("One or more supplied values for cohorts (",
                paste(selection$cohorts, collapse = ", "),
                ") are invalid cohort levels.\n",
                "Must match one or more of {",
                paste(unique(sdid$tsi$cohort), collapse = ", "),
                "}."))
  }

  # If the user specified coefs, check that all coefs appear in the model
  if(!is.null(coefs)) {
    if(!all(coefs %in% names(sdid$mdl$coefficients))) {
      stop(paste0("Specified coefs must be contained in sdid model coefficients."))
    }
  }

  # If the user didn't specify selection$cohorts, set it to include all cohorts except
  # the referent
  if(is.null(selection$cohorts)) {
    selection$cohorts <-
      unique(sdid$tsi[sdid$tsi[["cohort"]] != sdid$cohort$ref, "cohort"])
  }

  # The user specified cohorts and times, but not tsi
  if(is.null(coefs) &
     !is.null(selection$times) & is.null(selection$tsi)) {
    prelim_coefs <- with(sdid$cohort, paste0(
      var, "_", rep(selection$cohorts[selection$cohorts != ref],
                       each = length(selection$times)),
      ":", sdid$time$var, "_", rep(selection$times,
                          times = length(selection$cohorts))))
    # Check that all terms appear in the list of coefficients
    if(!all(prelim_coefs %in% names(sdid$mdl$coefficients))) {
      stop("This generates named interaction terms that do not appear in the model's coefficients.")
    } else coefs <- prelim_coefs

    # The user specified cohorts and tsi
  } else if(is.null(coefs) &
            !is.null(selection$tsi)) {

    # If selection$times is not null, display a warning
    if(!is.null(selection$times)) {
      warning(paste0("Because `tsi` is specified with values {",
                     paste(selection$tsi, collapse = ", "),
                     "}, all values specified for `times` {",
                     paste(selection$times, collapse = ", "),
                     "} will be ignored."))
    }

    # Restrict tsi dataset to the specified cohorts
    tsi <- sdid$tsi[sdid$tsi$cohort %in% selection$cohorts, ]

    # Restrict tsi dataset to just the requested tsis
    tsi <- tsi[tsi$tsi %in% selection$tsi, ]

    # Exclude referent time periods
    for(cohort_lvl in unique(tsi$cohort)) {
      tsi$time_ref[tsi$cohort == cohort_lvl] <- sdid$cohort$time_refs[[cohort_lvl]]
    }
    tsi <- tsi[tsi$time != tsi$time_ref,]

    # If there are invalid tsis passed through the selection list, display a warning
    for(cohort in unique(tsi$cohort)) {
      invalid_tsi <- selection$tsi[!(selection$tsi %in% tsi[tsi$cohort == cohort, "tsi"])]
      if(length(invalid_tsi) > 0) {
        warning(paste0("Supplied `tsi` value(s) (",
                       paste(invalid_tsi,
                             collapse = ", "),
                       ") are invalid for cohort ", cohort, "."))
      }
    }

    # Now retrieve the values of the relevant interaction terms
    tsi$coefs <- with(tsi,
                       paste0(sdid$cohort$var, "_", cohort,
                              ":",
                              sdid$time$var, "_", time))

    prelim_coefs <- tsi[, "coefs"]

    # Check that all terms appear in the list of coefficients
    if(!all(prelim_coefs %in% names(sdid$mdl$coefficients))) {
      stop("This generates named interaction terms that do not appear in the model's coefficients.")
    } else coefs <- prelim_coefs

  }

  # The user didn't specify enough values in selection list
  else if(is.null(coefs) &
          is.null(selection$times) &
          is.null(selection$tsi)) {
    stop("Must specify one of `coefs`, `times`, or `tsi`.")
  }

  # Check that all terms appear in the list of coefficients
  if(!all(coefs %in% names(sdid$mdl$coefficients))) {
    stop("Selected terms (",
         paste(coefs, collapse = ", "),
         ") do not all appear in the model's list of coefficients.")
  } else return(coefs)
}
