#' Identify time period referents within each cohort.
#'
#' @param df A data frame containing the variables in the model.
#' @param cohort_var String specifying the name of the column in `df` that
#' defines the intervention cohorts.
#' @param cohort_ref An optional string specifying the value of `cohort_var` to
#' be used as the referent in the model. If not specified, the value is taken
#' from the first observed value in `cohort_var`.
#' @param time_var String specifying the name of the column in `df` that defines
#' time periods over the study.
#' @param intervention_var String specifying the name of the column in `df` that
#' defines the intervention period. If values of `cohort_var` are named to match
#' values of `time_var`, this parameter is not necessary.
#' @param time_offset Integer specifying which time period relative to the
#' intervention time period should be used as the referent for each cohort.
#' Defaults to -1, which corresponds to the time period immediately preceding
#' intervention.
#'
#' @return list
#' @export pick_time_refs
#' @examples
#' pick_time_refs(hosp, "cohort", "0", "yr", "intervention_yr")

pick_time_refs <- function(df, cohort_var, cohort_ref, time_var, intervention_var = NULL, time_offset = -1) {
  if(!cohort_var %in% names(df)) stop("Invalid cohort_var")
  if(!time_var %in% names(df)) stop("Invalid time_var")

  cohort_lvls <- sort(unique(df[[cohort_var]][df[[cohort_var]] != cohort_ref]))
  time_lvls <- sort(unique(df[[time_var]]))

  # If cohorts are named corresponding to intervention time periods, just use arithmetic to
  # define reference periods for each cohort
  if(all(cohort_lvls %in% time_lvls)) {
    time_refs <- lapply(1:length(cohort_lvls), function(i) {
      time_ref <- as.character(as.integer(as.character(cohort_lvls[[i]])) + time_offset)
      if(time_ref %in% time_lvls) {
        return(time_ref)
      } else {
        stop("No time period level '", time_ref,
             "' corresponding to cohort index ", cohort_lvls[[i]], "\n",
             "Either pass a named list to parameter `time_refs` or ",
             "make sure cohort levels match time period levels.")
      }
    })
    # If cohorts are not named corresponding to intervention time periods, choose the factor
    # level preceding (according to time_offset) the intervention period for each cohort
  } else {
    if(is.null(intervention_var)) {
      stop(paste0("If cohorts are not named according to intervention periods, ",
                  "then `intervention_var` must be specified."))
    } else {
      if(!all(sort(unique(df[[intervention_var]])[!is.na(unique(df[[intervention_var]]))]) %in% time_lvls)) {
        stop(paste0("All values of ", intervention_var, " must match levels of ", time_var, "."))
      } else {
        time_refs <- lapply(cohort_lvls, function(c_lvl) {
          # Identify the index of time_lvls that matches the intervention period for the
          # current cohort
          intervention_index <- which(time_lvls == unique(df[df[[cohort_var]] == c_lvl, intervention_var]))
          time_ref <- time_lvls[[intervention_index + time_offset]]
          if(time_ref %in% time_lvls) {
            return(time_ref)
          } else {
            stop("No time period level '", time_ref,
                 "' corresponding to cohort index ", cohort_lvls[[c_lvl]], "\n",
                 "Either pass a named list to parameter `time_refs` or ",
                 "make sure cohort levels match time period levels.")
          }
        })
      }
    }
  }

  names(time_refs) <- cohort_lvls

  return(time_refs)
}
