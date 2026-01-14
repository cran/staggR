#' Identify time-since-intervention
#'
#' @description
#' `id_tsi()` identifies the number of time periods relative to the intervention
#' for each observation. This information is used for plotting and for
#' aggregating model coefficients with `ave_coeff()`.
#'
#' @param df Data frame containing the variables in the model.
#' @param cohort_var Name of the variable in `df` that contains cohort
#' assignments.
#' @param time_var Name of the variable in `df` that contains time periods.
#' @param intervention_var Name of the cohort-level variable in `df` that
#' specifies which values in `time_var` correspond to the first
#' post-intervention time period for each cohort.
#' @return `tsi` Object containing a data frame showing time since intervention
#' for each time period in the data frame for each cohort in the data frame.
#' @export id_tsi
#' @examples
#' # Generate a tsi object, containing a data frame showing the time since
#' # intervention (TSI value) for each time period in the data frame for each
#' # cohort.
#' id_tsi(hosp,
#'        cohort_var = "cohort",
#'        time_var = "yr",
#'        intervention_var = "intervention_yr")

id_tsi <- function(df,
                   cohort_var,
                   time_var,
                   intervention_var) {

  # Identify time since intervention
  # Create a dataset that contains all the unique values of cohort_var,
  # time_var, and tsi_var.
  tsi <- unique(df[order(df[[cohort_var]], df[[time_var]]),
                   c(cohort_var, time_var, intervention_var)])

  # Change column names so we don't have to keep referencing them dynamically
  colnames(tsi) <- c("cohort", "time", "intervention_time")

  # Number the rows within each cohort
  tsi$rn <- stats::ave(
    seq_len(nrow(tsi)), # the sequence to number
    tsi$cohort,         # grouping variable
    FUN = seq_along     # restart sequence for each group
  )

  # Identify the value of time_var that matches the value of tsi_var for
  # each cohort
  tsi$tsi <-
    with(tsi,
         rn[match(paste0(cohort, "_", intervention_time),
                  paste0(cohort, "_", time))])

  # To calculate time since intervention, subtract tsi from rn
  tsi$tsi <- tsi$rn - tsi$tsi

  # Remove the rn column
  tsi$rn <- NULL

  return(new_tsi(tsi))
}
