#' Prepare a data frame to work with sdid() function
#'
#' @param df A data frame containing the variables in the model.
#' @param y Name of outcome variable in `df.
#' @param intervention_var Name of the cohort-level variable in `df` that
#' specifies which values in `time_var` correspond to the first
#' post-intervention time period for each cohort.
#' @param cohort_var String specifying the name of the column in `df` that
#' defines the intervention cohorts.
#' @param covariates Character vector containing the names of covariates to
#' adjust the regression.
#' @param cohort_ref An optional string specifying the value of `cohort_var`
#' to be used as the referent in the model. If not specified, the value is taken
#' from the first observed value in `cohort_var`.
#' @param time_var String specifying the name of the column in `df` that defines
#' time periods over the study.
#' @param weights Optional column name containing the counts of observations to
#' be used as regression weights.
#' @return data.frame
#' @export prep_data
#'
#' @examples
#' dta_prepped <- prep_data(hosp,
#'                          y = "hospitalized",
#'                          intervention_var = "intervention_yr",
#'                          cohort_var = "cohort",
#'                          cohort_ref = "0",
#'                          time_var = "yr")
#' head(dta_prepped)

prep_data <- function(df, y, intervention_var, cohort_var, covariates=NULL, cohort_ref=NULL, time_var, weights=NULL) {
  # Make sure df is a data.frame
  df <- as.data.frame(df)

  # Validate cohort_var and time_var names
  if(!cohort_var %in% colnames(df)) {
    stop(paste0("Column '", cohort_var, "' not found in data."))
  }
  if(length(levels(factor(df[[cohort_var]]))) < 3) {
    stop(paste0("Column '", cohort_var, "' must contain at least 3 levels."))
  }
  if(!time_var %in% colnames(df)) {
    stop(paste0("Column '", time_var, "' not found in data."))
  }
  if(length(levels(factor(df[[time_var]]))) < 3) {
    stop(paste0("Column '", time_var, "' must contain at least 3 levels."))
  }

  # Check that there are no column name similarities that will blow up the sdid() function
  if(any(grepl(pattern = paste0("^", cohort_var),
               x = colnames(df[, c(y, intervention_var, covariates)])))) {
    stop("Column names are too similar. The column names for the outcome, ",
         "intervention variable, and covariates cannot start with the column ",
         "name for the time period or cohort variables.")
  }

  # If cohort_var or time_var are not factors, convert them to factors
  if(!inherits(df[[cohort_var]], "factor")) df[[cohort_var]] <- factor(df[[cohort_var]])
  if(!inherits(df[[time_var]], "factor")) df[[time_var]] <- factor(df[[time_var]])

  # Sanitize cohort and time factor levels
  # levels(df[[time_var]]) <- gsub("-", "", levels(df[[time_var]]))
  # levels(df[[cohort_var]]) <- gsub("-", "", levels(df[[time_var]]))
  levels(df[[time_var]]) <- make.names(levels(df[[time_var]]), unique = TRUE)
  levels(df[[cohort_var]]) <- make.names(levels(df[[cohort_var]]), unique = TRUE)

  # Identify referents if not passed through params
  if(is.null(cohort_ref)) {
    cohort_ref <- levels(factor(df[[cohort_var]]))[[1]]
  }

  # # Identify all levels of cohort variables and exclude the referents
  # cohort_lvls <- levels(factor(df[[cohort_var]][df[[cohort_var]] != make.names(cohort_ref)]))
  #
  # # Identify all levels of time variables
  # time_lvls <- levels(factor(df[[time_var]]))

  # Create dummy variables for cohorts
  cohort_dummies <- stats::model.matrix(stats::reformulate(cohort_var, intercept = FALSE, response = NULL),
                                        data = df)

  # Add an underscore between the cohort column name and the cohort number
  colnames(cohort_dummies) <- gsub(cohort_var, paste0(cohort_var, "_"), colnames(cohort_dummies))

  # Create dummy variables for time periods
  time_dummies <- stats::model.matrix(stats::reformulate(time_var, intercept = FALSE, response = NULL),
                                      data = df)

  # Add an underscore between the time period column name and the time period number
  colnames(time_dummies) <- gsub(time_var, paste0(time_var, "_"), colnames(time_dummies))

  # Drop reference groups from cohort
  cohort_dummies <- cohort_dummies[ , colnames(cohort_dummies) != paste0(make.names(cohort_var), "_", make.names(cohort_ref))]

  # Convert to integer
  cohort_dummies <- apply(cohort_dummies, 2, as.integer)
  time_dummies <- apply(time_dummies, 2, as.integer)

  # Combine with original data, omitting the original cohort and time variables
  return(cbind(df[, c(y, intervention_var, covariates, weights)],
               cohort_dummies,
               time_dummies))
}
