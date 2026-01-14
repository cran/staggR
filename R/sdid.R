#' Fit a staggered difference-in-differences model
#'
#' @description
#' Fits a linear staggered difference-in-differences model, following the
#' Abraham and Sun (2018) approach. It facilitates optional weighting and
#' user-specified variance-covariance function.
#'
#' @details
#' Fitting a staggered difference-in-differences model requires deliberate
#' attention to two specific independent variables:
#'
#' * The intervention cohort column assigns a cohort name to all individuals or groups having the the intervention during the same time period. For example, if the longitudinal data is at the year level, ranging from 2010 to 2020, and it contains 15 counties, 3 of whom implemented the intervention of interest in 2015, those 3 counties would be assigned to the same cohort. Similarly, if 2 more counties implemented the intervention in 2016, those 2 counties would be assigned to the next cohort.
#' * The time period column assigns each observation to a time period at the most granular level of the longitudinal data. In the example described above, these values would correspond to the years 2010, ..., 2020.
#'
#' To specify a model, a formula is passed following the format `response ~ cohort_var + time_var + covariates`. This, however, is not the formula use to fit the model; `sdid()` expands this formula to include main effects and every possible interaction between `cohort_var` and `time_var`, excluding referents for identification:
#'
#' * Referents for main effects are either the first levels `cohort_var` and `time_var` or the referents specified in `cohort_ref` and `time_ref`.
#' * Referents for cohort-time interactions are either the factor level of `time_var` that immediately precedes the value of `intervention_var` within each cohort or the referents specified in `cohort_time_refs`.
#'
#' `sdid()` also accommodates aggregated data through the `weights` argument.
#'
#' @references
#' Abraham S, Sun L. Estimating Dynamic Treatment Effects in Event Studies with Heterogeneous Treatment Effects. MIT; 2018.
#'
#' @param formula An object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted. The details of
#' model specification are given under 'Details'.
#' @param df A data frame containing the variables in the model.
#' @param weights An optional vector of weights to be passed to `stats::lm()` to
#' be used in the fitting process. Should be NULL or a numeric vector.
#' @param cohort_var Name of the variable in `df` that contains cohort
#' assignments. If NULL, this is assumed to be the first column named in the
#' right hand side of `formula`.
#' @param cohort_ref Value of `cohort_var` that serves as the referent for main
#' effects for cohorts. If NULL, this is assumed to the be the first value in
#' the set of values for `cohort_var`.
#' @param cohort_time_refs A list, whose elements are named to match levels of
#' `cohort_var`, specifying the value of `time_var` that serves as the referent
#' for each time interaction with values of `cohort_var`. See 'Details.'
#' @param time_var Name of the variable in `df` that contains time periods. If
#' NULL, this is assumed to be the second column named in the right hand side of
#' `formula`.
#' @param time_ref Value of `time_var` that serves as the referent for main
#' effects for time periods. If NULL, this is assumed to the be the first value
#' in the set of values for `time_var`.
#' @param intervention_var Name of the cohort-level variable in `df` that
#' specifies which values in `time_var` correspond to the first
#' post-intervention time period for each cohort.
#' @param .vcov Function to be used to estimate the variance-covariance matrix.
#' Defaults to stats::vcov.
#' @param ... Additional arguments to be passed to `.vcov`.
#'
#' @return Returns an object of class `sdid`, which is a list containing the
#' following components:
#'
#' mdl
#' : The `lm` object returned from the call to `stats::lm()` in `sdid()`
#'
#' formula
#' : A list object containing both the original formula specified in the call to `sdid()` and the generated formula, with all cohort-time interactions, passed to `stats::lm()` to fit the model
#'
#' vcov
#' : The variance-covariance matrix used to estimate standard errors
#'
#' tsi
#' : The time-since-intervention dataset used to enumerate time periods relative to the intervention period for each cohort
#'
#' obs_cnt
#' : Counts of observations within each cohort-time interaction
#' cohort
#' : A list object containing details about cohorts. `var` contains the name of the column in `df` that identifies cohorts; `ref` contains the value of the cohort column that functions as the referent for main effects; and `time_refs` contains the referent time values within each cohort for each set of cohort-time interactions.
#'
#' time
#' : A list object containing `var`, which is the name of the column in `df` identified by the `sdid()` argument `time_var`, and `ref`, the referent value of `time_var` for main effects.
#'
#' intervention_var
#' : Name of the column in `df` that contains the time period during which each cohort implemented the intervention of interest
#'
#' covariates
#' : A character vector containing the terms in `formula` other than those corresponding to cohorts and time periods
#'

#' @export sdid
#' @examples
#' # Fit a staggered difference-in-differences model
#' sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
#'                   df = hosp,
#'                   intervention_var  = "intervention_yr")
#' summary(sdid_hosp)


sdid <- function(formula,
                 df,
                 weights = NULL,
                 cohort_var = NULL, cohort_ref = NULL, cohort_time_refs = NULL,
                 time_var = NULL, time_ref = NULL,
                 intervention_var, .vcov = stats::vcov, ...) {

  # Make sure df is a data.frame
  df <- as.data.frame(df)

  # Define dependent variable from formula
  formula <- stats::formula(formula)
  y <- formula[[2]]

  # Retrieve RHS terms from formula
  trm <- stats::terms(formula)

  ## If cohort_var is not specified, pull it from the first RHS term in the formula
  if(is.null(cohort_var)) cohort_var <- attr(trm,  "term.labels")[[1]]

  ## If time_var is not specified, pull it from the second RHS term in the formula
  if(is.null(time_var)) time_var <- attr(trm, "term.labels")[[2]]

  ## Pull covariates from the remaining RHS terms in the formula
  covariates <-
    attr(trm, "term.labels")[!(attr(trm, "term.labels") %in% c(cohort_var, time_var))]

  # Validate cohort_var, time_var, intervention_var, and covariates
  if(!all(c(cohort_var, time_var, intervention_var, covariates) %in% names(df))) {
    stop("cohort_var, time_var, intervention_var, and all covariates must match column names in df.")
  }

  # Make sure intervention_var is consistent within each cohort
  if(nrow(unique(df[, c(cohort_var, intervention_var)])) != length(unique(df[[cohort_var]]))) {
    stop("Values of `intervention_var` are not consistent within each cohort.")
  }

  # If cohort_ref is not specified, choose the first level
  if(is.null(cohort_ref)) cohort_ref <- levels(factor(df[[cohort_var]]))[[1]]

  # If time_ref is not specified, choose the first level
  if(is.null(time_ref)) time_ref <- levels(factor(df[[time_var]]))[[1]]

  # Validate that there are no NAs in cohort_var or time_var; omit rows with any NAs
  if((n_missing_cohort_var <- nrow(df[is.na(df[[cohort_var]]), ])) > 0) {
    warning(n_missing_cohort_var, " observations deleted due to missing values in `", cohort_var, "`.")
    df <- df[!is.na(df[[cohort_var]])]
  }
  if((n_missing_time_var <- nrow(df[is.na(df[[time_var]]), ])) > 0) {
    warning(n_missing_time_var, " observations deleted due to missing values in `", time_var, "`.")
    df <- df[!is.na(df[[time_var]])]
  }

  # Prepare data by creating dummy variables
  df_prepped <- prep_data(df = df,
                          cohort_var = cohort_var,
                          time_var = time_var)

  # Define dummy variables
  cohort_dummies <- grep(paste0(cohort_var, "_"), names(df_prepped), value = TRUE)
  cohort_lvls <- sub(paste0(cohort_var, "_"), "", cohort_dummies)
  time_dummies <- grep(paste0(time_var, "_"), names(df_prepped), value = TRUE)
  time_lvls <- sub(paste0(time_var, "_"), "", time_dummies)

  # Gather counts of observations for all possible cohort-time interactions
  obs_cnt <- expand.grid(cohort_dummies, time_dummies)
  colnames(obs_cnt) <- c("cohort", "time")
  obs_cnt$n_obs <- mapply(function(cohort, time) {
    if(is.null(weights)) {
      nrow(df_prepped[eval(parse(text = paste0("df_prepped$", cohort, "==1 & ",
                                               "df_prepped$", time, "==1"))), ])
    } else {
      sum(df_prepped[eval(parse(text = paste0("df_prepped$", cohort, "==1 & ",
                                              "df_prepped$", time, "==1"))), weights])
    }
  },
  obs_cnt$cohort, obs_cnt$time)

  # Define time period referents, if not specified in arguments
  if(is.null(cohort_time_refs)) {
    cohort_time_refs <- pick_time_refs(df = df,
                                       cohort_var = cohort_var,
                                       cohort_ref = cohort_ref,
                                       time_var = time_var,
                                       intervention_var = intervention_var)
  }

  # Check that cohort_time_refs is a list object corresponding to cohort levels
  if(!inherits(cohort_time_refs, "list") | any(sort(as.integer(as.character(names(cohort_time_refs)))) !=
                                               sort(as.integer(as.character(cohort_lvls))))) {
    stop("cohort_time_refs must be a list object with elements named to match the levels of cohort_var.")
  }

  # Define the regression formula
  fml <- stats::formula(
    paste0(y, " ~ ",
           paste(
             c(cohort_dummies, # Fixed effects for cohorts,
               time_dummies[time_dummies != paste0(time_var, "_", time_ref)], # Fixed effects for time periods, exclude referent
               unlist(lapply(1:length(cohort_dummies), function(x) { # Fixed effects for cohort-time interactions
                 paste(cohort_dummies[[x]],
                       time_dummies[time_dummies != paste0(time_var, "_",
                                                           cohort_time_refs[names(cohort_time_refs) == cohort_lvls[[x]]])],
                       sep = ":")
               })),
               covariates),
             collapse = " + "))
  )

  # Fit model
  ## Unweighted
  if(is.null(weights)) {
    mdl <- stats::lm(formula = fml,
                     data = df_prepped)
  } else {
    ## Weighted
    if(!(weights %in% colnames(df_prepped))) {
      stop("Supplied `weights` column `",
           weights,
           "` does not exist in ",
           deparse(substitute(df)), ".")
    } else {
      mdl <- stats::lm(formula = fml,
                       data = df_prepped,
                       weights = df_prepped[[weights]])
    }
  }
  vcv <- .vcov(mdl, ...)

  # Identify time since intervention
  # Create a dataset that contains all the unique values of cohort_var,
  # time_var, and tsi_var.
  tsi <- id_tsi(df = df,
                cohort_var = cohort_var,
                time_var = time_var,
                intervention_var = intervention_var)

  # Create return object
  rslts <- new_sdid(mdl = mdl,
                    formula = list(supplied = formula,
                                   fitted = fml),
                    vcov = vcv,
                    tsi = tsi[["data"]],
                    obs_cnt = obs_cnt,
                    cohort = list(var = cohort_var,
                                  ref = cohort_ref,
                                  time_refs = cohort_time_refs),
                    time = list(var = time_var,
                                ref = time_ref),
                    intervention_var = intervention_var,
                    covariates = covariates)
  return(rslts)
}
