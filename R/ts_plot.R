#' Generates time-series plots, optionally faceted by groups
#'
#' @description
#' Generates time-series plots, optionally faceted by specified groups. The
#' resulting object can be customized using `ggplot2` functions and themes.
#'
#' @param formula An object of class `formula` (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted. The details of
#' model specification are given under 'Details'.
#' @param y Name of the variable in `df` that contains the outcome of interest.
#' If NULL, this is assumed to be the column named in the left-hand side of
#' `formula`.
#' @param group Name of the variable in `df` that contains cohort assignments or
#' other groups by which the plot should be faceted. If NULL, this is assumed to
#' be the first column named in the right-hand side of `formula`. If no
#' `formula` is specified, the resulting plot will aggregate all results into a
#' single panel.
#' @param time_var Name of the variable in `df` that contains time periods. If
#' NULL, this is assumed to be the second column named in the right-hand side of
#' `formula`.
#' @param intervention_var Name of the cohort-level variable in `df` that
#' specifies which values in `time_var` correspond to the first
#' post-intervention time period for each cohort. If NULL, vertical lines
#' indicating the intervention period will be omitted from the plot.
#' @param df A data frame containing the variables in the model.
#' @param tsi An object of class `tsi`, created by `tsi()`, that defines the number
#' of time periods relative to the intervention time period for each cohort
#' observation.
#' @param weights An optional vector of weights to be passed to `lm()` to be
#' used in the fitting process. Should be NULL or a numeric vector.
#' @param ncol Number of columns in the faceted plot
#'
#' @return Returns an object of class "ggplot"
#' @import ggplot2
#' @export ts_plot
#' @examples
#' # Use a formula to specify the setup of the time-series plot. Here we set
#' # hospitalized as the outcome, faceted by county, with yr on the X axis.
#' ts_plot(hospitalized ~ county + yr,
#'         df = hosp,
#'         intervention_var = "intervention_yr")
#'
#' # We can specify the same plot without using a formula.
#' ts_plot(y = "hospitalized",
#'         group = "county",
#'         time_var = "yr",
#'         df = hosp,
#'         intervention_var = "intervention_yr")

ts_plot <- function(formula = NULL,
                    y = NULL,
                    group = NULL,
                    time_var = NULL,
                    intervention_var = NULL,
                    df,
                    tsi = NULL,
                    weights = NULL,
                    ncol = 2) {

  # Make sure df is a data.frame
  df <- as.data.frame(df)

  # Parse formula
  if(!is.null(formula)) {
    # Convert text to formula
    formula <- stats::formula(formula)

    # Define dependent variable from formula
    y <- formula[[2]]

    # Retrieve RHS terms from formula
    trm <- stats::terms(formula)

    ## If `group` is not specified, pull it from the first RHS term in the formula
    if(is.null(group)) group <- attr(trm,  "term.labels")[[1]]

    ## If time_var is not specified, pull it from the second RHS term in the formula
    if(is.null(time_var)) time_var <- attr(trm, "term.labels")[[2]]
  }

  # Validate group, time_var, and intervention_var
  if(!all(c(group, time_var, intervention_var) %in% names(df))) {
    stop("group, time_var, and intervention_var must match column names in df.")
  }

  # If intervention_var is NULL, create a placeholder
  if(is.null(intervention_var)) {
    df$placeholder_intervention <- ""
    intervention_var <- "placeholder_intervention"
  }

  # Make sure intervention_var is consistent within each group
  if(nrow(unique(df[, c(group, intervention_var)])) != length(unique(df[[group]]))) {
    stop("Values of `intervention_var` are not consistent within each group.")
  }

  # Plot with time period on the X axis
  if(is.null(tsi)) {
    # Plot time series for all counties
    agg <- stats::aggregate(stats::as.formula(paste0(y, " ~ ", group, " + ", time_var, " + ", intervention_var)),
                            data = df,
                            FUN = mean)

    df[is.na(df$intervention_yr),]

    agg[[time_var]] <- as.character(agg[[time_var]])
    agg[[intervention_var]] <- as.character(agg[[intervention_var]])

    rtn_plt <- ggplot2::ggplot(data = agg,
                               ggplot2::aes(x = .data[[time_var]], y = .data[[y]], group = .data[[group]])) +
      ggplot2::facet_wrap(~ .data[[group]], ncol = ncol) +
      ggplot2::geom_point(stat = "identity", size = 1) +
      ggplot2::geom_line(stat = "identity", linewidth = 1.0, alpha = 0.8) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = .data[[intervention_var]]),
                          color = "blue") +
      ggplot2::theme_light()
  } else {
    # Plot with time-since-intervention on the X axis, centering all interventions at 0

    # First define a tsi object and merge to df
    df_tsi <- tsi$data

    # If intervention_var is null, make a placeholder
    if(is.null(intervention_var)) {
      df_tsi$placeholder_intervention_time <- ""
      df$placeholder_intervention_time <- ""
      intervention_var <- "placeholder_intervention_time"
    }

    # Make TSI column names for group and time_var match column names in df
    names(df_tsi)[1] <- group
    names(df_tsi)[2] <- time_var
    names(df_tsi)[3] <- intervention_var
    # Remove the `rn` column
    df_tsi$rn <- NULL

    # Merge with df
    agg <- stats::aggregate(stats::as.formula(paste0(y, " ~ ", group, " + tsi")),
                            data = merge(df, df_tsi,
                                         by = c(group, time_var, intervention_var)),
                            FUN = mean)

    # Make the plot
    rtn_plt <- ggplot2::ggplot(data = agg,
                               ggplot2::aes(x = tsi, y = .data[[y]], group = .data[[group]])) +
      ggplot2::facet_wrap(~ .data[[group]], ncol = ncol) +
      ggplot2::geom_point(stat = "identity", size = 1) +
      ggplot2::geom_line(stat = "identity", linewidth = 1.0, alpha = 0.8) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0),
                          color = "blue") +
      theme_light()
  }

  return(rtn_plt)
}
