#' Summarize an sdid model
#'
#' @param object A `sdid_mdl` object.
#' @param ... Passed through.
#' @return An object of class `summary.sdid_mdl`.
#' @examples
#' # Fit a staggered difference-in-differences model
#' sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
#'                   df = hosp,
#'                   intervention_var  = "intervention_yr")
#' # Summarize the results
#' summary(sdid_hosp)
#' @exportS3Method summary sdid_mdl
summary.sdid_mdl <- function(object, ...) {
  out <- list(formulas = object$formula,
              r_squared = stats::summary.lm(object$mdl)[["r.squared"]],
              adj_r_squared = stats::summary.lm(object$mdl)[["adj.r.squared"]],
              residuals = stats::summary.lm(object$mdl)[["residuals"]],
              df = stats::summary.lm(object$mdl)[["df"]],
              coefficients = data.frame(term = names(stats::coef(object$mdl)),
                                        estimate = stats::coef(object$mdl),
                                        std_error = sqrt(diag(object$vcov)),
                                        t_value = stats::coef(object$mdl) / sqrt(diag(object$vcov))))
  # Calculate p-values
  out$coefficients$p_value <- 2 * (1 - stats::pt(abs(out$coefficients$t_value), df = out$df[[2]]))


  class(out) <- c("summary.sdid_mdl", class(out))
  return(out)
}

# Pretty printer
#' @exportS3Method print summary.sdid_mdl
print.summary.sdid_mdl <- function(x, precision = 5, ...) {
  # Format top matter
  cat("\nSupplied formula:\n"); print(x$formulas$supplied, showEnv = FALSE)
  cat("\nFitted formula:\n"); print(x$formulas$fitted, showEnv = FALSE)
  cat("\nResiduals:\n")
  print(data.frame(Min = round(min(x$residuals), 4),
                   Q1 = round(stats::quantile(x$residuals, 0.25), 4),
                   Median = round(stats::median(x$residuals), 4),
                   Q3 = round(stats::quantile(x$residuals, 0.75), 4),
                   Max = round(max(x$residuals), 4)), row.names = FALSE)

  # Format coefficients table
  fmt_pval <- function(x) {
    ifelse(abs(x) < 1 / (10^precision),
           sprintf(paste0("%.", precision - 4, "e"), x),  # scientific notation for very small values
           sprintf(paste0("%.", precision, "f"), x))  # fixed 5 decimal places otherwise
  }
  coefs_output <- as.data.frame(x$coefficients, stringsAsFactors = FALSE)
  coefs_output$p_value <- paste0(fmt_pval(coefs_output$p_value))
  coefs_output$` ` <- format(cut(x$coefficients$p_value,
                                 breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                 labels = c("***", "**", "*", ".", ""),
                                 right = FALSE),
                             justify = "left")
  cat("\nCoefficients:\n")
  print.data.frame(coefs_output, row.names = FALSE)
  cat("\nSignificance codes: < 0.001: '***'; < 0.01: '**'; < 0.05: '*'; < 0.1: '.'\n")

  # Format bottom matter
  cat(paste0("Residual standard error: ", round(stats::sd(x$residuals), 4), " on ",
             x$df[[2]], " degrees of freedom\n"))
  cat(paste0("R^2: ", x$r_squared, "; ", "Adjusted R^2: ", x$adj_r_squared, "\n"))
}
