## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

# Check for sandwich package
if (!requireNamespace("sandwich", quietly = TRUE)) {
stop("Package 'sandwich' needed for this vignette to work. Please install it.")
}

## ----setup--------------------------------------------------------------------
data("hosp", package = "staggR")
head(hosp, 20)

## -----------------------------------------------------------------------------
with(hosp,
     table(county, intervention_yr, useNA = "ifany"))

## -----------------------------------------------------------------------------
county_cohorts <- unique(hosp[, c("county", "cohort", "intervention_yr")])
county_cohorts[order(county_cohorts$cohort), ]

## -----------------------------------------------------------------------------
stats::xtabs(hospitalized ~ county + yr, 
             data = hosp)

## ----tsplot, fig.width = 10, fig.height = 10, out.width = "100%"--------------
staggR::ts_plot(hospitalized ~ county + yr,
                df = hosp,
                intervention_var = "intervention_yr")

## ----tsplot_alt, fig.width = 10, fig.height = 10, out.width = "100%"----------
staggR::ts_plot(y = "hospitalized",
                group = "county",
                time_var = "yr",
                df = hosp,
                intervention_var = "intervention_yr")

## -----------------------------------------------------------------------------
table(hosp$hospitalized, useNA = "always")
table(hosp$county, useNA = "always")
table(hosp$yr, useNA = "always")
table(hosp$intervention_yr, useNA = "always")

## ----tsplot2, fig.width = 10, fig.height = 10, out.width = "100%"-------------
hosp2 <- hosp
hosp2$intervention_yr <- ifelse(is.na(hosp2$intervention_yr),
                                yes = "Comparison group",
                                no = hosp2$intervention_yr)
county_plot <- staggR::ts_plot(hospitalized ~ county + yr,
                               df = hosp2,
                               intervention_var = "intervention_yr")
county_plot

## -----------------------------------------------------------------------------
hosp_tsi <- staggR::id_tsi(df = hosp2,
                           cohort_var = "county",
                           time_var = "yr",
                           intervention_var = "intervention_yr")
head(hosp_tsi$data, 11)

## ----tsplot3, fig.width = 10, fig.height = 10, out.width = "100%"-------------
county_plot <- staggR::ts_plot(hospitalized ~ county + yr,
                               df = hosp2,
                               intervention_var = "intervention_yr", 
                               tsi = hosp_tsi)
county_plot

## ----tsplot4, fig.width = 10, fig.height = 10, out.width = "100%"-------------
library(ggplot2)
county_plot +
  scale_x_continuous(name = "Years", 
                     breaks = seq(-8, 5, by = 2)) +
  scale_y_continuous(name = "Percent hospitalized",
                     breaks = seq(0, 1, by = 0.2),
                     labels = function(x) paste0(round(x * 100), "%")) +
  theme_classic() +
  theme(panel.grid.major.x = element_line(linewidth = 0.5))

## -----------------------------------------------------------------------------
sdid_hosp <- staggR::sdid(hospitalized ~ cohort + yr + age + sex + comorb,
                          df = hosp,
                          intervention_var  = "intervention_yr")

summary(sdid_hosp)

## -----------------------------------------------------------------------------
names(sdid_hosp)
sdid_hosp$formula

## -----------------------------------------------------------------------------
sdid_hosp_earlier_timeref <- staggR::sdid(hospitalized ~ cohort + yr + age + sex + comorb,
                                          df = hosp,
                                          cohort_time_refs = list(`5` = "2013",
                                                                  `6` = "2014",
                                                                  `7` = "2015",
                                                                  `8` = "2016"),
                                          intervention_var  = "intervention_yr")

## -----------------------------------------------------------------------------
# Identify the coefficients that compose the effect of the intervention during the 
# post-intervention period
(post_coefs <- staggR::select_period(sdid = sdid_hosp, 
                                     period = "post"))

## -----------------------------------------------------------------------------
staggR::ave_coeff(sdid = sdid_hosp,
                  coefs = post_coefs)

## -----------------------------------------------------------------------------
# Identify the coefficients that represent the difference between exposure groups 
# during the pre-intervention period
(pre_coefs <- staggR::select_period(sdid = sdid_hosp, 
                                    period = "pre"))

staggR::ave_coeff(sdid = sdid_hosp,
                  coefs = pre_coefs)

## -----------------------------------------------------------------------------
# Identify the coefficients for the effect of the intervention on cohorts 5 and 6 
# during the post-intervention period
(post_coefs_cohort5 <- staggR::select_period(sdid = sdid_hosp, 
                                             period = "post",
                                             cohorts = c("5", "6")))

staggR::ave_coeff(sdid = sdid_hosp,
                  coefs = post_coefs_cohort5)


## -----------------------------------------------------------------------------
# Identify the coefficients for the effect of the intervention on cohorts 7 and 8 
# during the post-intervention period
(post_coefs_cohort8 <- staggR::select_period(sdid = sdid_hosp, 
                                             period = "post",
                                             cohorts = c("7", "8")))

staggR::ave_coeff(sdid = sdid_hosp,
                  coefs = post_coefs_cohort8)

## -----------------------------------------------------------------------------
# Identify the coefficients corresponding to all cohorts in 2018
(terms_2018 <- staggR::select_terms(sdid = sdid_hosp,
                                    selection = list(times = "2018")))

## -----------------------------------------------------------------------------
staggR::ave_coeff(sdid = sdid_hosp,
                  coefs = terms_2018)

## -----------------------------------------------------------------------------
(terms_2018_cohorts56 <- staggR::select_terms(sdid = sdid_hosp,
                                              selection = list(cohorts = c("5", "6"),
                                                               times = "2018")))

staggR::ave_coeff(sdid = sdid_hosp,
                  coefs = terms_2018_cohorts56)

## -----------------------------------------------------------------------------
(terms_custom <- staggR::select_terms(sdid = sdid_hosp,
                                      coefs = c("cohort_X5:yr_X2018", "cohort_X6:yr_X2018")))

staggR::ave_coeff(sdid = sdid_hosp,
                  coefs = terms_custom)

## -----------------------------------------------------------------------------
# Fit SDID model with standard errors clustered at the county level
sdid_hosp <- staggR::sdid(hospitalized ~ cohort + yr + age + sex + comorb,
                          df = hosp,
                          intervention_var  = "intervention_yr",
                          .vcov = sandwich::vcovCL,
                          cluster = hosp$county)
summary(sdid_hosp)

# Examine the association between the intervention and risk of hospitalization in 
# the post-intervention period
staggR::ave_coeff(sdid = sdid_hosp, 
                  coefs = staggR::select_period(sdid = sdid_hosp,
                                                period = "post"))

## -----------------------------------------------------------------------------
data("hosp_agg", package = "staggR")
head(hosp_agg)

# This data set contains one row per county-year
nrow(hosp_agg); nrow(unique(hosp_agg[,c("yr", "county")]))

## -----------------------------------------------------------------------------
sdid_hosp_agg <- staggR::sdid(pct_hospitalized ~ cohort + yr + mean_age + pct_fem + pct_cmb,
                              df = hosp_agg,
                              weights = "n_enr",
                              intervention_var  = "intervention_yr",
                              # Cluster standard errors at the county level
                              .vcov = sandwich::vcovCL,
                              cluster = hosp_agg$county)

## -----------------------------------------------------------------------------
# Summarize the original model
summary(sdid_hosp)

# Summarize the model based on aggregated data
summary(sdid_hosp_agg)

## -----------------------------------------------------------------------------
# Summarize the post-intervention period from the original model
summary_sdid_hosp <- 
  staggR::ave_coeff(sdid = sdid_hosp, 
                    coefs = staggR::select_period(sdid = sdid_hosp,
                                                  period = "post"))


# # Summarize the post-intervention period from the model fitted using aggregated data
summary_sdid_hosp_agg <- 
  staggR::ave_coeff(sdid = sdid_hosp_agg, 
                    coefs = staggR::select_period(sdid = sdid_hosp_agg,
                                                  period = "post"))

# Combine the two summaries
summary_sdid_hosp$model <- "Individual-level data"
summary_sdid_hosp_agg$model <- "Aggregated data"
rbind(summary_sdid_hosp, summary_sdid_hosp_agg)

## -----------------------------------------------------------------------------
# First examine the model we fit using the original outcome
sdid_hosp$formula$supplied

# Calculate de-trending adjustments
hosp_det <- staggR::detrend(sdid = sdid_hosp,
                            df = hosp)

# Then refit the same model, substituting the _detrended version of the outcome
sdid_hosp_det <- staggR::sdid(hospitalized_detrended ~ cohort + yr + age + sex + comorb,
                              df = hosp_det,
                              intervention_var = "intervention_yr")

# Organize and present results, comparing them to the non-trend-adjusted model
original_rslts <- staggR::ave_coeff(sdid = sdid_hosp,
                                    coefs = staggR::select_period(sdid = sdid_hosp, 
                                                                  period = "post"))
original_rslts$model = "Original"
original_rslts <- original_rslts[, c("model", "est", "se", "pval", "lb", "ub", "n")]

detrend_rslts <- staggR::ave_coeff(sdid = sdid_hosp_det,
                                   coefs = staggR::select_period(sdid = sdid_hosp_det, 
                                                                 period = "post"))
detrend_rslts$model = "Trend-adjusted"
detrend_rslts <- detrend_rslts[, c("model", "est", "se", "pval", "lb", "ub", "n")]

rbind(original_rslts, detrend_rslts)


