#' Hospitalization data
#'
#' A simulated data set of 15 counties, 11 of which implemented a policy
#' intervention during 2015 - 2018 to reduce hospitalizations. The data set is
#' longitudinal, with each row corresponding to an individual-year.
#'
#' Consider a policy intervention designed to reduce inpatient hospitalizations
#' in 15 counties. This longitudinal data set has one row per individual-year.
#' Each individual is identified by a globally unique identifier (`guid`), and
#' we have measures of the individuals' ages, sexes, and comorbidities, and a
#' column indicating whether the individual was hospitalized during the current
#' year.
#'
#' The column `intervention_yr` tells us the year during which each county
#' implemented the intervention. If `intervention_yr` is `NA`, we can conclude
#' that the county never implemented the intervention. Among the 15 counties, 3
#' implemented the intervention in 2015; 2 counties implemented in 2016; 5
#' counties implemented in 2017; 1 county implemented in 2018; and 4 counties
#' did not implement the intervention at all during the study period, which runs
#' for 11 years, from 2010 through 2020.
#'
#'
#' @format ## `hosp`
#' A data frame with 31,040 rows and 10 columns:
#' \describe{
#'   \item{guid}{Character vector containing globally unique identifiers for individuals living in the 15 counties}
#'   \item{county}{Character vector containing county names}
#'   \item{intervention_dt}{Dates on which each county implemented their policy intervention to reduce hospitalizations}
#'   \item{intervention_yr}{Character vector containing the year during which `intervention_dt` takes place}
#'   \item{age}{Integer containing individuals' ages. Time-varying by year.}
#'   \item{sex}{Character vector containing individuals' sexes. Not time-varying.}
#'   \item{comorb}{Logical indicating whether each individual has comorbidities. Time-varying by year.}
#'   \item{cohort}{Character vector identifying the intervention cohort to which each individual belongs. Takes values 0, 5, 6, 7, or 8, corresponding to counties that implemented the intervention not at all or during 2015, 2016, 2017, or 2018, respectively. Invariant within counties.}
#'   \item{yr}{Character vector representing the observation year for each row.}
#'   \item{hospitalized}{Integer indicating whether the individual was hospitalized during the current year.}
#' }
"hosp"

#' Aggregated hospitalization data
#'
#' A simulated data set of 15 counties, 11 of which implemented a policy
#' intervention during 2015 - 2018 to reduce hospitalizations. The data set is
#' longitudinal and aggregated to county-year.
#'
#' Consider a policy intervention designed to reduce inpatient hospitalizations
#' in 15 counties. This longitudinal data set has one row per county-year and
#'  includes aggregated measures of individuals' ages, sexes, and comorbidities,
#'   and a column indicating proportion of individuals who were hospitalized
#'   during the current year.
#'
#' The column `intervention_yr` tells us the year during which each county
#' implemented the intervention. If `intervention_yr` is `NA`, we can conclude
#' that the county never implemented the intervention. Among the 15 counties, 3
#' implemented the intervention in 2015; 2 counties implemented in 2016; 5
#' counties implemented in 2017; 1 county implemented in 2018; and 4 counties
#' did not implement the intervention at all during the study period, which runs
#'  for 11 years, from 2010 through 2020.
#'
#'
#' @format ## `hosp_agg`
#' A data frame with 31,040 rows and 10 columns:
#' \describe{
#'   \item{yr}{Character vector representing the observation year for each row.}
#'   \item{county}{Character vector containing county names}
#'   \item{cohort}{Character vector identifying the intervention cohort to which each county belongs. Takes values 0, 5, 6, 7, or 8, corresponding to counties that implemented the intervention not at all or during 2015, 2016, 2017, or 2018, respectively. Invariant within counties.}
#'   \item{intervention_yr}{Character vector containing the year during which `intervention_dt` takes place}
#'   \item{pct_hospitalized}{Numeric vector containing the proportion of individuals in each county-year who were hospitalized.}
#'   \item{n_enr}{Integer indicating the number of individuals living in each county during the curent year.}
#'   \item{mean_age}{Numeric containing mean ages among individuals living in each county during the current year.}
#'   \item{pct_fem}{Numeric containing the proportion of individuals in each county-year who are female.}
#'   \item{pct_cmb}{Numeric containing the proportion of individuals in each county-year who have comorbidities.}
#' }
"hosp_agg"
