
<!-- README.md is generated from README.Rmd. Please edit that file -->

# staggR

<!-- badges: start -->

<!-- badges: end -->

staggR makes it easy to fit linear difference-in-differences models
where interventions are staggered over time across multiple cohorts. The
approach uses cohort- and time-since-treatment specific
difference-in-differences parameters, and it provides convenience
functions both for specifying the model and for flexibly aggregating
coefficients to answer a variety of research questions.

## Installation

``` r
install.packages("staggR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(staggR)

#' Fit the staggered DiD model
sdid_hosp <- staggR::sdid(hospitalized ~ cohort + yr,
                          df = hosp,
                          intervention_var  = "intervention_yr",
                          .vcov = sandwich::vcovCL,
                          cluster = hosp$county)

summary(sdid_hosp)
#> 
#> Supplied formula:
#> hospitalized ~ cohort + yr
#> 
#> Fitted formula:
#> hospitalized ~ cohort_5 + cohort_6 + cohort_7 + cohort_8 + yr_2011 + 
#>     yr_2012 + yr_2013 + yr_2014 + yr_2015 + yr_2016 + yr_2017 + 
#>     yr_2018 + yr_2019 + yr_2020 + cohort_5:yr_2010 + cohort_5:yr_2011 + 
#>     cohort_5:yr_2012 + cohort_5:yr_2013 + cohort_5:yr_2015 + 
#>     cohort_5:yr_2016 + cohort_5:yr_2017 + cohort_5:yr_2018 + 
#>     cohort_5:yr_2019 + cohort_5:yr_2020 + cohort_6:yr_2010 + 
#>     cohort_6:yr_2011 + cohort_6:yr_2012 + cohort_6:yr_2013 + 
#>     cohort_6:yr_2014 + cohort_6:yr_2016 + cohort_6:yr_2017 + 
#>     cohort_6:yr_2018 + cohort_6:yr_2019 + cohort_6:yr_2020 + 
#>     cohort_7:yr_2010 + cohort_7:yr_2011 + cohort_7:yr_2012 + 
#>     cohort_7:yr_2013 + cohort_7:yr_2014 + cohort_7:yr_2015 + 
#>     cohort_7:yr_2017 + cohort_7:yr_2018 + cohort_7:yr_2019 + 
#>     cohort_7:yr_2020 + cohort_8:yr_2010 + cohort_8:yr_2011 + 
#>     cohort_8:yr_2012 + cohort_8:yr_2013 + cohort_8:yr_2014 + 
#>     cohort_8:yr_2015 + cohort_8:yr_2016 + cohort_8:yr_2018 + 
#>     cohort_8:yr_2019 + cohort_8:yr_2020
#> 
#> Residuals:
#>      Min      Q1  Median     Q3  Max
#>  -0.8007 -0.3788 -0.2139 0.4595 0.84
#> 
#> Coefficients:
#>              term     estimate   std_error     t_value p_value    
#>       (Intercept)  0.228155340 0.031307912   7.2874659 3.2e-13 ***
#>          cohort_5  0.088156223 0.028579679   3.0845771 0.00204 ** 
#>          cohort_6 -0.159451272 0.016375109  -9.7374175 0.0e+00 ***
#>          cohort_7  0.075133698 0.018413395   4.0803827 0.00005 ***
#>          cohort_8 -0.237490795 0.003992926 -59.4778926 0.0e+00 ***
#>           yr_2011  0.055426750 0.027992651   1.9800465 0.04771 *  
#>           yr_2012  0.025200365 0.048720589   0.5172426 0.60499    
#>           yr_2013  0.047663300 0.034037028   1.4003367 0.16142    
#>           yr_2014  0.110034046 0.035770010   3.0761536 0.00210 ** 
#>           yr_2015  0.130370006 0.039124167   3.3322116 0.00086 ***
#>           yr_2016  0.150645703 0.037030770   4.0681224 0.00005 ***
#>           yr_2017  0.193950839 0.030086602   6.4464189 1.2e-10 ***
#>           yr_2018  0.198984915 0.042004187   4.7372638 2.2e-06 ***
#>           yr_2019  0.239084188 0.032639986   7.3248863 2.4e-13 ***
#>           yr_2020  0.226537217 0.032775690   6.9117451 4.9e-12 ***
#>  cohort_5:yr_2010 -0.075771022 0.048141117  -1.5739357 0.11551    
#>  cohort_5:yr_2011 -0.073492699 0.044324381  -1.6580649 0.09731 .  
#>  cohort_5:yr_2012  0.008214848 0.029858839   0.2751228 0.78322    
#>  cohort_5:yr_2013  0.018970874 0.035225443   0.5385560 0.59020    
#>  cohort_5:yr_2015  0.031183015 0.047308110   0.6591473 0.50981    
#>  cohort_5:yr_2016  0.073549064 0.039113486   1.8804016 0.06006 .  
#>  cohort_5:yr_2017  0.111184626 0.039801432   2.7934831 0.00522 ** 
#>  cohort_5:yr_2018  0.181886621 0.033410927   5.4439263 5.3e-08 ***
#>  cohort_5:yr_2019  0.188240613 0.019982991   9.4200420 0.0e+00 ***
#>  cohort_5:yr_2020  0.257882482 0.043630603   5.9105871 3.4e-09 ***
#>  cohort_6:yr_2010  0.192165497 0.040766852   4.7137683 2.4e-06 ***
#>  cohort_6:yr_2011  0.083833784 0.021768788   3.8511001 0.00012 ***
#>  cohort_6:yr_2012  0.130360273 0.017418812   7.4838784 7.4e-14 ***
#>  cohort_6:yr_2013  0.103217201 0.054669436   1.8880239 0.05903 .  
#>  cohort_6:yr_2014  0.019231429 0.018934185   1.0156988 0.30978    
#>  cohort_6:yr_2016  0.039597597 0.014527663   2.7256687 0.00642 ** 
#>  cohort_6:yr_2017 -0.017621795 0.045720916  -0.3854209 0.69993    
#>  cohort_6:yr_2018 -0.065811049 0.024388171  -2.6984823 0.00697 ** 
#>  cohort_6:yr_2019 -0.071375212 0.028617604  -2.4941016 0.01263 *  
#>  cohort_6:yr_2020 -0.083590800 0.043178676  -1.9359278 0.05289 .  
#>  cohort_7:yr_2010 -0.028841089 0.040446429  -0.7130688 0.47581    
#>  cohort_7:yr_2011 -0.097944813 0.025984565  -3.7693458 0.00016 ***
#>  cohort_7:yr_2012 -0.046622258 0.030682909  -1.5194862 0.12865    
#>  cohort_7:yr_2013 -0.036083533 0.037344341  -0.9662383 0.33393    
#>  cohort_7:yr_2014 -0.053870348 0.028528256  -1.8883155 0.05899 .  
#>  cohort_7:yr_2015 -0.042354696 0.030159042  -1.4043780 0.16022    
#>  cohort_7:yr_2017  0.123013287 0.013601784   9.0439086 0.0e+00 ***
#>  cohort_7:yr_2018  0.131938968 0.032104648   4.1096531 0.00004 ***
#>  cohort_7:yr_2019  0.159289976 0.015694481  10.1494260 0.0e+00 ***
#>  cohort_7:yr_2020  0.189118922 0.037494013   5.0439765 4.6e-07 ***
#>  cohort_8:yr_2010  0.169335455 0.030086602   5.6282679 1.8e-08 ***
#>  cohort_8:yr_2011  0.162863929 0.014472854  11.2530626 0.0e+00 ***
#>  cohort_8:yr_2012  0.231193914 0.024009151   9.6294083 0.0e+00 ***
#>  cohort_8:yr_2013  0.185971220 0.018317313  10.1527563 0.0e+00 ***
#>  cohort_8:yr_2014  0.166476981 0.011655637  14.2829587 0.0e+00 ***
#>  cohort_8:yr_2015  0.050939971 0.009977242   5.1056167 3.3e-07 ***
#>  cohort_8:yr_2016  0.059839177 0.007804591   7.6671765 1.8e-14 ***
#>  cohort_8:yr_2018  0.009379666 0.013978605   0.6710016 0.50222    
#>  cohort_8:yr_2019  0.036917934 0.003425534  10.7772793 0.0e+00 ***
#>  cohort_8:yr_2020 -0.003312873 0.016633407  -0.1991698 0.84213    
#> 
#> Significance codes: < 0.001: '**'; < 0.01: '**'; < 0.05: '*'; < 0.1: '.'
#> Residual standard error: 0.4632 on 30985 degrees of freedom
#> R^2: 0.118875596336873; Adjusted R^2: 0.117339991437799

#' What is the effect of the intervention in the post-intervention period?
ave_coeff(sdid = sdid_hosp, 
          coefs = select_period(sdid_hosp, period = "post"))
#>          est        se         pval sign         lb        ub     n
#> 1 0.09962262 0.0159212 3.890523e-10  *** 0.06841706 0.1308282 11745
```

## Getting help

If you encounter a problem, please file an issue with a minimal
reproducible example on [GitHub](https://github.com/chse-ohsu/staggR).
