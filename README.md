
# shinystats

<!-- badges: start -->
<!-- badges: end -->

The goal of shinystats is to make shiny apps that explain statistical concepts.
The apps are designed to be light-weight (no extra packages) and fast (base-plot)
so that they can be used in quarto presentations with shiny-live.

## Installation

You can install the development version of shinystats from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("biostats-r/shinystats")
```

Instructions on how to install into a webR session will be added when I manage it!

## Apps so far

### Linear regression

- `coef_explain_app()` - how to interpret the coefficients of a two-way ANOVA 
- `f_test_app()` - interpreting the F statistic
- `one_sided_z_app()` - power of a z-test
- `regression_line_ss_app()` - fit a regression and find the sum of squares 
- `sample_app()` - in development
- `t_test_power_app()` - in development
- `x_distribution_app()` - shows that it is the distribution of residuals that matter

### Autocorrelation

- `ar_app()` - effect of autocorrelation on models
- `acf_app()` - how ACF is calculated
- `acf_pacf_app()` - using ACF and PACF to determine number of AR terms





