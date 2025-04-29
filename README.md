
# shinystats

<!-- badges: start -->
<!-- badges: end -->

The goal of shinystats is to make shiny apps that explain statistical concepts.
The apps are designed to be light-weight (no extra packages) and fast
so that they can be used in quarto presentations with [shinylive](https://posit-dev.github.io/r-shinylive/).

## Installation and Use

You can install shinystats from [GitHub](https://github.com/) with: 

``` r
# install.packages("pak")
pak::pak("biostats-r/shinystats")
```

To use shinystats in a quarto document with shinylive, you also need to do the following:

1) Install `shinylive` from CRAN with `install.packages("shinylive")`

2) Install shinylive assets with `shinylive::assets_ensure()`

3) Add the following to the YAML header of your quarto document:
 
```yaml
format: 
  html:
    resources: 
      - shinylive-sw.js
filters:
  - shinylive
```

Shinylive also works with revealjs presentations.
Simply substitute `html` with `revealjs` in the YAML header.


4) Add a shinylive-r code chunk that runs the app:

````
```{shinylive-r}
#| label: power-app
#| standalone: true
#| viewerHeight: 600

shinystats::ar_app()
```
````

5) Render the document


### Example

Here is a sample quarto document that uses shinystats.



````
---
title: "shinystats demo"
format: 
  html:
    resources: 
      - shinylive-sw.js
filters:
  - shinylive
---


App should appear below. Might take a while to load.

```{shinylive-r}
#| label: ar-app
#| standalone: true
#| viewerHeight: 600

shinystats::ar_app()
```
````

## Available Apps so far

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
