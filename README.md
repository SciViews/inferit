# Hypothesis Tests and Statistical Distributions for 'SciViews::R' <a href='https://www.sciviews.org/inferit'><img src='man/figures/logo.png' align='right' height='139'/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/inferit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/inferit/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/SciViews/inferit/branch/main/graph/badge.svg)](https://codecov.io/gh/SciViews/inferit?branch=main) [![CRAN status](https://www.r-pkg.org/badges/version/inferit)](https://cran.r-project.org/package=inferit) [![r-universe status](https://sciviews.r-universe.dev/badges/inferit)](https://sciviews.r-universe.dev/inferit) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

Inference Tools for SciViews::R

## Installation

{inferit} is not available from CRAN yet. You should install it from the [SciViews R-Universe](https://sciviews.r-universe.dev). You would also need the following packages: {chart} as an alternate formula interface to {ggplot2}, {tabularise} to produce publication-ready (rich-formatted) tabular output, {equatags} and {equatiomatic} packages are optional additions to tabularise to display equations and {data.io} is useful too because it manages labels and units that {chart} and {tabularise} use. To install these six packages and their dependencies, run the following command in R:

``` r
install.packages(c('inferit', 'chart', 'tabularise', 'equatags', 'equatiomatic', 'data.io'),
  repos = c('https://sciviews.r-universe.dev', 'https://cloud.r-project.org'))
```

You can also install the latest development version of {inferit}. Make sure you have the {remotes} R package installed:

``` r
# install.packages("remotes")
remotes::install_github("SciViews/inferit")
```

## Usage

You can get further help about this package this way: Make the {inferit} package available in your R session:

``` r
library(inferit)
```

Get help about this package:

``` r
library(help = "inferit")
help("inferit-package")
vignette("inferit") # None is installed with install_github()
```

For further instructions, please, refer to the help pages at <https://www.sciviews.org/inferit/>.

## Code of Conduct

Please note that the {inferit} package is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
