#' Hypothesis Tests and Statistical Distributions for 'SciViews::R'
#'
#' Statistical distributions and hypothesis tests objects with rich-formatted
#' charts and tables.
#'
#' @section Important functions:
#'
#' - [tabularise()] methods for **htest** objects.
#'
#' - [stddev()] for **distribution** objects.
#'
#' - [dfun()], [cdfun()] density function for **distribution** objects.

#'- [chart()] method for **distribution** objects.
#'
#'- [geom_funfill()] fills a part of a distribution density function.
#'
#' @docType package
#' @name inferit-package

## usethis namespace: start
#' @importFrom flextable add_footer_lines add_header_lines align as_paragraph
#'   autofit flextable compose ncol_keys width
#' @importFrom tabularise colformat_sci para_md
#' @importFrom chart chart theme_sciviews
#' @importFrom ggplot2 aes autoplot ggplot geom_function geom_segment
#'   stat_function xlab xlim ylab
#' @importFrom stats density quantile
#' @importFrom distributional cdf support
## usethis namespace: end
NULL
