#' Get standard deviation for a distribution objects
#'
#' The **distribution** objects represent one or more statistical distributions.
#' The generic functions [stddev()] returns the standard deviation for these
#' distributions.
#'
#' @param x A **distribution** object, as from the \{distributional\} package.
#' @param ... Further arguments (not used yet).
#'
#' @return A numeric vector with one or more standard deviations.
#'
#' @export
#' @importFrom distributional variance
#'
#' @examples
#' library(distributional)
#' n1 <- dist_normal(mu = 1, sigma = 1.5)
#' n1
#' class(n1)
#' family(n1)
#' mean(n1)
#' variance(n1)
#' stddev(n1)
stddev <- function(x, ...)
  UseMethod("stddev")

#' @export
#' @rdname stddev
#' @method stddev default
stddev.default <- function(x, ...) {
  stop("The stddev() method is not supported for objects of type ",
    paste(deparse(class(x)), collapse = ""))
}

#' @export
#' @rdname stddev
#' @method stddev distribution
stddev.distribution <- function(x, ...)
  sqrt(variance(x, ...))

# TODO: also tidy() and glance()
#augment.distribution <- function(x, at = NULL, ...) {
#  if (is.null(at)) {
#    range <- quantile(x, c(0.001, 0.999)) |> unlist() |> range()
#    # If range[1] is very close to 0, put it at zero
#    if (range[1] > 0 && range[1] < 0.001)
#      range[1] <- 0
#    at <- seq(from = range[1], to = range[2], length.out = 100L)
#  }
#  dens <- density(x, at = at) |> as_dtf()
#  l <- length(dens)
#  if (l == 1) {
#    names(dens) <- "density"
#  } else {
#    names(dens) <- paste0("density", c("", 2:l))
#  }
#
#  attr(dens, "dist") <- format(x)
#  dtx(quantile = at, dens)
#}
