#' Create a rich-formatted table from an htest object
#'
#' @description
#' Default type to [tabularise()] an **htest** object as a flextable.
#'
#' @param data An **htest** object
#' @param header If `TRUE` (by default), add a title to the table
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE` (by default), add the significance stars
#'   to the table.
#' @param ... Additional arguments (unused for now).
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#' @return A **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#'
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default htest
#'
#' @examples
#' data(iris)
#' cor <- cor.test(iris$Sepal.Length, iris$Sepal.Width)
#' tabularise::tabularise(cor)
#'
#' tabularise::tabularise(t.test(x = 1:10, y = 7:20), lang = "fr")
tabularise_default.htest <- function(data, header = TRUE,
lang = getOption("data.io_lang", "en"),
show.signif.stars = getOption("show.signif.stars", TRUE),
..., env = parent.frame()) {

  lang <- tolower(lang)
  if (lang != "fr") {
    lang <- "en" # Only en or fr for now
    info_lang <- infos_en_htest
  } else {
    info_lang <- infos_fr_htest
  }

  x <- data
  res <- x[c("estimate", "statistic", "parameter", "null.value", "p.value")]

  if (length(res$estimate) > 1) {
    if (x$method %in% c("Welch Two Sample t-test", " Two Sample t-test")) {
      res$estimate <- NULL
      res <- c(estimate = x$estimate[[1]] - x$estimate[[2]], res)
      names(res$estimate) <- "difference of means"
    } else {
      names(res$estimate) <- paste0("estimate", seq_along(res$estimate))
      res <- c(res$estimate, res)
      res$estimate <- NULL
    }
  }

  res <- Filter(Negate(is.null), res)
  dat <- as.data.frame(res)

  # Use flextable with colformat_sci()
  if (isTRUE(show.signif.stars)) {
    ft <- flextable(dat, col_keys = c(names(dat), "signif"))
  } else {
    ft <- flextable(dat)
  }
  ft <- colformat_sci(ft)

  # labels (named vector)

  # estimate
  if (!is.null(x$estimate)) {
    estimate <- info_lang[["labs"]][["estimate"]]
    if (!is.null(names(res$estimate))) {
      esti <- names(res$estimate)
      esti[esti %in% names(info_lang[["estimate"]])] <-
        info_lang[["estimate"]][esti]
      #estimate <- paste0(estimate, "\n\n", esti)
      estimate <- esti
    }
  } else {
    estimate <- NULL
  }

  # statistic
  if (!is.null(x$statistic)) {
    statistic <- names(x$statistic)
    statistic[statistic %in% names(info_lang[["statistic"]])] <-
      info_lang[["statistic"]][statistic]
  } else {
    statistic <- "Statistic"
  }

  # parameter
  if (!is.null(x$parameter)) {
    parameter <- names(x$parameter)
    parameter[parameter %in% names(info_lang[["parameter"]])] <-
      info_lang[["parameter"]][parameter]
  } else {
    parameter <- "Parameter"
  }

  labels_auto <- c(
    estimate = estimate,
    statistic = statistic,
    parameter = parameter,
    null.value = info_lang[["labs"]][["null.value"]],
    p.value = info_lang[["labs"]][["p.value"]],
    signif = "")

  labels_red <- labels_auto[names(labels_auto) %in% ft$header$col_keys]
  for (i in seq_along(labels_red))
    ft <- compose(ft, i = 1, j = names(labels_red)[i],
      value = para_md(labels_red[i]), part = "header")

  # Add information on the p.value
  if (ncol_keys(ft) > ncol(dat)) {
    ft <- compose(ft, j = "signif", value =  as_paragraph(
      pvalue_format(.data$p.value)))
    ft <- add_footer_lines(ft,
      values = c("0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"))
    ft <- align(ft, i = 1, align = "right", part = "footer")
  }

  # Add information on confidence levels
  if (!is.null(x$conf.int)) {
    ft <- compose(ft, j = "estimate", value = as_paragraph(
      estimate, " (",x$conf.int[[1]], "-", x$conf.int[[2]], ")"))

    ft <- compose(ft, j = "estimate",part = "header", value = para_md(paste0(
      labels_auto["estimate"], " (",
      info_lang["conf.level"], ":",
      100 * attr(x$conf.int, "conf.level"), "%)"))
    )
  }

  ft <- align(ft, i = 1, align = "center", part = "header")
  # header with method and alternative

  # Add header
  if (isTRUE(header)) {
    method <- info_lang[["method"]]
    if (x$method %in% names(method)) {
      heads <- method[[x$method]]
      if (length(heads) > 1) {
        headers <- heads[[x$alternative]]
      } else {
        headers <- heads
      }
    } else {
      headers <- x$method
    }
    ft <- add_header_lines(ft, values = para_md(headers), top = TRUE)
    ft <- align(ft, i = 1, align = "right", part = "header")
  }

  # Adjust cell with autofit()
  ft <- autofit(ft)

  if (isTRUE(show.signif.stars))
    ft <- width(ft, j = "signif", width = 0.4)

  ft
}

infos_en_htest <- list(
  labs = c(
    estimate = "Estimate",
    null.value = "Value under *H*~0~",
    value = "value",
    p.value = "*p* value"),
  method = list(
    " Two Sample t-test" = c(
      #two.sided = "Two sided two-sample t-test",
      two.sided = "Two sided independent samples t-test",
      less = "Left sided independent samples t-test",
      greater = "Right sided independent samples t-test"
    ),
    "Welch Two Sample t-test" = c(
      two.sided = "Two sided Welch independent samples t-test",
      less = "Left sided Welch independent samples t-test",
      greater = "Right sided Welch independent samples t-test"
    ),
    "Paired t-test" = c(
      two.sided = "Two sided paired t-test",
      less = "Left sided paired t-test",
      greater = "Right sided paired t-test"
    ),
    "One Sample t-test" = c(
      two.sided = "Two sided one sample t-test",
      less = "Left sided one sample t-test",
      greater = "Right sided paired t-test"
    ),
    "Wilcoxon rank sum test with continuity correction" = c(
      two.sided = "Two sided Wilcoxon rank sum test with continuity correction",
      less = "Left sided Wilcoxon rank sum test with continuity correction",
      greater = "Right sided Wilcoxon rank sum test with continuity correction"
    ),
    "Wilcoxon rank sum test" = c(
      two.sided = "Two sided Wilcoxon rank sum test",
      less = "Left sided Wilcoxon rank sum test",
      greater = "Right sided Wilcoxon rank sum test"),
    "Wilcoxon signed rank test with continuity correction" = c(
      two.sided =
        "Two sided Wilcoxon signed rank test with continuity correction",
      less =
        "Left sided Wilcoxon signed rank test with continuity correction",
      greater =
        "Right sided Wilcoxon signed rank test with continuity correction"
    ),
    "Pearson's product-moment correlation" = c(
      two.sided = "Two sided Pearson's product-moment correlation test",
      less = "Left sided Pearson's product-moment correlation test",
      greater = "Right sided Pearson's product-moment correlation test"
    ),
    "Kendall's rank correlation tau" = c(
      two.sided = "Two sided Kendall's rank correlation test",
      less = "Left sided Kendall's rank correlation test",
      greater = "Right sided Kendall's rank correlation test"
    ),
    "Spearman's rank correlation rho" = c(
      two.sided = "Two sided Spearman's rank correlation test",
      less = "Left sided Spearman's rank correlation test",
      greater = "Right sided Spearman's rank correlation test"
    ),
    "Kruskal-Wallis rank sum test" = "Kruskal-Wallis rank sum test",
    "Pearson's Chi-squared test" = "Pearson's $\\chi^{2}$ test",
    "Chi-squared test for given probabilities" =
      "$\\chi^{2}$ test for given probabilities",
    "Pearson's Chi-squared test with Yates' continuity correction" =
      "Pearson's $\\chi^{2}$ test with Yates' continuity correction"
  ),
  statistic = c(
    "t" = "*t*~obs.~ value",
    "W" = "*W*~obs.~ value",
    "V" = "*V*~obs.~ value",
    "z" = "*Z*~obs.~ value",
    "S" = "*S*~obs.~ value",
    "Kruskal-Wallis chi-squared" = "Kruskal-Wallis $\\chi^{2}_{obs.}$",
    "X-squared" = "$\\chi^{2}_{obs.}$",
    "Bartlett's K-squared" = "Bartlett's $\\chi^{2}_{obs.}$"
  ),
  parameter = c(
    df = "Df"
  ),
  estimate = c(
    "difference of means" = "Difference of means",
    "mean difference" = "Mean of the differences",
    "cor" = "Pearson's *r* coefficient",
    "mean of x" = "Mean of variable",
    "tau" = "Kendall's $\\tau$ coefficient",
    "rho" = "Spearman's $\\rho$ coefficient"
  ),
  conf.level = "CI"
)

infos_fr_htest <- list(
  labs = c(
    estimate = "Valeur estim\u00e9e",
    null.value = "Valeur sous *H*~0~",
    value = "Valeur",
    p.value = "Valeur de *p*"),
  method = list(
    " Two Sample t-test" = c(
      two.sided =
        "Test t bilat\u00e9ral d'ind\u00e9pendance  avec variances \u00e9gales",
      less =
        "Test t unilat\u00e9ral \u00e0 gauche d'ind\u00e9pendance avec variances \u00e9gales",
      greater =
        "Test t unilat\u00e9ral \u00e0 droite d'ind\u00e9pendance avec variances \u00e9gales"),
    "Welch Two Sample t-test" = c(
      two.sided = "Test bilat\u00e9ral d'ind\u00e9pendance de Welch",
      less = "Test unilat\u00e9ral \u00e0 gauche d'ind\u00e9pendance de Welch",
      greater =
        "Test unilat\u00e9ral \u00e0 droite d'ind\u00e9pendance de Welch"
    ),
    "Paired t-test" = c(
      two.sided = "Test t bilat\u00e9ral appari\u00e9",
      less = "Test t unilat\u00e9ral \u00e0 gauche appari\u00e9",
      greater = "Test t unilat\u00e9ral \u00e0 droite appari\u00e9"
    ),
    "One Sample t-test" = c(
      two.sided = "Test t bilat\u00e9ral univari\u00e9",
      less = "Test t unilat\u00e9ral \u00e0 gauche univari\u00e9",
      greater = "Test t unilat\u00e9ral \u00e0 droite univari\u00e9"
    ),
    "Wilcoxon rank sum test with continuity correction" = c(
      two.sided = "Test de Wilcoxon bilat\u00e9ral",
      less = "Test de Wilcoxon unilat\u00e9ral \u00e0 gauche",
      greater = "Test de Wilcoxon unilat\u00e9ral \u00e0 droite"
    ),
    "Wilcoxon rank sum test" = c(
      two.sided = "Test de Wilcoxon bilat\u00e9ral",
      less = "Test de Wilcoxon unilat\u00e9ral \u00e0 gauche",
      greater = "Test de Wilcoxon unilat\u00e9ral \u00e0 droite"
    ),
    "Wilcoxon signed rank test with continuity correction" = c(
      two.sided = "Test de Wilcoxon bilat\u00e9ral univari\u00e9",
      less = "Test de Wilcoxon unilat\u00e9ral \u00e0 gauche univari\u00e9",
      greater = "Test de Wilcoxon unilat\u00e9ral \u00e0 droite univari\u00e9"
    ),
    "Pearson's product-moment correlation" = c(
      two.sided = "Test de corr\u00e9lation de Pearson bilat\u00e9ral",
      less =
        "Test de corr\u00e9lation de Pearson unilat\u00e9ral \u00e0 gauche",
      greater =
        "Test de corr\u00e9lation de Pearson unilat\u00e9ral \u00e0 droite"
    ),
    "Kendall's rank correlation tau" = c(
      two.sided = "Test de corr\u00e9lation de Kendall bilat\u00e9ral",
      less =
        "Test de corr\u00e9lation de Kendall unilat\u00e9ral \u00e0 gauche",
      greater =
        "Test de corr\u00e9lation de Kendall unilat\u00e9ral \u00e0 droite"
    ),
    "Spearman's rank correlation rho" = c(
      two.sided = "Test de corr\u00e9lation de Spearman bilat\u00e9ral",
      less =
        "Test de corr\u00e9lation de Spearman unilat\u00e9ral \u00e0 gauche",
      greater =
        "Test de corr\u00e9lation de Spearman unilat\u00e9ral \u00e0 droite"
    ),
    "Kruskal-Wallis rank sum test" = "Test de Kruskal-Wallis",
    "Pearson's Chi-squared test" = "Test de $\\chi^{2}$ d'ind\u00e9pendance",
    "Chi-squared test for given probabilities" =
      "Test de $\\chi^{2}$ pour des probabilit\u00e9s donn\u00e9es",
    "Pearson's Chi-squared test with Yates' continuity correction" =
      "Test de $\\chi^{2}$ d'ind\u00e9pendance avec correction de Yates",
    "Bartlett test of homogeneity of variances" =
      "Test d'homog\u00e9n\u00e9it\u00e9 des variances de Bartlett"
  ),
  estimate = c(
    "difference of means" = "Diff\u00e9rence des moyennes",
    "mean difference" = "Moyenne des diff\u00e9rences",
    "cor" = "Coefficent de Pearson *r* ",
    "mean of x" = "Moyenne de la variable",
    "difference in location" = "Diff\u00e9rence des positions",
    "(pseudo)median" = "(Pseudo) m\u00ediane",
    "tau" = "Coefficient de Kendall $\\tau$",
    "rho" = "Coefficent de Spearman $\\rho$"
  ),
  statistic = c(
    "t" = "Valeur de *t*~obs.~",
    "W" = "Valeur de *W*~obs.~",
    "V" = "Valeur de *V*~obs.~",
    "z" = "Valeur de *Z*~obs.~",
    "S" = "Valeur de *S*~obs.~",
    "Kruskal-Wallis chi-squared" = "$\\chi^{2}_{obs.}$ de Kruskal-Wallis",
    "X-squared" = "$\\chi^{2}_{obs.}$",
    "Bartlett's K-squared" = "$\\chi^{2}_{obs.}$ de Bartlett"
  ),
  parameter = c(
    df = "Ddl"
  ),
  conf.level = "IC"
)

# Internal function of flextable
pvalue_format <- function(x) {
  #x <- get(as.character(substitute(x)), inherits = TRUE)
  z <- cut(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    labels = c("***", " **", "  *", "  .", "   "))
  z <- as.character(z)
  z[is.na(x)] <- ""
  z
}
