#' Create aba adjust object for p-value adjustment
#'
#' @param method
#' @param by
#' @param form
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
aba_adjust <- function(method = c("none", "bonferroni", "fdr", "hochberg",
                                  "holm", "hommel", "BH", "BY"),
                       by = c('group', 'outcome', 'stat'),
                       form = c('metric', 'coef')) {

  method <- match.arg(method)

  s <- list(
    method = method,
    by = by,
    form = form
  )

  class(s) <- 'abaAdjust'
  s
}

adjust_pvals <- function(adjust, results) {

  method <- adjust$method
  by <- adjust$by
  form <- adjust$form

  r %>%
    filter(term == 'Pval') %>%
    group_by(
      groups, outcomes, stats
    ) %>%
    nest() %>%
    mutate(
      pvals_adj = purrr::map(
        .data$data,
        function(x) {
          stats::p.adjust(
            x$est,
            method = p_adjust_method
          )
        }
      )
    ) %>%
    unnest(cols = c(data, pvals_adj)) %>%
    ungroup() %>%
    select(-est) %>%
    rename(
      est = pvals_adj
    ) %>%
    select(groups:form, est, everything()) %>%
    mutate(
      term = 'Pval_adj'
    )

}
