#' Create aba adjust object for p-value adjustment
#'
#' @param method method
#' @param by by
#' @param form form
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

adjust_pvals <- function(results, adjust) {

  # adjust metric
  if ('metric' %in% adjust$form) {
    r_adj <- results %>%
      filter(term == 'Pval') %>%
      group_by(
        across(all_of(adjust$by))
      ) %>%
      nest() %>%
      mutate(
        estimate_adj = purrr::map(
          .data$data,
          function(x) stats::p.adjust(x$estimate, method = adjust$method)
        )
      ) %>%
      unnest(cols = c(data, estimate_adj)) %>%
      ungroup() %>%
      select(-estimate) %>%
      rename(
        estimate = estimate_adj
      ) %>%
      select(group:form, estimate, everything())

    results <- results %>%
      filter(term != 'Pval') %>%
      bind_rows(r_adj)
  }

  if ('coef' %in% adjust$form) {
    r_adj <- results %>%
      filter(form == 'coef') %>%
      group_by(
        across(all_of(adjust$by))
      ) %>%
      nest() %>%
      mutate(
        pval_adj = purrr::map(
          .data$data,
          function(x) {
            # dont include intercept in pval adjustment
            xx <- x$pval
            is_intercept <- x$term == '(Intercept)'
            xx[!is_intercept] <-
              stats::p.adjust(xx[!is_intercept], method = adjust$method)
            xx
          }
        )
      ) %>%
      unnest(cols = c(data, pval_adj)) %>%
      ungroup() %>%
      select(-pval) %>%
      rename(
        pval = pval_adj
      )

    results <- r_adj %>%
      bind_rows(
        results %>% filter(form != 'coef')
      )
  }

  return(results)
}
