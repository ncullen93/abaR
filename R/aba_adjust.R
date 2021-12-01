#' Create aba adjust object for p-value adjustment
#'
#' @param method method
#' @param by vector. Possible choices: group, outcome, stat, predictor_set
#' @param form form
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
aba_adjust <- function(model_summary,
                       method = c("bonferroni", "fdr", "hochberg",
                                  "holm", "hommel", "BH", "BY"),
                       by = c('group', 'outcome', 'stat'),
                       form = c('metric', 'coef')) {

  method <- match.arg(method)
  model_summary$results <- adjust_pvals(
    model_summary$results, method, by, form
  )
  model_summary
}

adjust_pvals <- function(results, method_, by_, form_) {

  # adjust metric
  if ('metric' %in% form_) {
    r_adj <- results %>%
      filter(term == 'Pval') %>%
      group_by(
        across(all_of(by_))
      ) %>%
      nest() %>%
      mutate(
        estimate_adj = purrr::map(
          .data$data,
          function(x) stats::p.adjust(x$estimate, method = method_)
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

  if ('coef' %in% form_) {
    r_adj <- results %>%
      filter(form == 'coef') %>%
      group_by(
        across(all_of(by_))
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
              stats::p.adjust(xx[!is_intercept], method = method_)
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
