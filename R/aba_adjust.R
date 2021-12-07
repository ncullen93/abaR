#' Create an aba_adjust object.
#'
#' Adjust the p-values (model and/or coefficients) of an abaSummary object.
#'
#' @param object abaSummary. The aba summary whose p-values will be adjusted.
#' @param method string. The method to adjust with. See `p.adjust`.
#' @param by vector. The groupings to use for adjustment.
#'   Possible choices: group, outcome, stat, predictor_set
#' @param form vector. Whether to adjust both metrics and coefs, or just one.
#'
#' @return an abaSummary object. The abaSummary passed to aba_adjust but with
#' p-values changed according to how the user specified.
#' @export
#'
#' @examples
#'
#' df <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' model <- df %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm') %>%
#'   aba_fit()
#'
#' model_summary <- model %>% aba_summary()
#'
#' # default - correct within group, outcome, and stat (x4 comparisons)
#' model_summary_adj <- model_summary %>% aba_adjust(method='bonferroni')
#'
#' # correct within group but across outcomes (x8 comparisons)
#' model_summary_adj2 <- model_summary %>%
#'   aba_adjust(method='bonferroni', by = c('group'))
#'
#' # correct only model P-values, not coefficient P-values
#' model_summary_adj3 <- model_summary %>% aba_adjust(form = c('metric'))
#'
aba_adjust<- function(object,
                      method = c("bonferroni", "fdr", "hochberg",
                                 "holm", "hommel", "BH", "BY"),
                      by = c('group', 'outcome', 'stat'),
                      form = c('metric', 'coef')) {

  method <- match.arg(method)
  object$results <- adjust_pvals(
    object$results, method, by, form
  )
  object
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
