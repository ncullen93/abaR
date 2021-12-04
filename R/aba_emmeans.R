
#' Calculate estimated marginal / least-square means and treatment effects
#' for mmrm, lme, etc
#'
#' @param model abaModel
#' @param pairs logical
#' @param ... additional parameters
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
aba_emmeans <- function(model,
                        ...) {

  r <- model$results %>%
    filter(predictor != '') %>%
    rowwise() %>%
    mutate(
      stat_emmeans = list(
        run_emmeans(
          fit = .data$stat_fit,
          extra_params = .data$stat_obj$extra_params
        )
      )
    ) %>%
    unnest_wider(stat_emmeans)

  emmeans_df <- r %>%
    select(group:predictor_set, emmeans) %>%
    unnest(emmeans) %>%
    select(-c(df, statistic))

  pairs_df <- r %>%
    select(group:predictor_set, pairs) %>%
    unnest(pairs) %>%
    select(-c(term, null.value, df, statistic))

  s <- list(
    'emmeans' = emmeans_df,
    'pairs' = pairs_df
  )
  class(s) <- 'abaEmmeans'
  return(s)
}
