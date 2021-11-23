#' aba (pre-)screening model
#'
#' @param model aba model. should be fitted, only glm supported now.
#' @param threshold value or vector of values. between 0 and 1. Relative cutoff
#'   from glm model to use as theoretical screening inclusion cutoff
#' @param ntrials integer. number of bootstrap trials.
#' @param verbose logical. whether to print updates.
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
aba_screen <- function(model,
                       threshold,
                       cost_multiplier,
                       include_n,
                       ntrials = 100,
                       verbose = TRUE) {

  m <- list(
    'model' = model,
    'threshold' = threshold,
    'cost_multiplier' = cost_multiplier,
    'include_n' = include_n,
    'params' = list(
      'ntrials' = ntrials
    ),
    'verbose' = verbose
  )
  class(m) <- 'abaScreen'
  return(m)
}


#' Fit an aba screen object
#'
#' @param object aba screen object to fit
#' @param ... additional parameters
#'
#' @return abaScreen object
#' @export
#'
#' @examples
#' x <- 1
fit.abaScreen <- function(object, ...) {
  model <- object$model
  model_results <- model$results
  ntrials <- object$params$ntrials

  # expand by threshold / cost_multiplier / include_n
  param_list <- list(
    'MID' = unique(model_results$MID),
    'threshold' = object$threshold,
    'cost_multiplier' = object$cost_multiplier,
    'include_n' = object$include_n
  )

  model_results <- model_results %>%
    right_join(
      param_list %>% purrr::cross_df(),
      by = 'MID'
    )

  screen_results <- 1:ntrials %>%
    purrr::map(
      function(idx) {
        model_results %>%
          rowwise() %>%
          mutate(
            screen_results = list(
              run_screen_model(
                fit = .data$stats_fit,
                outcome = .data$outcomes,
                threshold = .data$threshold,
                cost_multiplier = .data$cost_multiplier,
                include_n = .data$include_n,
                idx = idx
              )
            )
          ) %>%
          unnest(screen_results) %>%
          select(
            -c(stats_obj, stats_fit)
          ) %>%
          mutate(trial = idx)
      }
    ) %>%
    bind_rows() %>%
    arrange(MID, groups, outcomes)

  all_metrics <- screen_results %>%
    select(-c(MID:include_n,
              trial)) %>%
    colnames()

  screen_results_summary <- screen_results %>%
    filter(trial == 1) %>%
    left_join(
      screen_results %>% filter(trial != 1) %>%
        group_by(
          MID, groups, outcomes,
          predictors, covariates, stats,
          threshold, cost_multiplier, include_n
        ) %>%
        summarise(
          across(all_of(all_metrics),
                 list(
                   'conf_lo' = ~quantile(., 0.025),
                   'conf_hi' = ~quantile(., 0.975)
                 )),
          .groups = 'keep'
        ) %>%
        ungroup(),
      by = c("MID", "groups", "outcomes",
             "stats", "predictors", "covariates",
             "threshold", "cost_multiplier", "include_n")
    ) %>%
    arrange(groups, outcomes, MID)

  object$results <- screen_results
  object$results_summary <- screen_results_summary %>%
    select(
      MID:include_n,
      all_of(
        apply(expand.grid(c('','_conf_lo','_conf_hi'), all_metrics),
              1, function(x) paste0(x[2],x[1]))
      )
    )

  return(object)
}


run_screen_model <- function(fit,
                             outcome,
                             threshold,
                             cost_multiplier,
                             include_n,
                             idx) {

  data_fit <- stats::model.frame(fit) %>% tibble::tibble()
  data_fit <- data_fit %>%
    mutate(
      .Predicted = stats::predict(fit, type='response'),
      .Truth = .data[[outcome]]
    )

  cut_val <- unname(quantile(data_fit$.Predicted, 1 - threshold))

  data_fit <- data_fit %>%
    mutate(
      .Included = as.integer(.Predicted > cut_val)
    )

  # run bootstrap sample
  if (idx != 1) {
    data_fit <- data_fit[sample(nrow(data_fit), nrow(data_fit), replace=T),]
  }

  base_rate <- mean(data_fit$.Predicted)
  base_test_n <- ceiling(include_n / base_rate)
  base_cost <- base_test_n * cost_multiplier

  data_included <- data_fit %>% filter(.Included == 1)
  model_rate <- mean(data_included$.Truth)
  model_test_n <- ceiling(include_n / model_rate)
  model_screen_n <- ceiling(model_test_n / threshold)
  model_cost <- model_test_n * cost_multiplier + model_screen_n * 1
  model_cost_save <- 100 * (base_cost - model_cost) / base_cost

  results_df <- tibble::tibble(
    base_rate,
    base_test_n,
    base_cost,
    model_rate,
    model_test_n,
    model_screen_n,
    model_cost,
    model_cost_save
  )
  return(results_df)
}
