
#' Create an aba screen object.
#'
#' This function runs a clinical trial screening analysis based on a fitted aba
#' model with glm stats. You can supply different inclusion thresholds which
#' represent predicted probabilities from the glm stats, and you can also
#' supply cost multipliers and the required sample size in order to perform a
#' cost-benefit analysis. This analysis uses bootstrap sampling to generate
#' confidence intervals.
#'
#' @param object an aba model. The fitted aba model which you want to use as
#'   the screening algorithm.
#' @param threshold double or vector of doubles between 0 and 1. The threshold
#'   represents the percentage of individuals who will be invited to take the
#'   inclusion test. Note that the threshold value is calculated in a relative
#'   manner based on the values in the data population, not based on an absolute
#'   risk value.
#' @param cost_multiplier double or vector of doubles. The cost multiplier
#'   represents how much more expensive it is to perform the main inclusion
#'   test versus the screening test. Larger values mean that the main inclusion
#'   test/biomarker is much more expensive than the screening test and will
#'   therefore result in larger cost savings by using the screening model to
#'   identify individuals who are at low risk to be positive on the main
#'   inclusion test.
#' @param include_n integer. The number of participants who you expect
#'   to be included in the clinical trial. This is therefore the number of
#'   individuals who must pass the screening test and who then must pass the
#'   main inclusion test
#' @param ntrials integer. The number of bootstrap trials to run in order to
#'   generate the confidence interval
#' @param verbose logical. Whether to show a progress bar for each trial.
#'
#' @return an abaScreen object
#' @export
#'
#' @examples
#'
#' # use built-in data
#' df <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # first, fit an aba model to predict amyloid PET status from plasma markers
#' # In this scenario, PET is the "inclusion" marker and plasma is the
#' # "screening" marker. PET is expensive and plasma is cheap, so we want to
#' # use plasma markers to decide who should undergo PET scans in order to
#' # minimize the risk of negative (i.e., wasted) PET scans.
#' model <- df %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(PET_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_PTAU181_bl,
#'     PLASMA_NFL_bl,
#'     c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats('glm') %>%
#'   fit()
#'
#' # summarise the model just to show the plasma biomarkers do in fact
#' # provide some predictive value for amyloid PET status
#' model_summary <- model %>% aba_summary()
#'
#' # Run the screening analysis while varying the inclusion threshold from
#' # 25% to 75% (this is the percent of individuals who will be invited for
#' # the PET scan) and varying the cost multiplier from 4 to 16 (this is how
#' # much more PET costs compared to plasma) and assuming we want to recruit
#' # 1000 amyloid PET positive subjects.
#' model_screen <- model %>%
#'   aba_screen(
#'     threshold = seq(0.25, 0.75, by = 0.1),
#'     cost_multiplier = c(4, 8, 16),
#'     include_n = 1000,
#'     ntrials = 5,
#'     verbose = TRUE
#'   )
#'
aba_screen <- function(object,
                       threshold,
                       cost_multiplier,
                       include_n,
                       ntrials = 100,
                       verbose = TRUE) {

  m <- list(
    'model' = object,
    'threshold' = threshold,
    'cost_multiplier' = cost_multiplier,
    'include_n' = include_n,
    'params' = list(
      'ntrials' = ntrials
    ),
    'verbose' = verbose
  )
  class(m) <- 'abaScreen'

  m <- fit_screen(m)

  return(m)
}

# helper function for running the screening analysis
fit_screen <- function(object, ...) {
  model <- object$model
  model_results <- model$results
  ntrials <- object$params$ntrials

  # expand by threshold / cost_multiplier / include_n
  param_list <- list(
    'predictor' = unique(model_results$predictor),
    'threshold' = object$threshold,
    'cost_multiplier' = object$cost_multiplier,
    'include_n' = object$include_n
  )

  model_results <- model_results %>%
    right_join(
      param_list %>% purrr::cross_df(),
      by = 'predictor'
    )

  if (object$verbose) pb <- progress::progress_bar$new(total = ntrials)

  screen_results <- 1:ntrials %>%
    purrr::map(
      function(idx) {
        if (object$verbose) pb$tick()

        model_results %>%
          rowwise() %>%
          mutate(
            screen_results = list(
              run_screen_model(
                fit = .data$fit,
                outcome = model$outcomes[[.data$outcome]],
                threshold = .data$threshold,
                cost_multiplier = .data$cost_multiplier,
                include_n = .data$include_n,
                idx = idx
              )
            )
          ) %>%
          unnest(screen_results) %>%
          select(
            -c(fit)
          ) %>%
          mutate(trial = idx)
      }
    ) %>%
    bind_rows() %>%
    arrange(predictor, group, outcome)

  all_metrics <- screen_results %>%
    select(-c(group:include_n,
              trial)) %>%
    colnames()

  screen_results_summary <- screen_results %>%
    filter(trial == 1) %>%
    left_join(
      screen_results %>% filter(trial != 1) %>%
        group_by(
          group, outcome, stat, predictor,
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
      by = c("group", "outcome", "stat", "predictor",
             "threshold", "cost_multiplier", "include_n")
    ) %>%
    arrange(group, outcome, predictor)

  object$results <- screen_results
  object$results_summary <- screen_results_summary %>%
    select(
      predictor:include_n,
      all_of(
        apply(expand.grid(c('','_conf_lo','_conf_hi'), all_metrics),
              1, function(x) paste0(x[2],x[1]))
      )
    )

  return(object)
}

# helper function for running the screening analysis
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
    data_fit <- data_fit[sample(nrow(data_fit), nrow(data_fit), replace=TRUE),]
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
