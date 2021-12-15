#' Create a retest stat object.
#'
#' This function creates a retest stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a test-retest analysis on data in long format. It can be used to calculate
#' the bias and variance of biomarkers (or any variables, for that matter)
#' when measured multiple times. Moreover, the result of a model fit with this
#' stat can be subsequently passed to the `aba_robust()` object in order to
#' test the effect of test-retest bias/variance on clinical prediction models
#' which can also be fit as an aba model.
#'
#' @param id string. This is the subject id variable in the dataset. This is
#'   necessary to keep track of which values belong to which individuals.
#' @param time string. This is the time variable in the dataset. This is
#'   necessary to keep track of which values belong to which time point.
#' @param method string. This is the method used to calculate the difference
#'   between outcome values across time points. Options are:
#'   `percent_change` calculated by 100 * (x - y) / y where x is the earlier
#'   time and y is the later time.
#' @param std.beta logical. Whether to standardize the model outcomes and
#'   predictors/outcomes prior to analysis.
#' @param complete.cases  logical. Whether to only include the subset of data
#'   with no missing data for any of the outcomes, predictors, or covariates.
#'   Note that complete cases are considering within each group - outcome
#'   combination but across all predictor sets.
#'
#' @return An abaStat object with `retest` stat type.
#' @export
#'
#' @examples
#' # use longitudinal data in healthy controls as pseudo "test-retest"
#' data <- adnimerge %>%
#'   dplyr::filter(
#'     VISCODE %in% c('bl' ,'m06', 'm12'),
#'     DX_bl == 'CU'
#'   )
#'
#' # fit model over two groups and two endpoints
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     CSF_ABETA_STATUS_bl == 1,
#'     labels = c('CU', 'CU AB-')
#'   ) %>%
#'   set_outcomes(
#'     ADAS13, MMSE,
#'     labels = c('ADAS13', 'MMSE')
#'   ) %>%
#'   set_stats(
#'     stat_retest(id = 'RID', time = 'VISCODE')
#'   ) %>%
#'   aba_fit()
#'
#' # summarise model to get bias and variance estimates
#' model_summary <- model %>% aba_summary()
#'
#' # plot model results like any other summary
#' g <- model_summary %>% aba_plot_coef(
#'   x='term', group='group', facet=c('outcome','predictor'), coord_flip=TRUE
#' )
stat_retest <- function(id,
                        time,
                        method = c('percent_change'),
                        std.beta = FALSE,
                        complete.cases = FALSE) {
  method <- match.arg(method)

  fns <- list(
    'formula_fn' = formula_retest,
    'fit_fn' = fit_retest,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    ),
    'extra_params' = list(
      'id' = id,
      'time' = time,
      'method' = method
    )
  )
  fns$stat_type <- 'retest'
  class(fns) <- 'abaStat'
  return(fns)
}

# helper function for stat_retest
formula_retest <- function(outcome, predictors, covariates, extra_params) {
  time <- extra_params$time
  id <- extra_params$id

  if (length(predictors) > 0) stop('Retest should not have predictors.')
  if (length(covariates) > 0) stop('Retest does not currently support covariates.')
  f <- as.character(glue('{outcome} ~ {time} | {id}'))

  return(f)
}

# helper function for stat_retest
fit_retest <- function(formula, data, extra_params) {
  vars <- formula %>% strsplit(' ', fixed=T) %>% unlist()
  outcome <- vars[1]
  id <- vars[5]
  time <- vars[3]

  # calculate
  df_res <- data %>%
    select(all_of(c(outcome, id, time))) %>%
    pivot_wider(
      names_from = {{ time }},
      values_from = {{ outcome }},
      values_fn = mean
    )
  df_res_orig <- df_res

  all_visits <- df_res %>% select(-all_of(c(id))) %>% names()
  all_visit_combos <- all_combos(all_visits)
  is_pairs <- all_visit_combos %>% purrr::map_lgl(~length(.) == 2)
  all_visit_pairs <- all_visit_combos[is_pairs] %>% unique()

  for (pair in all_visit_pairs) {
    xvar <- pair[1]
    yvar <- pair[2]

    df_res <- df_res %>%
      mutate(
        '{xvar} - {yvar}' := percent_change(.data[[xvar]], .data[[yvar]])
      )
  }

  df_res_subject <- df_res %>%
    select(-all_of(c(all_visits))) %>%
    pivot_longer(-all_of(c(id)),
                 names_to = 'time',
                 values_to = 'estimate') %>%
    select(all_of(c(id)), time, estimate)

  struct <- list(
    results = df_res_subject,
    data = df_res_orig,
    outcome = outcome,
    id = id,
    time = time
  )

  class(struct) <- 'rtm'
  struct
}

# helper function for stat_retest
aba_tidy.rtm <- function(model, predictors, covariates, ...) {
  res <- model$results
  id <- model$id
  time <- model$time
  outcome <- model$outcome

  # bias value
  res_mean <- res %>%
    group_by(time) %>%
    summarise(
      val = mean(estimate, na.rm=T),
      conf.low = mean(estimate, na.rm=T) - 1.96 * sd(estimate, na.rm=T),
      conf.high = mean(estimate, na.rm=T) + 1.96 * sd(estimate, na.rm=T),
      p.value = stats::t.test(estimate)$p.value
    ) %>%
    rename(estimate = val,
           term = time) %>%
    mutate(term = paste0('Bias [', term,']'))

  # variance value
  res_sd <- res %>%
    group_by(time) %>%
    summarise(
      val = sd(estimate, na.rm=T),
      conf.low = sqrt(dplyr::n() * sd(estimate, na.rm=T) * sd(estimate, na.rm=T) /
                        stats::qchisq(0.025, dplyr::n(),lower.tail=FALSE)),
      conf.high = sqrt(dplyr::n() * sd(estimate, na.rm=T) * sd(estimate, na.rm=T) /
                        stats::qchisq(0.025, dplyr::n())),
      p.value = NA,
    ) %>%
    rename(estimate = val,
           term = time) %>%
    mutate(term = paste0('Variance [', term,']'))

  res_summary <- res_mean %>% bind_rows(res_sd)

  res_summary
}

# helper function for stat_retest
aba_glance.rtm <- function(x, x0, ...) {
  res <- tibble::tibble(
    nobs = nrow(x$data)
  )
  glance_df <- res %>%
    pivot_longer(cols = everything()) %>%
    rename(term = name, estimate = value)

  # add conf.low and conf.high columns
  glance_df <- glance_df %>% mutate(conf.low = NA, conf.high = NA)
  glance_df
}

# helper function for stat_retest
percent_change <- function(x, y) {
  xx <- 100 * (x - y) / x
  xx[xx %in% c(NaN, Inf, -Inf)] <- NA
  xx
}
