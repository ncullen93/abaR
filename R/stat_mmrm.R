#' Create an mmrm stat object.
#'
#' This function creates an mmrm stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a MMRM analysis using the `gls` function from the
#' `nlme` package. Please note that the default mode is to include an interaction
#' term between the `time` variable and each predictor - i.e., `time*predictor`
#' will be in the model formula - but this does not happen for covariates.
#' The data for this model should be in long format with one row per
#' subject-visit.
#'
#' @param id string. This is the variable in the data which represents the
#'   subject id to be used for random intercepts and random slopes.
#' @param time string. This is the time variable in the data which represents
#'   the time from baseline that the visit occured. This should be a categorical
#'   variable or a continuous variable where the values are shared by
#'   all subjects. The fact that time visits should be common across all subjects
#'   is a major operational difference from `stat_lme`, among other differences.
#' @param treatment string. The treatment variable whose effect on the outcome
#'   you care about. This is useful for `aba_emmeans` and other functions.
#' @param baseline_suffix string. The suffix to add to each outcome variable
#'   in order to pick up the associated baseline variable. You must adjust for
#'   the baseline outcome in mmrm, and there is no other way to specify a
#'   different predictor for each outcome. So if the outcomes are e.g.
#'   "CDRSB" and "MMSE", then a baseline_suffix of "bl" will mean that each
#'   mmrm fit with "CDRSB" as outcome will have "CDRSB_bl" added to the
#'   formula and every fit with "MMSE" as outcome will have "MMSE_bl" added.
#'   This means that these baseline variables must actually exist in the data.
#'   Also, there will always be an interaction between the baseline outcome
#'   variable and the time variable.
#' @param std.beta logical. Whether to standardize model predictors and
#'   covariates prior to analysis.
#' @param complete.cases  logical. Whether to only include the subset of data
#'   with no missing data for any of the outcomes, predictors, or covariates.
#'   Note that complete cases are considering within each group - outcome
#'   combination but across all predictor sets.
#'
#' @return An abaStat object with `mmrm` stat type.
#' @export
#'
#' @examples
#'
#' data <- adnimerge %>%
#'   dplyr::filter(VISCODE %in% c('bl','m06','m12','m24'))
#'
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     DX_bl %in% c('MCI', 'AD')
#'   ) %>%
#'   set_outcomes(CDRSB, ADAS13) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl,
#'     PLASMA_PTAU181_bl,
#'     PLASMA_NFL_bl
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats(
#'     stat_mmrm(id = 'RID', time = 'VISCODE')
#'   ) %>%
#'   fit()
#'
#' model_summary <- model %>% aba_summary()
#'
stat_mmrm <- function(id,
                      time,
                      treatment = NULL,
                      baseline_suffix = 'bl',
                      std.beta = FALSE,
                      complete.cases = TRUE) {
  fns <- list(
    'formula_fn' = formula_lme,
    'fit_fn' = fit_mmrm,
    'treatment' = treatment,
    'extra_params' = list(
      'id' = id,
      'time' = time,
      'treatment' = treatment,
      'baseline_suffix' = baseline_suffix
    ),
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    )
  )
  fns$stat_type <- 'mmrm'
  class(fns) <- 'abaStat'

  return(fns)
}

# helper function for stat_mmrm
fit_mmrm <- function(formula, data, extra_params) {
  time <- extra_params$time
  id <- extra_params$id

  # add treatment variable to formula
  treatment <- extra_params$treatment
  if (!is.null(treatment)) {
    formula <- glue('{formula} + {treatment}*{time}')
  }

  # add baseline variable to formula
  bl_suffix <- extra_params$baseline_suffix
  if (!is.null(bl_suffix)) {
    outcome <- formula %>% strsplit(' ~ ') %>% unlist() %>% head(1)
    formula <- glue('{formula} + {outcome}_{bl_suffix}*{time}')
  }

  # make correlation and weights form
  correlation_form <- glue::glue('~ 1 | {id}')
  weights_form <- glue::glue('~ 1 | {time}')

  # make sure data is in the right format:
  # - time variable should be a factor
  # - first visit should be removed
  unique_visits <- levels(factor(data[[time]]))
  if (length(unique_visits) > 10) {
    stop('10+ unique time points detected... MMRM requires discrete time points.
         Did you accidently use a continuous time variable?')
  }
  first_visit <- unique_visits[1]

  # remove the first visit from the data
  data_original <- data %>% distinct()

  data <- data_original %>%
    filter(.data[[time]] != first_visit) %>%
    mutate({{ time }} := factor(.data[[time]]))

  # fit the model
  model <- nlme::gls(
    stats::formula(formula),
    correlation = nlme::corSymm(form = stats::formula(correlation_form)),
    weights = nlme::varIdent(form = stats::formula(weights_form)),
    data = data,
    na.action = na.omit,
    method = 'REML'
  )

  model$call$model <- stats::formula(formula)
  model$call$correlation$form <- stats::formula(correlation_form)
  model$call$weights$form <- stats::formula(weights_form)
  model$call$data <- data
  model$data_original <- data_original

  return(model)
}

# helper function for stat_mmrm
aba_tidy.gls <- function(model, predictors, covariates, ...) {

  time_var <- strsplit(as.character(model$call$weights)[2], ' | ')[[1]][3]
  tidy_df <- broom.mixed::tidy(model, conf.int=TRUE) %>%
    filter(
      !(.data$term %in% predictors)
    ) %>%
    filter(
      !startsWith(.data$term, time_var) | grepl('\\:', .data$term)
    )
  return(tidy_df)
}

# helper function for stat_mmrm
aba_glance.gls <- function(x, ...) {
  glance_df <- broom.mixed::glance(x) %>% select(-logLik)

  # add sample size info
  glance_df <- glance_df %>%
    bind_cols(
      tibble::tibble(
        nobs = x$dims$N,
        nsub = length(levels(x$groups))
      )
    )

  # pivot longer to be like coefficients
  glance_df <- glance_df %>%
    pivot_longer(cols = everything()) %>%
    rename(term = name, estimate = value)

  # add confidence interval
  glance_df <- glance_df %>%
    mutate(
      conf.low = NA,
      conf.high = NA
    )

  return(glance_df)
}

# helper function for stat_mmrm
run_emmeans.gls <- function(fit, extra_params) {
  time <- extra_params$time
  id <- extra_params$id
  treatment <- extra_params$treatment

  emmeans_formula <- formula(glue('~ {treatment} | {time}'))

  emmeans_result <- purrr::quietly(emmeans::emmeans)(fit, emmeans_formula)$result
  pairs_result <- pairs(emmeans_result)

  emmeans_df <- emmeans_result %>%
    broom::tidy(conf.int = TRUE) %>%
    rename(
      treatment = {{ treatment }},
      time = {{ time }},
      pval = p.value
    )

  pairs_df <- pairs_result %>%
    broom::tidy(conf.int = TRUE) %>%
    rename(
      time = {{ time }},
      treatment = contrast,
      pval = p.value
    )

  return(
    list(
      'emmeans' = emmeans_df,
      'pairs' = pairs_df
    )
  )
}

