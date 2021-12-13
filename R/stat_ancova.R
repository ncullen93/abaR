#' Create an ancova stat object.
#'
#' This function creates an ancova stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a traditional ancova analysis using the `lm` function with change in endpoint
#' as outcome, adjustment for baseline covariates and baseline outcome, and also
#' a treatment variable whose effect on the endpoint you care about.
#'
#' @param treatment string. The treatment variable whose effect on the outcome
#'   you care about. If you don't have a treatment, then you can just use
#'   `stat_lm` with a change variable as the outcome.
#' @param baseline_suffix string. The suffix to add to each outcome variable
#'   in order to pick up the associated baseline variable. You must adjust for
#'   the baseline outcome in ancova, and there is no other way to specify a
#'   different predictor for each outcome. So if the outcomes are e.g.
#'   "CDRSB" and "MMSE", then a baseline_suffix of "bl" will mean that each
#'   ancova fit with "CDRSB" as outcome will have "CDRSB_bl" added to the
#'   formula and every fit with "MMSE" as outcome will have "MMSE_bl" added.
#'   This means that these baseline variables must actually exist in the data!
#' @param std.beta logical. Whether to standardize model predictors and
#'   covariates prior to analysis.
#' @param complete.cases  logical. Whether to only include the subset of data
#'   with no missing data for any of the outcomes, predictors, or covariates.
#'   Note that complete cases are considering within each group - outcome
#'   combination but across all predictor sets.
#'
#' @return An abaStat object with `ancova` stat type.
#' @export
#'
#' @examples
#' # filter to 24 month visit; calculate change in cognition to use as outcome;
#' # assume abeta status as "treatment" variable.
#' # The goal is to see if "treatment" has an effect on 2y cognitive decline
#' data <- adnimerge %>%
#'   dplyr::filter(
#'     VISCODE == 'm24',
#'     DX_bl %in% c('MCI', 'AD'),
#'     !is.na(CSF_ABETA_STATUS_bl)
#'   ) %>%
#'   dplyr::mutate(
#'     CDRSB = CDRSB - CDRSB_bl,
#'     ADAS13 = ADAS13 - ADAS13_bl,
#'     TREATMENT = factor(CSF_ABETA_STATUS_bl, levels=c(1,0),
#'                        labels=c('Placebo','Treatment'))
#'   )
#'
#' # fit model. note that baseline outcome will be added based on the suffix.
#' # e.g., fits with "CDRSB" as outcome will also add "CDRSB_bl" to the formula.
#' ancova_model <- data %>% aba_model() %>%
#'   set_outcomes(CDRSB, ADAS13) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats(
#'     stat_ancova(treatment = 'TREATMENT', baseline_suffix = 'bl')
#'   ) %>%
#'   fit()
#'
#' # summarise model. treatment effect will be shown in the treatment coefficient
#' ancova_summary <- ancova_model %>% summary()
#'
stat_ancova <- function(treatment,
                        baseline_suffix = 'bl',
                        std.beta = FALSE,
                        complete.cases = TRUE) {
  fns <- list(
    'formula_fn' = formula_std,
    'fit_fn' = fit_ancova,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    ),
    'extra_params' = list(
      'treatment' = treatment,
      'baseline_suffix' = baseline_suffix
    )
  )
  fns$stat_type <- 'ancova'
  class(fns) <- 'abaStat'
  return(fns)
}

# helper function for ancova
fit_ancova <- function(formula, data, extra_params) {

  # add treatment variable to formula
  treatment <- extra_params$treatment
  formula <- glue('{formula} + {treatment}')

  # add baseline variable to formula
  bl_suffix <- extra_params$baseline_suffix
  outcome <- formula %>% strsplit(' ~ ') %>% unlist() %>% head(1)
  formula <- glue('{formula} + {outcome}_{bl_suffix}')

  model <- stats::lm(stats::formula(formula), data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}
