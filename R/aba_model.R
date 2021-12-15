#' Create an aba model.
#'
#' An aba model is the foundational object in the aba package. It is composed
#' of the following:
#'   - data: a data.frame to be used to fit the statistical models
#'   - spec: the specification for the aba model composed of the following:
#'     - groups: subsets of the data
#'     - outcomes: dependent variables in statistical fits.
#'     - covariates: independent variables which should always be included in
#'         statistical fits.
#'     - predictors: independent variables which will vary across different
#'         statistical fits.
#'   - results: the resulting fitted statistics.
#'
#' @param data data.frame the data to use for the object
#' @param groups vector or list of logical statements as trings. Groups are
#' subsets of the data on which different models will be fit.
#' @param outcomes vector or list of strings Outcomes are the dependent
#'   variables in the statistical fits.
#' @param covariates vector of strings Covariates are independent variables
#'   which remain fixed across all statistical fits and are therefore always
#'   included with the different combinations of predictors.
#' @param predictors vector or list of strings Predictors are independent
#'   variables which you want to vary. You can include variables on their own
#'   or in combination with others. A collection of variables is referred to as
#'   a `predictor` and unique variables are referred to as a `term`.
#' @param stats string or abaStat object(s) with `stat_` prefix. Stats are
#'   the actual statistical models which you want to fit on the data. Their
#'   primary functions are to 1) generate a suitable model formula given the
#'   outcome - covariate - predictor combination, and 2) to actually fit the
#'   statistical model.
#' @param verbose logical. Whether to give a progress bar during model fitting.
#'   This can be useful if the fitting procedure is going to take a long time.
#'
#' @return An aba model which can be fitted using the `aba_fit()` function and
#'   which can be modified in any manner.
#'
#' @export
#'
#' @examples
#'
#' # use built-in data and only take the baseline visit
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # Create aba model w/ data, groups, outcomes, covariates, predictors, stats.
#' # Note that we start with piping the data into the aba_model... This is
#' # possible because `data` is the first argument of the `aba_model()` function
#' # and is useful because it gives auto-completion of variables names in Rstudio.
#' model <- data %>% aba_model() %>%
#'   set_groups(everyone(), DX_bl %in% c('MCI','AD')) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm')
#'
#' # get a useful view of the model spec:
#' print(model)
#'
#' # model specs can be modified to build on one another and save time when
#' # doing sensitivity analyses. Here, we create the same model as before but
#' # just add APOE4 as covariate.
#' model2 <- model %>%
#'   set_covariates(AGE, GENDER, EDUCATION, APOE4)
#'
#' # see this change in the model print
#' print(model2)
#'
#' # Calling the `fit()` function actually triggers fitting of statistics.
#' model <- model %>% fit()
#' model2 <- model2 %>% fit()
#'
#' # Access the raw results in case you care about that:
#' print(model$results)
#'
#' # Calling the `summary()` function summarises covariates and metrics in
#' # a useful manner
#' model_summary <- model %>% summary()
#' model2_summary <- model2 %>% summary()
#'
#' # see a nicely formatted print out of the summary
#' print(model_summary)
#'
#' # or access the raw summary results:
#' print(model_summary$results)
#'
aba_model <- function(data = NULL,
                      groups = NULL,
                      outcomes = NULL,
                      predictors = NULL,
                      covariates = NULL,
                      stats = NULL,
                      verbose = FALSE) {

  m <- list(
    'data' = data,
    'groups' = groups,
    'outcomes' = outcomes,
    'predictors' = predictors,
    'covariates' = covariates,
    'stats' = stats,
    'results' = list(),
    'verbose' = verbose,
    'is_fit' = FALSE
  )

  class(m) <- 'abaModel'

  if (!is.null(data)) m <- m %>% set_data(data)
  if (!is.null(groups)) m <- m %>% set_groups(groups)
  if (!is.null(outcomes)) m <- m %>% set_outcomes(outcomes)
  if (!is.null(predictors)) m <- m %>% set_predictors(predictors)
  if (!is.null(covariates)) m <- m %>% set_covariates(covariates)
  if (!is.null(stats)) m <- m %>% set_stats(stats)

  return(
    m
  )
}

#' @export
print.abaModel <- function(x, ...) {
  model <- x

  group_vals <- model$groups
  group_labels <- names(model$groups)
  outcome_vals <- model$outcomes
  outcome_labels <- names(model$outcomes)
  covariate_vals <- model$covariates
  predictor_vals <- model$predictors[-1]
  predictor_labels <- names(model$predictors[-1])
  stat_vals <- model$stats

  cat('\n')
  cat('Groups:\n   ')
  if (length(group_vals) > 0) {
    cat(paste0(group_labels, ': ', group_vals, ''), sep='\n   ')
  }

  # OUTCOMES #
  cat('\nOutcomes:\n   ')
  if (length(outcome_vals) > 0) {
    cat(paste0(outcome_labels, ': ', outcome_vals, ''), sep='\n   ')
  }

  # COVARIATES #
  cat('\nCovariates:\n   ')
  if (length(covariate_vals) > 0) {
    cat(covariate_vals)
    cat('\n')
  }

  # PREDICTORS #
  cat('\nPredictors:\n   ')
  if (length(predictor_vals) > 0) {
    cat(paste0(predictor_labels, ': ', predictor_vals), sep='\n   ')
  }

  # STAT #
  cat('\nStats:\n   ')
  if (length(stat_vals) > 0) {
    stat_vals %>% purrr::walk(~cat(print(.),'\n   '))
  }
}
