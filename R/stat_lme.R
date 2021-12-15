#' Create an lme stat object.
#'
#' This function creates an lme stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a linear mixed effects model analysis using the `lme` function from the
#' `nlme` package. Please note that the default mode is to include an interaction
#' term between the `time` variable and each predictor - i.e., `time*predictor`
#' will be in the model formula - but this does not happen for covariates. Also,
#' this model fits random intercepts and random slopes. The data for this model
#' should be in long format with one row per subject-visit.
#'
#' @param id string. This is the variable in the data which represents the
#'   subject id to be used for random intercepts and random slopes.
#' @param time string. This is the time variable in the data which represents
#'   the time from baseline that the visit occured.
#' @param std.beta logical. Whether to standardize model predictors and
#'   covariates prior to analysis.
#' @param complete.cases  logical. Whether to only include the subset of data
#'   with no missing data for any of the outcomes, predictors, or covariates.
#'   Note that complete cases are considering within each group - outcome
#'   combination but across all predictor sets.
#'
#' @return An abaStat object with `lme` stat type.
#' @export
#'
#' @examples
#'
#' data <- adnimerge %>%
#'   dplyr::filter(VISCODE %in% c('bl','m06','m12','m24'))
#'
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     DX_bl %in% c('MCI', 'AD')
#'   ) %>%
#'   set_outcomes(CDRSB, ADAS13) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl,
#'     PLASMA_PTAU181_bl,
#'     PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats(
#'     stat_lme(id = 'RID', time = 'YEARS_bl')
#'   ) %>%
#'   fit()
#'
#' model_summary <- model %>% aba_summary()
#'
stat_lme <- function(id,
                     time,
                     std.beta = FALSE,
                     complete.cases = TRUE) {
  fns <- list(
    'formula_fn' = formula_lme,
    'fit_fn' = fit_lme,
    'extra_params' = list(
      'id' = id,
      'time' = time
    ),
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    )
  )
  fns$stat_type <- 'lme'
  class(fns) <- 'abaStat'

  return(fns)
}

# helper function for lme
formula_lme <- function(outcome, predictors, covariates, extra_params) {
  time <- extra_params$time
  id <- extra_params$id
  #interaction_vars <- extra_params$interaction_vars
  interaction_vars <- c()
  covariates <- covariates[!(covariates %in% interaction_vars)]

  f <- paste(outcome, "~", time)
  if (length(covariates) + length(predictors) > 0) f <- paste(f, '+')
  if (length(covariates) > 0) {
    f <- paste(f, paste(covariates, collapse = " + "))
    if (length(interaction_vars) > 0) {
      f <- paste(f, '+', paste0(interaction_vars, '*',
                                time, collapse=' + '))
    }
    if (length(predictors) > 0) f <- paste(f, '+')
  }
  if (length(predictors) > 0) f <- paste(f, paste0(predictors, "*",
                                                  time,
                                                  collapse = " + "))
  return(f)
}

# helper function for lme
fit_lme <- function(formula, data, extra_params) {
  time <- extra_params$time
  id <- extra_params$id
  random_formula <- glue::glue('~ {time} | {id}')

  model <-
   tryCatch(
     {
       model <- nlme::lme(stats::formula(formula),
                          random = stats::formula(random_formula),
                          control = nlme::lmeControl(
                            maxIter = 1e10,
                            msMaxIter = 1000,
                            opt = "optim"
                          ),
                          na.action = stats::na.omit,
                          data = data, method = "REML")

       model$call$fixed <- stats::formula(formula)
       model$call$random <- stats::formula(random_formula)
       model
     },
     error = function(cond) {
       warning(
         glue('Problem fitting model:
         {formula}
         Check your variables for collinearity or missingness.
         Skipping for now...')
       )
       NULL
     }
   )

  return(model)
}

# helper function for lme
aba_tidy.lme <- function(model, predictors, covariates, ...) {

  # include time in predictors
  time_var <- as.character(model$call$random)[2] %>%
    strsplit(' | ', fixed=TRUE) %>% unlist() %>% head(1)

  tidy_df <- broom.mixed::tidy(model, effects='fixed', conf.int=TRUE)

  tidy_df <- tidy_df %>%
    select(-c(.data$df)) %>%
    filter(
      !(.data$term %in% predictors)#,
      #.data$term != time_var
    ) %>%
    mutate(
      term = strsplit(.data$term, ':') %>%
        purrr::map_chr(~.[length(.)])
    )

  return(tidy_df)
}


# helper function for lme
aba_glance.lme <- function(x, ...) {

  glance_df <- broom.mixed::glance(x) %>% #select(-logLik)
    dplyr::bind_cols(
      tibble::tibble(
        R2 = suppressWarnings(MuMIn::r.squaredGLMM(x)[1,][['R2m']])
      )
    )

  glance_df <- glance_df %>%
    bind_cols(
      tibble::tibble(
        nobs = x$dims$N,
        nsub = unname(x$dims$ngrps[1])
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

