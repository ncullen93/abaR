#' Calculate reference grid from aba model
#'
#' This function calculates a reference grid for each fitted result in an
#' aba model based on specified parameters (see `emmeans::ref_grid` for examples).
#' The predictions and standard errors from the fitted result are also included
#' in the grid by default
#'
#' @param model aba model. The fitted aba model on which to compute a
#'   reference grid.
#' @param at named list of vectors. A named list where the name is a variable
#'  in the model and the value is a vector of values which should be included
#'  in the list.
#' @param expand character vector. Variables in the model for which all observed
#'  values of the variable should be included in the grid.
#' @param reduce_fn function. Any variables not in `at` and not in `expand` will
#'  will be reduced based on this function. The default is `mean`, where only
#'  the mean value of these variables will be included in the grid. However, the
#'  reduce function can just as well return multiple values from a custom fn:
#'  e.g. `function(x) { seq(min(x), max(x), length.out=10) }` will include 10
#'  values in the grid spaced around the min and max.
#'
#' @return dataframe
#' @export
#'
#' @examples
#' # get data
#' data <- aba::adnimerge %>%
#'   filter(
#'     AGE > 20,
#'     YEARS_bl <= 3.5,
#'     DX_bl == 'MCI'
#'   )
#' data_bl <- data %>% filter(VISCODE == 'bl')
#'
#' model <- data_bl %>%
#'   aba_model() %>%
#'   set_outcomes(ADAS13_bl, CDRSB_bl) %>%
#'   set_predictors(
#'     c(AGE, EDUCATION),
#'     AGE
#'   ) %>%
#'   set_stats('lm') %>%
#'   fit()
#'
#' model_grid <- model %>% aba_grid(at = list('AGE' = c(70, 80, 90)))
#'
#' # All observed values of 'EDUCATION' will be included in the grid
#' # For all other vars ('AGE'), a 4-val seq spaced from min-max will be included
#' model_grid <- model %>% aba_grid(
#'   expand = c('EDUCATION'),
#'   reduce_fn = function(x) { seq(min(x), max(x), length.out=4) }
#' )
aba_grid <- function(model,
                     at = NULL,
                     expand = NULL,
                     reduce_fn = mean) {

  grid <- 1:nrow(model$results) %>%
    purrr::map(
      function(idx) {
        compute_grid(model, idx, at, reduce_fn, expand)
      }
    )

  res <- model$results %>%
    mutate(
      grid = .env$grid
    ) %>%
    select(-fit)

  return(res)
}

#' Calculate longitudinal trajectories from aba model
#'
#' Note: This only works on longitudinal stats
#'
#' @param model
#' @param time_vals
#' @param predictor_mode
#' @param covariate_mode
#' @param level
#'
#' @return
#' @export
#'
#' @examples
aba_trajectory <- function(model,
                           time_vals = NULL,
                           predictor_mode = 'mean',
                           covariate_mode = 'mean',
                           level = 0) {

}

#' Plot trajectories of fitted longitudinal stats
#'
#' level=0: group-level estimates
#' level=1: individual-level estimates
#' level=2: individual-level observations
#'
#' @param model
#' @param time_vals
#' @param level
#'
#' @return
#' @export
#'
#' @examples
aba_plot_trajectory <- function(model, time_vals=NULL, level = 0) {

  # extract trajectories
  df_traj <- aba_trajectory(model, time_vals)

}

# this function computes a reference grid using `emmeans::ref_grid`
# it is a helper function for `aba_grid`
compute_grid <- function(model,
                         idx,
                         at = NULL,
                         expand = NULL,
                         reduce_fn = mean) {

  m <- model$results$fit[[idx]]
  s <- model$stats[[model$results$stat[[idx]]]]
  outcome <- model$results$outcome[[idx]]

  # xvar defaults to time for longitudinal models; errors otherwise
  if (is.null(xvar)) {
    if ('time' %in% names(s$extra_params)) xvar <- s$extra_params$time
    else stop('Must supply x_var if the abaStat does not have time component.')
  }

  # Create reference grid
  # at: named list to specify your own values for each var in the grid
  # cov.keep: will use all observed values for given var in the grid
  # NOTE - `at` overrides `cov.keep`
  grid_obj <- emmeans::ref_grid(m,
                                at = at,
                                cov.keep = expand,
                                cov.reduce = reduce_fn)
  df_sum <- data.frame(summary(grid_obj)) %>%
    rename({{ outcome }} := prediction) %>%
    select(-df)

  return(df_sum)
}

