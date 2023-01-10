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
        compute_grid(
          model = model,
          idx = idx,
          at = at,
          expand = expand,
          reduce_fn = reduce_fn
        )
      }
    )

  res <- model$results %>%
    mutate(
      grid = .env$grid
    ) %>%
    select(-fit)

  return(res)
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
#' data <- aba::adnimerge %>%
#'   filter(
#'     AGE > 20,
#'     YEARS_bl <= 3.5,
#'     DX_bl == 'MCI'
#'   )
#' data_bl <- data %>% filter(VISCODE == 'bl')
#'
#' model <- data %>%
#'   rename(XID = RID) %>%
#'   aba_model() %>%
#'   set_outcomes(ADAS13, CDRSB) %>%
#'   set_predictors(c(AGE, CSF_ABETA_bl)) %>%
#'   set_stats(
#'     stat_lme(id = 'XID', time = 'YEARS_bl', poly=2)
#'   ) %>%
#'   fit()
#'
#' model %>%
#'   aba_plot_trajectory(
#'     time_at=seq(0,3,length.out=20)
#'   )
#'
#' model %>%
#'   aba_plot_trajectory(
#'     time_at=seq(0,3,length.out=20),
#'     include_ranef = T
#'   )
#'
#'
#' # two different stats
#' model <- data %>%
#'   rename(XID = RID) %>%
#'   aba_model() %>%
#'   set_outcomes(ADAS13, CDRSB) %>%
#'   set_predictors(c(AGE, CSF_ABETA_bl)) %>%
#'   set_stats(
#'     stat_lme(id = 'XID', time = 'YEARS_bl'),
#'     stat_lme(id = 'XID', time = 'YEARS_bl', poly=2)
#'   ) %>%
#'   fit()
#'
#' # two different stats
#' model %>%
#'   aba_plot_trajectory(
#'     time_at=seq(0,3,length.out=20),
#'     include_ranef = T
#'   )
#'
#' # two stats and "at" vars
#' model %>%
#'   aba_plot_trajectory(
#'     time_at=seq(0,3,length.out=20),
#'     include_ranef = T,
#'     at = list('AGE'=c(50,80))
#'   )
#'
aba_plot_trajectory <- function(model,
                                time_at = NULL,
                                at = NULL,
                                expand = NULL,
                                reduce_fn = mean,
                                include_ranef = FALSE,
                                time_label = 'Time',
                                outcome_label = 'Outcome',
                                include_basic = TRUE) {

  if (is.null(expand)) expand <- c()
  time_vars <- model$stats %>%
    purrr::map_chr(function(s) s$extra_params$time) %>%
    unname()

  if (is.null(at)) at <- list()
  if (is.null(time_at)) {
    expand <- c(expand, time_vars)
  } else {
    if (!is.list(time_at)) {
      time_at <- time_vars %>%
        purrr::set_names() %>%
        purrr::map(function(x) time_at)
    }
    at <- c(time_at, at)
  }

  # extract trajectories
  df_grid <- model %>%
    aba_grid(
      at = at,
      expand = expand,
      reduce_fn = reduce_fn
    ) %>%
    unnest(grid)

  # gather all vars in `at` or `expand` which need to be handled in figure
  mvars <- names(at) %>%
    purrr::map_chr(
      function(mvar) {
        if ( (!mvar %in% time_vars) & (length(at[[mvar]]) > 1) ) return(mvar)
        else return(NA)
      }
    )
  mvars <- mvars[!is.na(mvars)]
  mvars <- c(mvars, expand[!expand %in% time_vars])

  if (length(mvars) > 0) {
    df_grid <- df_grid %>%
      tidyr::unite(
        'INTERACTION',
        all_of(mvars),
        sep=' | ',
        remove=FALSE,
        na.rm=T
      ) %>%
      mutate(
        INTERACTION = ifelse(INTERACTION!='', INTERACTION, 'N/A')
      )
  }

  if (!include_basic) df_grid <- df_grid %>% filter(predictor != 'Basic')

  # include random slopes if specified
  if (include_ranef) {
    df2 <- model$results %>%
      rowwise() %>%
      mutate(
        ranef = list(broom::augment(fit) %>% mutate(PRED = .fitted))
      ) %>%
      select(-fit)

    for (idx in 1:nrow(df2)) {
      # rename outcome var to OUTCOME
      df2$ranef[[idx]][['OUTCOME']] <- df2$ranef[[idx]][[df2$outcome[[idx]]]]
      df2$ranef[[idx]][[df2$outcome[[idx]]]] <- NULL
      # rename time var to TIME
      tmp_stat <- model$stats[[model$results$stat[[idx]]]]
      time_var <- tmp_stat$extra_params$time
      df2$ranef[[idx]][['TIME']] <- df2$ranef[[idx]][[time_var]]
      df2$ranef[[idx]][[time_var]] <- NULL
      # rename id var to ID
      id_var <- tmp_stat$extra_params$id
      df2$ranef[[idx]][['ID']] <- df2$ranef[[idx]][[id_var]]
      df2$ranef[[idx]][[id_var]] <- NULL
    }
    keep_cols <- colnames(df_grid)
    keep_cols <- keep_cols[!keep_cols %in% c('SE','INTERACTION')]
    df2 <- df2 %>%
      unnest(ranef) %>%
      select(all_of(keep_cols), ID, OUTCOME)
  }

  if (length(mvars) > 0) {
    if (length(unique(df_grid$stat)) > 1) {
      fig <- df_grid %>%
        mutate(
          INTERACTION = interaction(stat,INTERACTION,sep=': ')
        ) %>%
        ggplot(aes(x = TIME, y = PRED,
                   group=INTERACTION, color=INTERACTION))
    } else {
      fig <- df_grid %>%
        ggplot(aes(x = TIME, y = PRED,
                   group=INTERACTION, color = INTERACTION))
    }

  } else {
    if (length(unique(df_grid$stat)) > 1) {
      fig <- df_grid %>%
        ggplot(aes(x = TIME, y = PRED, group=stat, color=stat))
    } else {
      fig <- df_grid %>%
        ggplot(aes(x = TIME, y = PRED))
    }
  }

  # add ranef if necessary
  if (include_ranef) {
    fig <- fig +
      geom_line(data=df2,
                aes(x=TIME, y=PRED, group=ID),
                linewidth=0.5, linetype='solid', alpha=0.5,
                color='darkgray')
  }

  if (length(mvars) > 0) {
    fig <- fig +
      geom_line(
        linewidth=1.5, linetype='solid'
      )
  } else {
    fig <- fig +
      geom_line(
        linewidth=1.5, linetype='solid'
      )
  }


  fig <- fig +
    facet_wrap(outcome ~ predictor, scales='free') +
    theme_bw() +
    xlab(time_label) +
    ylab(outcome_label)

  return(fig)
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

  # Create reference grid
  # at: named list to specify your own values for each var in the grid
  # cov.keep: will use all observed values for given var in the grid
  # NOTE - `at` overrides `cov.keep`
  grid_obj <- suppressMessages(
    emmeans::ref_grid(m,
                      at = at,
                      cov.keep = expand,
                      cov.reduce = reduce_fn)
  )
  df_sum <- data.frame(summary(grid_obj)) %>%
    rename(PRED = prediction) %>%
    select(-df)

  if ('time' %in% names(s$extra_params)) {
    time_var <- s$extra_params$time
    df_sum <- df_sum %>%
      rename(TIME = all_of(time_var))
  }

  return(df_sum)
}





