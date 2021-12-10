
#' Summarise a fitted aba model.
#'
#' This function concisely summarises coefficients and metrics for the stat fits
#' from the different group - outcome - stat combinations. This is the primary
#' function to use if you want to see the results of a fitted aba model. It is
#' also the way to generate publication-ready tables of model results.
#'
#' @param object abaModel. The fitted aba model which you want to summarise.
#' @param control abaControl. An aba control object which allows users to
#'   customize the summary process -- e.g., whether to include covariates in
#'   the table.
#' @param verbose logical. Whether to provide a progress bar to track status.
#'
#' @return an abaSummary object which contains coefficients and metrics from
#'   the different statistical fits summarised into publication-ready tables.
#' @export
#'
#' @examples
#'
#' # use built-in data
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit an aba model
#' model <- data %>% aba_model() %>%
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
#' # default aba summary
#' model_summary <- model %>% aba_summary()
#'
#' # create an aba control object to customize the summary
#' my_control <- aba_control(include_covariates = F)
#'
#' # summarise model with th custom aba control - notice covariates
#' # wont be included in the tables when you print the summary to console
#' model_summary2 <- model %>% aba_summary(control = my_control)
#'
aba_summary <- function(object,
                        control = aba_control(),
                        verbose = FALSE) {

  # coefficients and model metrics
  coefs_df <- coefs_summary(object, control) %>% mutate(form = 'coef')
  metrics_df <- metrics_summary(object) %>% mutate(form = 'metric')
  results_df <- coefs_df %>% bind_rows(metrics_df) %>%
    select(group, outcome, stat, predictor_set, term, form, estimate:pval) %>%
    filter(!is.na(estimate))

  s <- list(
    model = object,
    results = results_df
  )

  class(s) <- 'abaSummary'
  return(s)
}

#' @export
summary.abaModel <- function(object,
                             control = aba_control(),
                             verbose = FALSE) {
  object %>% aba_summary(control=control, verbose = verbose)
}

# helper function for aba summary
coefs_summary <- function(model, control) {

  all_predictors <- model$spec$predictors %>%
    purrr::map(~strsplit(.,' + ',fixed=T)) %>%
    unlist() %>% unique()

  all_covariates <- model$spec$covariates
  if (is.null(all_covariates)) all_covariates <- c('')

  # coefficients
  r <- model$results %>%
    rowwise() %>%
    mutate(
      stat_coefs = list(aba_tidy(.data$stat_fit, all_predictors, all_covariates))
    ) %>%
    ungroup()

  r <- r %>%
    unnest(
      .data$stat_coefs
    )

  r <- r %>%
    select(
      -c(.data$predictor, .data$covariate,
         .data$std.error, .data$statistic,
         .data$stat_obj, .data$stat_fit)
    )

  r <- r %>%
    rename(
      conf_low = conf.low,
      conf_high = conf.high,
      pval = p.value
    ) %>%
    select(-pval, everything())

  ## remove intercept if specified in control
  #if (!control$include_intercept) {
  #  r <- r %>% filter(term != c('(Intercept)'))
  #}

  # remove covariates if specified in control
  if (!control$include_covariates) {
    r <- r %>% filter(!(term %in% c(all_covariates)))
  }

  return(r)
}

# helper function for aba summary
coef_fmt <- function(est, lo, hi, pval) {
  coef <- glue('{sprintf("%.2f", est)}')
  if (!is.na(lo)) {
    coef <- glue('{coef} [{sprintf("%.2f", lo)}, {sprintf("%.2f", hi)}]')
  }
  if (!is.na(pval)) {
    coef <- glue('{coef} (P={sprintf("%.4f", pval)})')
  }
  coef
}

# helper function for aba summary
coef_pivot_wider <- function(r) {
  r %>% rowwise() %>%
    mutate(
      estimate = as.character(coef_fmt(.data$estimate, .data$conf_low,
                                  .data$conf_high, .data$pval))
    ) %>%
    select(-c(pval, conf_low, conf_high)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = .data$term,
      values_from = .data$estimate
    )
}

# helper function for aba summary
metrics_summary <- function(model) {
  metric_vars <- c(
    'adj.r.squared',
    'R2',
    'AUC',
    #'Cut',
    'AIC',
    'Pval',
    'nobs',
    'nsub'
  )

  # add null model
  model_results <- model$results %>%
    group_by(group, outcome, stat) %>%
    mutate(
      stat_fit_null = list(
        ifelse(sum(c('M1','Basic') %in% .data$predictor_set)>0,
               .data$stat_fit[.data$predictor_set %in% c('M1','Basic')],
               NA)
      )[[1]]
    ) %>%
    ungroup()

  r <- model_results %>% rowwise() %>%
    mutate(
      stat_metrics = list(
        aba_glance(
          x = .data$stat_fit,
          x0 = .data$stat_fit_null
        )
      )
    )

  r <- r %>%
    unnest(
      .data$stat_metrics
    )

  r <- r %>%
    select(
      -c(.data$predictor, .data$covariate),
      -c(.data$stat_obj, .data$stat_fit, .data$stat_fit_null)
    )

  r <- r %>%
    filter(
      term %in% metric_vars
    ) %>%
    rename(
      estimate = estimate,
      conf_low = conf.low,
      conf_high = conf.high
    ) %>%
    arrange(
      match(term, metric_vars)
    )

  return(r)
}

# helper function for aba summary
metric_fmt <- function(est, lo, hi) {
  metric <- glue('{sprintf("%.2f", est)}')
  if (!is.na(lo)) {
    metric <- glue('{metric} [{sprintf("%.2f", lo)}, {sprintf("%.2f", hi)}]')
  }
  metric
}

# helper function for aba summary
metric_pivot_wider <- function(r) {
  r %>% rowwise() %>%
    mutate(
      estimate = as.character(metric_fmt(.data$estimate, .data$conf_low,
                                    .data$conf_high))
    ) %>%
    select(-c(conf_low, conf_high, pval)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = .data$term,
      values_from = .data$estimate
    )
}


#' Convert an aba summary to a nicely formatted table
#'
#' This function allows you to format an aba summary in the same way
#' which it is printed to the console using the `print` function. However,
#' only one dataframe will result (i.e., the tables will not be split by
#' group - outcome - stat combinations).
#'
#' @param object abaSummary. The aba summary to format as a table.
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' # use built-in data
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit an aba model
#' model <- data %>% aba_model() %>%
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
#' # default aba summary
#' model_summary <- model %>% aba_summary()
#'
#' # convert summary to table
#' my_table <- model_summary %>% as_table()
#'
as_table <- function(object) {
  x <- object

  x_res <- x$results %>%
    group_by(
      .data$group,
      .data$outcome,
      .data$stat
    ) %>%
    nest() %>%
    ungroup() %>%
    unnest(cols=c(.data$data))

  r_coef <- coef_pivot_wider(
    x_res %>%
      filter(form == 'coef') %>%
      select(-c('form'))
  ) %>%
    select(
      -c(any_of('(Intercept)'))
    )

  r_metric <- metric_pivot_wider(
    x_res %>%
      filter(form == 'metric') %>%
      select(-c('form'))
  ) %>%
    select(-nobs, everything())

  r_results <- r_coef %>%
    left_join(
      r_metric,
      by = c('group', 'outcome', 'stat', 'predictor_set')
    )

  # replace group names for printing if they exist
  if (!is.null(names(x$model$spec$groups))) {
    r_results$group <- factor(
      r_results$group,
      levels = x$model$spec$groups,
      labels = names(x$model$spec$groups)
    )
  }

  # replace outcome names for printing if they exist
  if (!is.null(names(x$model$spec$outcomes))) {
    r_results$outcome <- factor(
      r_results$outcome,
      levels = x$model$spec$outcomes,
      labels = names(x$model$spec$outcomes)
    )
  }
  r_results
}


#' @export
print.abaSummary <- function(x, ...) {
  args <- list(...)
  nprint <- 5
  if ('n' %in% names(args)) nprint <- args$n
  if (nprint == -1) nprint <- 1000

  x_res <- x$results %>%
    group_by(
      .data$group,
      .data$outcome,
      .data$stat
    ) %>%
    nest()

  r_length_orig <- x_res %>% nrow()

  x_res <- x_res %>%
    ungroup() %>%
    slice(1:nprint) %>%
    unnest(cols=c(.data$data))

  r_coef <- coef_pivot_wider(
    x_res %>%
      filter(form == 'coef') %>%
      select(-c('form'))
  ) %>%
    select(
      -c(any_of('(Intercept)'))
    )

  r_metric <- metric_pivot_wider(
    x_res %>%
      filter(form == 'metric') %>%
      select(-c('form'))
  )

  r_metric <- r_metric %>%
    select(-nobs, everything())

  r_results <- r_coef %>%
    left_join(
      r_metric,
      by = c('group', 'outcome', 'stat', 'predictor_set')
    )

  # replace group names for printing if they exist
  if (!is.null(names(x$model$spec$groups))) {
    r_results$group <- factor(
      r_results$group,
      levels = x$model$spec$groups,
      labels = names(x$model$spec$groups)
    )
  }

  # replace outcome names for printing if they exist
  if (!is.null(names(x$model$spec$outcomes))) {
    r_results$outcome <- factor(
      r_results$outcome,
      levels = x$model$spec$outcomes,
      labels = names(x$model$spec$outcomes)
    )
  }

  r_nested <- r_results %>%
    group_by(
      .data$group,
      .data$outcome,
      .data$stat
    ) %>%
    nest() %>%
    mutate(
      label = glue('{group} | {outcome} | {stat}')
    )

  if (!is.null(x$model$spec$treatment)) {
    r2_nested <- x$results_treatment$emmeans %>%
      group_by(group, outcome, stat) %>% nest() %>%
      rename(emmeans = data)
    r3_nested <- x$results_treatment$pairs %>%
      group_by(group, outcome, stat) %>% nest() %>%
      rename(pairs = data)
    r_nested <- r_nested %>%
      left_join(r2_nested, by=c('group','outcome','stat')) %>%
      left_join(r3_nested, by=c('group','outcome','stat'))
  }


  r_split <- stats::setNames(
    split(r_nested, 1:nrow(r_nested)),
    r_nested$label
  )

  r_length <- length(r_split)

  r_split %>% purrr::iwalk(
    function(x,y) {
      nchar_label <- nchar(y)
      cat('\n'); cat(rep('-', nchar_label), sep=''); cat('\n')
      cat(y)
      cat('\n'); cat(rep('-', nchar_label), sep=''); cat('\n')

      #cat('\n')
      #cat('Coefficients & Metrics:\n\n')

      # coefficients
      print(
        x$data[[1]][,colMeans(is.na(x$data[[1]])) < 1]
      )
      if ('emmeans' %in% colnames(x)) {
        cat('\nTreatment effects:\n\n')
        print(
          x$pairs[[1]] %>%
            select(-c(predictor, covariate, term, null.value, df, statistic)) %>%
            rename(comparison = contrast) %>%
            select(predictor_set, comparison, everything()) %>%
            rowwise() %>%
            mutate(
              estimate = as.character(
                glue('{sprintf("%.2f",estimate)} [{sprintf("%.2f",std.error)}] (P={sprintf("%.4f",p.value)})')
              )
            ) %>%
            select(-c(std.error, p.value, comparison)) %>%
            pivot_wider(names_from = WEEK, values_from = estimate,
                        names_prefix = 'Estimate_Time_')
        )
      }
    }
  )

  if (r_length < r_length_orig) {
    cat(glue('\n\n ... {r_length_orig - r_length} of',
    ' {r_length_orig} tables not printed ...\n\n\n'))
  }

}
