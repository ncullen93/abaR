#' Create a train-test evaluator
#'
#' @param split double between 0 and 1. percent of data to use as train set
#' @param ntrials integer. number of train-test trials to run
#' @param conf_type string. How to calculate confidence interval of performance
#'   metrics across trials: 'norm' calcualtes std err using the 'sd' function,
#'   'perc' calculats lower and upper conf values using the 'quantile' function.
#' @param contrasts logical. Whether to compare test performance of fits within
#'   each group-outcome-stat combination (i.e., between predictors). This will
#'   result in a p-value for each model comparison as the proporiton of trials
#'   where one model had a lower performance than another model. Thus, a p-value
#'   of 0.05 indicates that one model performed worse than the other model 5%
#'   of the trials. If ntrials == 1, then this value can only be 0 or 1 to
#'   indicate which model is better.
#' @return an aba model with modified evals parameter
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm') %>%
#'   set_evals('traintest') %>%
#'   fit()
eval_traintest <- function(split = 0.8,
                           ntrials = 1,
                           conf_type = c('norm', 'perc'),
                           contrasts = TRUE) {
  conf_type <- match.arg(conf_type)

  struct <- list(
    split = split,
    ntrials = ntrials,
    conf_type = conf_type,
    contrasts = contrasts
  )
  struct$eval_type <- 'traintest'
  class(struct) <- 'abaEval'

  struct
}

fit_traintest <- function(object, split = 0.8, ntrials = 1, verbose = FALSE) {

  model <- object
  if (is.null(model$groups)) model <- model %>% set_groups(everyone())
  if (is.null(model$predictors)) model$predictors <- list('Basic'=c())

  # compile model
  fit_df <- model %>% aba_compile()

  # progress bar
  pb <- NULL
  if (verbose) pb <- progress::progress_bar$new(total = nrow(fit_df))

  fit_df <- 1:ntrials %>%
    purrr::map(
      function(index) {
        fit_df <- fit_df %>%
          group_by(group, outcome, stat) %>%
          nest() %>%
          rename(info=data) %>%
          rowwise() %>%
          mutate(
            data = process_dataset(
              data = model$data,
              group = .data$group,
              outcome = .data$outcome,
              stat = .data$stat,
              predictors = model$predictors,
              covariates = model$covariates
            )
          ) %>%
          ungroup()

        fit_df <- fit_df %>%
          mutate(
            data = purrr::map(
              .data$data,
              function(data) {
                train_idx <- sample(nrow(data), split*nrow(data), replace=FALSE)
                test_idx <- setdiff(1:nrow(data), train_idx)
                data_train <- data[train_idx,]
                data_test <- data[test_idx,]
                list('data'=data_train, 'data_test'=data_test)
              }
            )
          ) %>%
          unnest_wider(data) %>%
          unnest(info)

        # fit model
        fit_df <- fit_df %>%
          rowwise() %>%
          mutate(
            fit = fit_stat(
              data = .data$data,
              outcome = .data$outcome,
              stat = .data$stat,
              predictors = .data$predictor,
              covariates = .data$covariate,
              pb = pb
            )
          ) %>%
          ungroup()

        # select only factor labels and fit
        fit_df <- fit_df %>%
          select(gid, oid, sid, pid, fit, .data$data_test) %>%
          rename(
            group = gid,
            outcome = oid,
            stat = sid,
            predictor = pid
          )

        fit_df <- fit_df %>% mutate(trial = index)

        # check that all models are not null
        if (sum(purrr::map_lgl(fit_df$fit, ~!is.null(.))) == 0) {
          stop('All models failed to be fit. Check your model setup.')
        }
        fit_df
      }
    ) %>%
    bind_rows()

  model$results <- fit_df
  model$is_fit <- TRUE
  model$fit_type <- 'traintest'
  return(model)
}


summary_traintest <- function(model,
                              label,
                              control = aba_control(),
                              adjust = aba_adjust(),
                              verbose = FALSE) {
  if (length(model$evals) > 1) model$results <- model$results[[label]]
  results <- model$results
  ntrials <- max(results$trial)
  eval_obj <- model$evals[[label]]
  conf_type <- eval_obj$conf_type
  contrasts <- eval_obj$contrasts

  # grab stat object
  results <- results %>%
    mutate(
      stat_obj = purrr::map(stat, ~model$stats[[.]])
    )

  # use evaluate function from stat object on fitted model and test data
  results <- results %>%
    mutate(
      results_test = purrr::pmap(
        list(stat_obj, fit, data_test),
        function(stat_obj, fit, data_test) {
          x <- stat_obj$fns$evaluate(fit, data_test)
          x
        }
      )
    )

  results <- results %>%
    select(-c(fit, data_test, stat_obj)) %>%
    unnest(results_test)

  # store for calculating contrasts later
  results_raw <- results

  # summarise values
  results <- results %>%
    pivot_longer(rmse:mae) %>%
    group_by(group, outcome, stat, predictor, form, name) %>%
    summarise(
      estimate = mean(value),
      std_err = sd(value),
      conf_low = quantile(value, 0.025, na.rm=T),
      conf_high = quantile(value, 0.975, na.rm=T),
      .groups='keep'
    ) %>%
    ungroup()

  results_train <- results %>%
    filter(form == 'train') %>%
    select(group:predictor, name, estimate) %>%
    rename(estimate_train = estimate)

  results <- results %>%
    filter(form == 'test') %>%
    select(-form) %>%
    left_join(
      results_train,
      by = c("group", "outcome", "stat", "predictor", "name")
    ) %>%
    rename(term = name)

  if (conf_type == 'norm') {
    results <- results %>%
      mutate(
        conf_low = estimate - 1.96 * std_err,
        conf_high = estimate + 1.96 * std_err
      )
  }

  if (ntrials == 1) results <- results %>% mutate(conf_low = NA, conf_high = NA)

  results <- results %>%
    select(group:term, estimate, conf_low, conf_high, estimate_train)

  results_list <- list(
    test_metrics = results
  )

  if (contrasts) {
    metric <- results_raw %>% select(-c(group:form)) %>% names() %>% head(1)
    contrasts_df <- results_raw %>%
      filter(form == 'test') %>%
      rename(estimate = {{ metric }}) %>%
      select(group:form, estimate) %>%
      pivot_wider(names_from=predictor, values_from=estimate)

    xdf <- contrasts_df %>% select(all_of(unique(results_raw$predictor)))

    cdf <- combn(data.frame(xdf), 2, FUN = function(x) x[,1] - x[,2]) %>%
      data.frame() %>% tibble() %>%
      set_names(
        combn(unique(results_raw$predictor), 2,
              FUN = function(o) paste0(o[[1]],'_',o[[2]]))
      )

    contrasts_df <- contrasts_df %>%
      select(-all_of(unique(results_raw$predictor))) %>%
      bind_cols(cdf)

    contrasts_df <- contrasts_df %>%
      group_by(group, outcome, stat) %>%
      summarise(
        across(colnames(cdf),
               list(
                 'estimate' = ~ mean(.x, na.rm=T),
                 'stderr' = ~ sd(.x, na.rm=T),
                 'conflow' = ~ quantile(.x, 0.025, na.rm=T),
                 'confhigh' = ~ quantile(.x, 0.975, na.rm=T),
                 'pval' = ~ mean(.x < 0, na.rm=T) # direction should be inferred
               )),
        .groups = 'keep'
      ) %>%
      ungroup()

    contrasts_df <- contrasts_df %>%
      pivot_longer(
        cols = -c(group, outcome, stat),
        names_to=c('predictor', 'predictor2', 'form'),
        names_sep = '_'
      ) %>%
      pivot_wider(names_from = form, values_from = value) %>%
      rename(conf_low = conflow, conf_high = confhigh, std_err = stderr)

    if (conf_type == 'norm') {
      contrasts_df <- contrasts_df %>%
        mutate(
          conf_low = estimate - 1.96 * std_err,
          conf_high = estimate + 1.96 * std_err
        )
    }

    contrasts_df <- contrasts_df %>%
      select(-c(std_err))

    results_list$contrasts <- contrasts_df
  }

  results_list
}

as_table_test_metrics <- function(results, control) {
  r <- results %>%
    mutate(
      estimate = purrr::pmap_chr(
        list(
          est = .data$estimate,
          lo = .data$conf_low,
          hi = .data$conf_high,
          term = .data$term
        ),
        metric_fmt,
        control = control
      )
    ) %>%
    select(-c(conf_low, conf_high, estimate_train))

  r <- r %>%
    pivot_wider(names_from = term, values_from = estimate)

  r
}

as_table_traintest <- function(results, control) {

  # test metrics table
  tbl <- as_table_test_metrics(
    results = results$test_metrics,
    control = control
  )
  tbl <- list('test_metrics' = tbl)

  # contrasts table
  if ('contrasts' %in% names(results)) {
    tbl2 <- as_table_contrasts(
      results = results$contrasts,
      control = control
    )

    tbl <- c(
      tbl,
      list('contrasts' = tbl2)
    )
  }

  tbl
}
