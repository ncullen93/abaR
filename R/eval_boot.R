#' Create a bootstrap evaluator
#'
#' @param ntrials integer. number of trials to perform
#' @param conf_type string. How to calculate confidence interval of performance
#'   metrics across trials: 'norm' calcualtes std err using the 'sd' function,
#'   'perc' calculats lower and upper conf values using the 'quantile' function.
#' @param contrasts logical. Whether to compare performance of fits within
#'   each group-outcome-stat combination (i.e., between predictors). This will
#'   result in a p-value for each model comparison as the proporiton of trials
#'   where one model had a lower performance than another model. Thus, a p-value
#'   of 0.05 indicates that one model performed worse than the other model 5%
#'   of the trials. This is useful to compare models which are not overlapping
#'   in parameters and therefore cannot be compared using a typical anova call.
#'
#' @return aba model
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
#'   set_evals(eval_boot(ntrials = 5)) %>%
#'   fit()
eval_boot <- function(ntrials = 10,
                      conf_type = c('norm', 'perc'),
                      contrasts = TRUE) {
  conf_type <- match.arg(conf_type)

  struct <- list(
    ntrials = ntrials,
    conf_type = conf_type,
    contrasts = contrasts
  )
  struct$eval_type <- 'boot'
  class(struct) <- 'abaEval'

  struct
}


fit_boot <- function(model, ntrials, verbose = FALSE) {
  if (ntrials <= 1) stop('ntrials must be greater than 1.')

  # compile model
  fit_df <- model %>% aba_compile()
  ntrials <- 1 + ntrials

  # progress bar
  pb <- NULL
  if (verbose) pb <- progress::progress_bar$new(total = ntrials*nrow(fit_df))

  fit_df <- 1:ntrials %>%
    purrr::map(
      function(index) {
        # add data
        fit_df <- fit_df %>%
          group_by(group, outcome, stat) %>%
          nest() %>%
          rename(info=data) %>%
          rowwise() %>%
          mutate(
            data = process_dataset(
              data = model$data,
              group = group,
              outcome = outcome,
              stat = stat,
              predictors = model$predictors,
              covariates = model$covariates
            )
          ) %>%
          ungroup()

        # apply bootstrap sampling to data
        if (index != 1) {
          fit_df <- fit_df %>%
            mutate(
              data = purrr::map(
                data,
                function(data) {
                  data[sample(nrow(data), nrow(data), replace=TRUE),]
                }
              )
            )
        }

        # fit model
        fit_df <- fit_df %>%
          unnest(info) %>%
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
          select(gid, oid, sid, pid, fit) %>%
          rename(
            group = gid,
            outcome = oid,
            stat = sid,
            predictor = pid
          )

        fit_df <- fit_df %>% mutate(trial = index - 1)

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
  model$fit_type <- 'boot'
  return(model)
}

summary_boot <- function(object,
                         label,
                         control = aba_control(),
                         adjust = aba_adjust(),
                         verbose = FALSE) {

  if (length(object$evals) > 1) object$results <- object$results[[label]]
  eval_obj <- object$evals[[label]]
  conf_type <- eval_obj$conf_type
  contrasts <- eval_obj$contrasts

  ###########
  ## coefs ##
  ###########

  coefs_df <- object %>% calculate_coefs(control)

  coefs_df_orig <- coefs_df %>%
    filter(.data$trial==0) %>%
    select(-c(.data$trial, conf_low, conf_high))

  coefs_df_boot <- coefs_df %>%
    filter(.data$trial != 0) %>%
    group_by(group, outcome, stat, predictor, term) %>%
    summarise(
      estimate_boot = mean(estimate, na.rm=T),
      std_err = sd(estimate, na.rm=T),
      conf_low = quantile(estimate, 0.025, na.rm=T),
      conf_high = quantile(estimate, 0.975, na.rm=T),
      .groups = 'keep'
    ) %>%
    ungroup()


  coefs_df_proc <- coefs_df_orig %>%
    left_join(
      coefs_df_boot,
      by = c("group", "outcome", "stat", "predictor", "term")
    ) #%>%
    #mutate(bias = estimate_boot - estimate)

  if (conf_type == 'norm') {
    coefs_df_proc <- coefs_df_proc %>%
      mutate(
        conf_low = estimate - 1.96 * std_err,
        conf_high = estimate + 1.96 * std_err
      )
  }

  coefs_df_proc <- coefs_df_proc %>%
    select(group:term, estimate, conf_low, conf_high, pval, estimate_boot)

  #############
  ## metrics ##
  #############

  metrics_df <- object %>% calculate_metrics(control)

  metrics_df_orig <- metrics_df %>%
    filter(.data$trial==0) %>%
    select(-c(.data$trial, conf_low, conf_high))

  metrics_df_boot <- metrics_df %>%
    filter(.data$trial != 0) %>%
    group_by(group, outcome, stat, predictor, term) %>%
    summarise(
      estimate_boot = mean(estimate, na.rm=T),
      std_err = sd(estimate, na.rm=T),
      conf_low = quantile(estimate, 0.025, na.rm=T),
      conf_high = quantile(estimate, 0.975, na.rm=T),
      .groups = 'keep'
    ) %>%
    ungroup()

  metrics_df_proc <- metrics_df_orig %>%
    left_join(
      metrics_df_boot,
      by = c("group", "outcome", "stat", "predictor", "term")
    )

  if (conf_type == 'norm') {
    metrics_df_proc <- metrics_df_proc %>%
      mutate(
        conf_low = estimate - 1.96 * std_err,
        conf_high = estimate + 1.96 * std_err
      )
  }

  metrics_df_proc <- metrics_df_proc %>%
    select(group:term, estimate, conf_low, conf_high, estimate_boot)


  ###############
  ## contrasts ##
  ###############
  if (contrasts) {
    # only compare main performance metric
    metric <- metrics_df$term[1]
    contrasts_df <- metrics_df %>%
      filter(
        trial != 0,
        term == metric
      ) %>%
      select(-c(conf_low, conf_high)) %>%
      pivot_wider(names_from=predictor, values_from=estimate)

    xdf <- contrasts_df %>% select(all_of(unique(metrics_df$predictor)))

    cdf <- utils::combn(data.frame(xdf), 2, FUN = function(x) x[,1] - x[,2]) %>%
      data.frame() %>% tibble() %>%
      set_names(
        utils::combn(unique(metrics_df$predictor), 2,
              FUN = function(o) paste0(o[[1]],'_',o[[2]]))
      )

    contrasts_df <- contrasts_df %>%
      select(-all_of(unique(metrics_df$predictor))) %>%
      bind_cols(cdf)

    contrasts_df <- contrasts_df %>%
      group_by(group, outcome, stat, term) %>%
      summarise(
        across(colnames(cdf),
               list(
                 'estimate' = ~ mean(.x, na.rm=T),
                 'stderr' = ~ sd(.x, na.rm=T),
                 'conflow' = ~ quantile(.x, 0.025, na.rm=T),
                 'confhigh' = ~ quantile(.x, 0.975, na.rm=T),
                 'pval' = ~ mean(.x > 0, na.rm=T)
               )),
        .groups = 'keep'
      ) %>%
      ungroup()

    contrasts_df <- contrasts_df %>%
      pivot_longer(
        cols = -c(group, outcome, stat, term),
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
      select(-c(term, std_err))
  }

  results = list(
    coefs = coefs_df_proc,
    metrics = metrics_df_proc
  )
  if (contrasts) results$contrasts <- contrasts_df

  if (adjust$method != 'none') results <- adjust_pvals(results, adjust)

  results
}


as_table_contrasts <- function(results, control) {
  r <- results

  r <- r %>%
    mutate(
      across(estimate:conf_high,
             ~purrr::map_chr(., ~sprintf(glue('%.{control$coef_digits}f'), .))),
      pval = purrr::map_chr(
        pval,
        ~clip_metric(
          sprintf(glue('%.{control$pval_digits}f'), .),
          control$pval_digits
        )
      )
    ) %>%
    mutate(
      estimate = purrr::pmap_chr(
        list(
          est = .data$estimate,
          lo = .data$conf_low,
          hi = .data$conf_high,
          pval = .data$pval
        ),
        coef_fmt
      )
    ) %>%
    select(-c(conf_low, conf_high, pval))

  r <- r %>%
    pivot_wider(names_from = predictor2, values_from = estimate)

  r
}

as_table_standard_boot <- function(results, control) {
  # not sure what to do with bootstrap estimate / bias right now
  coefs <- results$coefs %>%
    select(-estimate_boot) %>%
    as_table_coefs(control)

  # not sure what to do with bootstrap estimate / bias right now
  metrics <- results$metrics %>%
    select(-estimate_boot) %>%
    as_table_metrics(control)

  tbl <- coefs %>%
    left_join(metrics, by=c('group','outcome','stat','predictor')) %>%
    select(all_of(colnames(coefs)),everything())

  list(
    'coefs_metrics_boot' = tbl
  )
}

as_table_boot <- function(results, control) {

  # table for coefs + metrics
  tbl <- as_table_standard_boot(
    results = results[c('coefs','metrics')],
    control = control
  )

  if ('contrasts' %in% names(results)) {
    tbl2 <- as_table_contrasts(
      results = results[['contrasts']],
      control = control
    )

    tbl <- c(
      tbl,
      list('contrasts' = tbl2)
    )
  }

  tbl
}


