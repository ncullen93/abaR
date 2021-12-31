#' @export
summary.abaModel <- function(object,
                             control = aba_control(),
                             adjust = aba_adjust(),
                             verbose = FALSE,
                             ...) {
  object %>% aba_summary(...)
}

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
#' @param adjust abaAdjust. An aba adjust object which allows users to
#'   specify p-value adjustment using a variety of methods and across arbitrary
#'   model factors.
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
#' my_control <- aba_control(include_covariates = FALSE)
#'
#' # summarise model with th custom aba control - notice covariates
#' # wont be included in the tables when you print the summary to console
#' model_summary2 <- model %>% aba_summary(control = my_control)
#'
aba_summary <- function(object,
                        control = aba_control(),
                        adjust = aba_adjust(),
                        verbose = FALSE) {
  if (!object$is_fit) stop('You cant summarise a model which has not been fit.
  Please call "aba_fit()" on your model first.')
  model <- object

  # go through all evals
  evals <- model$evals

  results <- evals %>%
    purrr::imap(
      function(.eval, .label) {
        summary_fn <- methods::getFunction(glue('summary_{.eval$eval_type}'))
        tmp_summary <- summary_fn(model, .label, control, adjust, verbose)
        tmp_summary
      }
    )

  if (length(evals) == 1) results <- results[[1]]

  s <- list(
    results = results,
    control = control,
    model = object,
    verbose = verbose
  )

  class(s) <- 'abaSummary'

  return(s)
}

# helper function for aba summary
calculate_coefs <- function(object, control) {

  all_variables <- object$predictors %>% unlist() %>% unique()
  all_covariates <- object$covariates
  if (is.null(all_covariates)) all_covariates <- c('')

  # coefficients
  r <- object$results %>%
    mutate(
      coefs = purrr::map2(
        .data$fit, .data$stat,
        function(.fit, stat_label) {
          if (is.null(.fit)) return(NULL)
          stat_obj <- object$stats[[stat_label]]
          stat_obj$fns$tidy(.fit, all_variables, all_covariates) %>%
            rename(
              conf_low = conf.low,
              conf_high = conf.high,
              pval = p.value
            ) %>%
            select(term, estimate, conf_low, conf_high, pval)
        }
      )
    )

  # unnest coefficient tables and remove NA coefficients
  r <- r %>%
    select(-.data$fit) %>%
    unnest(.data$coefs) %>%
    filter(!is.na(estimate))

  # remove covariates if specified
  if (!control$include_covariates) {
    r <- r %>% filter(!term %in% object$covariates)
  }

  # remove intercept if specified
  if (!control$include_intercept) {
    r <- r %>% filter(!term == '(Intercept)')
  }

  return(r)
}

# helper function for aba_summary
calculate_metrics <- function(object, control) {
  metric_vars <- c(
    'adj.r.squared',
    'R2',
    'AUC',
    'concordance',
    'AIC',
    'Pval',
    'nobs',
    'nsub',
    'nevent'
  )

  # add null model
  r <- object$results %>%
    group_by(group, outcome, stat) %>%
    mutate(
      fit_basic = list(
        ifelse(sum('Basic' %in% .data$predictor) > 0,
          .data$fit[.data$predictor == 'Basic'],
          NULL
        )
      )[[1]]
    ) %>%
    ungroup()

  # remove basic model when complete.cases = F because we cannot compare
  # models fit on different sizes of data (e.g., pvalue and aic is not valid)
  r <- r %>%
    rowwise() %>%
    mutate(complete_cases = object$stats[[.data$stat]]$params$complete.cases) %>%
    ungroup() %>%
    mutate(fit_basic = ifelse(.data$complete_cases, fit_basic, list(NULL)))

  # coefficients
  r <- r %>%
    mutate(
      metrics = purrr::pmap(
        list(.data$fit, .data$fit_basic, .data$stat),
        function(model, basic_model, stat_label) {
          if (is.null(model)) return(NULL)
          stat_obj <- object$stats[[stat_label]]
          x <- stat_obj$fns$glance(model, basic_model) %>%
            filter(term %in% metric_vars) %>%
            rename(conf_low = conf.low, conf_high = conf.high) %>%
            arrange(match(term, metric_vars)) %>%
            mutate(term = tolower(term))
          x
        }
      )
    )

  # remove aic if !complete_cases
  process_metrics <- function(m, c) {
    if (!c) m <- m %>% filter(!(term %in% c('aic')))
    m
  }

  r <- r %>%
    rowwise() %>%
    mutate(
      metrics = list(process_metrics(.data$metrics, .data$complete_cases))
    ) %>%
    ungroup()

  r <- r %>%
    select(-any_of(c('fit', 'fit_basic', 'complete_cases'))) %>%
    unnest(metrics) %>%
    filter(!is.na(estimate))

  r
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
as_table.abaSummary <- function(object) {
  control <- object$control
  n_evals <- length(object$model$evals)
  results <- object$results

  if (n_evals == 1) results <- list(results)

  tables <- seq_along(object$model$evals) %>%
    purrr::map(
      function(eval_idx) {
        eval_obj <- object$model$evals[[eval_idx]]
        res <- results[[eval_idx]]
        tbl_fn <- methods::getFunction(glue('as_table_{eval_obj$eval_type}'))
        tbl <- tbl_fn(res, control)
        tbl
      }
    )

  if (n_evals == 1) tables <- tables[[1]]
  tables
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
as_table <- function(object) {
  UseMethod('as_table')
}

#' Convert an aba summary to a interactive react table
#'
#' This function allows you to format an aba summary in the same way
#' which it is printed to the console using the `print` function. And then
#' it will be converted to an interactive react table that can be explored
#' in the Rstudio viewer or in a Shiny app.
#'
#' @param object abaSummary. The aba summary to format as a reacttable.
#'
#' @return A reactable object from the reactable package
#' @export
#'
#' @examples
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
#' my_table <- model_summary %>% as_reactable()
as_reactable.abaSummary <- function(object) {
  mytable <- object %>% as_table() %>% map_df(~.x) %>%
    tidyr::unite('grouping', group, outcome, stat, sep=' | ')

  reactable::reactable(mytable, groupBy=c('grouping'),
                       pagination = FALSE,
                       sortable = FALSE,
                       showPageInfo = FALSE,
                       defaultColDef = reactable::colDef(
                         header = function(value) gsub(".", " ", value, fixed = TRUE),
                         cell = function(value) format(value, nsmall = 1),
                         align = "center",
                         minWidth = 140,
                         html=TRUE,
                         headerStyle = list(background = "#f7f7f8")
                       ),
                       style = list(fontFamily = "Work Sans, sans-serif",
                                    fontSize = "14px"),
                       compact=F,
                       striped = TRUE)
}

#' Convert an aba summary to a interactive react table
#'
#' This function allows you to format an aba summary in the same way
#' which it is printed to the console using the `print` function. And then
#' it will be converted to an interactive react table that can be explored
#' in the Rstudio viewer or in a Shiny app.
#'
#' @param object abaSummary. The aba summary to format as a reacttable.
#'
#' @return A reactable object from the reactable package
#' @export
#'
#' @examples
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
#' my_table <- model_summary %>% as_reactable()
as_reactable <- function(object) {
  UseMethod('as_reactable')
}

tup <- function(x) {
  stringr::str_to_title(x)
}

#' @export
print.abaSummary <- function(x, ...) {
  params <- list(...)

  split <- c('group', 'outcome')
  if (length(x$model$outcomes) >= 5*length(x$model$predictors)) {
    split <- c('group', 'predictor')
  }
  if ('split' %in% names(params)) split <- params$split
  if (length(split) != 2) stop('split must have length == 2.')
  a1 <- split[1]
  a2 <- split[2]

  tbl <- x %>% as_table()
  if (tibble::is_tibble(tbl)) tbl <- list(tbl)

  tbls <- tbl %>% purrr::imap(
    function(tbl, tbl_label) {
      tbl_nested <- tbl %>%
        group_by(
          .data[[a1]],
          .data[[a2]],
          .data$stat
        ) %>%
        nest() %>%
        mutate(
          label =
            glue('{tup(a1)}: {.data[[a1]]} | {tup(a2)}: {.data[[a2]]} | Stat: {stat}'),
          tbl_label = tbl_label
        )
    }
  )

  tbls_nested <- tbls %>% bind_rows() %>% group_by(.data$label, .add=T) %>% nest()

  tbl_split <- stats::setNames(
    split(tbls_nested, 1:nrow(tbls_nested)),
    tbls_nested$label
  )

  tbl_length <- length(tbl_split)

  tbl_label_map <- list(
    'coefs_metrics' = 'Coefficients & Metrics',
    'coefs_metrics_boot' = 'Coefficients & Metrics (bootstrapped)',
    'contrasts' = 'Contrasts',
    'test_metrics' = 'Test Performance'
  )

  tbl_split %>% purrr::iwalk(
    function(x,y) {
      nchar_label <- nchar(y)
      cat(rep('-', nchar_label), sep=''); cat('\n')
      cat(y)
      cat('\n'); cat(rep('-', nchar_label), sep=''); cat('\n')

      all_data <- x$data[[1]]
      for (row_idx in 1:nrow(all_data)) {
        tmp_data <- all_data$data[[row_idx]]
        tmp_label <- all_data$tbl_label[[row_idx]]
        cat(tbl_label_map[[tmp_label]]); cat(':\n')
        print(
          tmp_data[,colMeans(is.na(tmp_data)) < 1]
        )
        cat('\n')
      }
    }
  )
}


#' @export
aba_plot.abaSummary <- function(object, ...) {
  g_metric <- object %>% aba_plot_metric(...)
  g_coef <- object %>% aba_plot_coef(...)
  fig <- suppressWarnings(ggpubr::ggarrange(
    g_metric, g_coef, nrow = 2
  ))
  fig
}




