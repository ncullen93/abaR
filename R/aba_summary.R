
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
  model <- object
  if (!model$is_fit) stop('You cant summarise a model which has not been fit.
  Please call "aba_fit()" on your model first.')

  # calculate coefs
  coefs_df <- object %>% calculate_coefs(control)

  # calculate metrics
  metrics_df <- object %>% calculate_metrics(control)

  s <- list(
    results = list(
      coefs = coefs_df,
      metrics = metrics_df
    ),
    control = control,
    model = model
  )

  if (adjust$method != 'none') {
    s$results <- adjust_pvals(s$results, adjust)
  }

  class(s) <- 'abaSummary'

  return(s)
}

#' @export
summary.abaModel <- function(object, ...) {
  object %>% aba_summary(...)
}

# helper function for aba summary
calculate_coefs <- function(object, control) {

  all_variables <- object$predictors %>% unlist() %>% unique()
  all_covariates <- object$covariates
  if (is.null(all_covariates)) all_covariates <- c('')

  # coefficients
  r <- object$results %>%
    mutate(
      coefs = purrr::map(
        .data$fit,
        function(.f) {
          if (is.null(.f)) return(NULL)

          aba_tidy(.f, all_variables, all_covariates) %>%
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
    select(-fit) %>%
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
coefs_pivot_wider <- function(object, wider = FALSE) {
  df <- object$results$coefs
  control <- object$control

  # handle digits
  df <- df %>%
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
    )

  df <- df %>%
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
    select(-c(conf_low, conf_high, pval)) %>%
    pivot_wider(
      names_from = term,
      values_from = estimate
    )

  df
}

# helper function for aba summary
coef_fmt <- function(est, lo, hi, pval) {
  coef <- glue('{est}')
  if (!is.na(lo) | !is.na(hi)) coef <- glue('{coef} [{lo}, {hi}]')
  if (!is.na(pval)) {
    if (grepl('<',pval)) {
      coef <- glue('{coef} (P{pval})')
    } else {
      coef <- glue('{coef} (P={pval})')
    }
  }
  coef
}

# helper function for aba_summary
calculate_metrics <- function(object, control) {
  metric_vars <- c(
    'adj.r.squared',
    'R2',
    'AUC',
    'AIC',
    'Pval',
    'nobs',
    'nsub'
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
      metrics = purrr::map2(
        .data$fit, .data$fit_basic,
        function(model, basic_model) {
          if (is.null(model)) return(NULL)
          x <- aba_glance(model, basic_model) %>%
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

metrics_pivot_wider <- function(object) {

  df <- object$results$metrics %>%
    mutate(
      estimate = purrr::pmap_chr(
        list(
          est = .data$estimate,
          lo = .data$conf_low,
          hi = .data$conf_high,
          term = .data$term
        ),
        metric_fmt,
        control = object$control
      )
    ) %>%
    select(-c(conf_low, conf_high)) %>%
    pivot_wider(
      names_from = term,
      values_from = estimate
    )

  if ('pval' %in% colnames(df)) {
    df <- df %>% mutate(
      pval = purrr::map_chr(pval, clip_metric, object$control$pval_digits)
    )

  }

  df
}

clip_metric <- function(metric, digits) {
  if (metric == paste0('0.',paste0(rep('0', digits),collapse=''))) {
    metric <- paste0('<0.',paste0(rep('0', digits-1),collapse=''),'1')
  }
  metric
}

default_digits_map <- list(
  'nobs' = 0,
  'nsub' = 0
)

# helper function for aba summary
metric_fmt <- function(est, lo, hi, term, control) {

  if (term %in% names(default_digits_map)) {
    digits <- default_digits_map[[term]]
  } else if (glue('{term}_digits') %in% names(control)) {
    digits <- control[[glue('{term}_digits')]]
  } else {
    digits <- control$metric_digits
  }
  fmt <- glue('%.{digits}f')

  metric <- glue('{sprintf({fmt}, est)}')
  if (!is.na(lo)) {
    metric <- glue('{metric} [{sprintf({fmt}, lo)}, {sprintf({fmt}, hi)}]')
  }

  metric
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
  df1 <- object %>% coefs_pivot_wider()
  df2 <- object %>% metrics_pivot_wider()
  df <- df2 %>%
    left_join(df1, by=c('group','outcome','stat','predictor')) %>%
    select(all_of(df1 %>% names),everything())
  df
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
#' my_table <- model_summary %>% as_reactable()
#'
as_reactable <- function(object) {
  mytable <- object %>% as_table() %>%
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

#' @export
print.abaSummary <- function(x, ...) {
  tbl <- x %>% as_table()

  tbl_nested <- tbl %>%
    group_by(
      .data$group,
      .data$outcome,
      .data$stat
    ) %>%
    nest() %>%
    mutate(
      label = glue('Group = {group} | Outcome = {outcome} | Stat = {stat}')
    )

  tbl_split <- stats::setNames(
    split(tbl_nested, 1:nrow(tbl_nested)),
    tbl_nested$label
  )

  tbl_length <- length(tbl_split)

  tbl_split %>% purrr::iwalk(
    function(x,y) {
      nchar_label <- nchar(y)
      cat('\n'); cat(rep('-', nchar_label), sep=''); cat('\n')
      cat(y)
      cat('\n'); cat(rep('-', nchar_label), sep=''); cat('\n')

      # coefficients
      print(
        x$data[[1]][,colMeans(is.na(x$data[[1]])) < 1]
      )
    }
  )


}
