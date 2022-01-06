#' Create a glm stat object.
#'
#' This function creates a glm stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a traditional logistic regression analysis using the `glm` function with
#' a binary outcome. Coefficients will be presented as odds ratios. Default
#' metrics include AUC.
#'
#' @param std.beta logical. Whether to standardize model predictors and
#'   covariates prior to analysis.
#' @param complete.cases  logical. Whether to only include the subset of data
#'   with no missing data for any of the outcomes, predictors, or covariates.
#'   Note that complete cases are considering within each group - outcome
#'   combination but across all predictor sets.
#'
#' @return An abaStat object with `glm` stat type.
#' @export
#'
#' @examples
#'
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit glm model with binary outcome variables
#' model <- data %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats(
#'     stat_glm(std.beta = TRUE)
#'   ) %>%
#'   fit()
#'
#' # summarise glm model
#' model_summary <- model %>% summary()
#'
#' # plot glm results
#' fig1 <- model_summary %>% aba_plot_coef()
#' fig2 <- model_summary %>% aba_plot_metric()
#' fig3 <- model_summary %>% aba_plot_roc()
#'
stat_glm <- function(std.beta = FALSE,
                     complete.cases = TRUE) {
  fns <- list(
    'fns' = list(
      'formula' = formula_lm,
      'fit' = fit_glm,
      'tidy' = tidy_glm,
      'glance' = glance_glm
    ),
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    )
  )
  fns$stat_type <- 'glm'
  class(fns) <- 'abaStat'
  return(fns)
}

# helper function for glm
fit_glm <- function(formula, data, ...) {
  model <- stats::glm(
    stats::formula(formula),
    family = 'binomial',
    data = data,
    na.action = na.omit
  )
  model$call$formula <- stats::formula(formula)
  return(model)
}

# helper function for glm
tidy_glm <- function(model, predictors, covariates, ...) {
  # using conf.int = T with broom::tidy gives warning if no covariates present
  x <- suppressWarnings(
    broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
  )
  x
}

# helper function for glm
glance_glm <- function(x, x0, ...) {
  # tidy glance
  glance_df <- broom::glance(x)

  # custom glance
  fit <- x
  fit0 <- x0
  data <- stats::model.frame(fit)
  outcome <- colnames(data)[1]

  data$.Predicted <- stats::predict(fit, type='response')
  data$.Truth <- factor(data[[outcome]])

  roc_obj <- pROC::roc(data, .Truth, .Predicted, ci = TRUE, quiet = TRUE)
  auc_val <- roc_obj$auc[1]
  auc_val_lo <- roc_obj$ci[1]
  auc_val_hi <- roc_obj$ci[3]

  # add other metrics here... sens, spec, ppv, npv, etc..
  # ...

  ## Optimal Cutoff
  #cut_model <- OptimalCutpoints::optimal.cutpoints(
  #  .Predicted ~ .Truth,
  #  data = data,
  #  tag.healthy=0, direction='<', methods='Youden'
  #)
  #cut_val <- cut_model$Youden$Global$optimal.cutoff$cutoff[1]

  # compare current model to null model
  if (!is.null(fit0)) {
    s <- stats::anova(fit, fit0)
    null_pval <- 1 - stats::pchisq(abs(s$Deviance[2]), abs(s$Df[2]))
    glance_df <- glance_df %>%
      bind_cols(tibble::tibble(Pval = null_pval))
  }


  # combine broom::glance with extra metrics
  glance_df <- glance_df %>%
    bind_cols(
      tibble::tibble(
        AUC = auc_val,
        #Cut = cut_val
      )
    )

  # pivot longer to be like coefficients
  glance_df <- glance_df %>%
    pivot_longer(cols = everything()) %>%
    rename(term = name, estimate = value)

  # add confidence intervals
  glance_df <- glance_df %>%
    left_join(
      tibble::tibble(
        term = c('AUC'),
        conf.low = c(auc_val_lo),
        conf.high = c(auc_val_hi)
      ),
      by = 'term'
    )

  #print(glance_df)

  return(glance_df)
}



#' Plot ROC curves from an aba model
#'
#' This function plots ROC curves across group - outcome - stat combinations
#' and currently supports `stat_glm`.
#'
#' @param model abaModel. A fitted aba model with `stat_glm` type.
#' @param drop_facet_labels logical. Whether to remove facet labels from plots.
#'   The facet labels tell you what the group and outcome is for the plot.
#'   Sometimes these labels are unnecessary when you have only one group and
#'   one outcome, or when you want to add labels in another way.
#'
#' @return a ggplot with roc curves for all predictor sets across each
#'   group - outcome - stat combination
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit glm model with binary outcome variables
#' model <- data %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats(
#'     stat_glm(std.beta = TRUE)
#'   ) %>%
#'   fit()
#'
#' fig <- model %>% aba_plot_roc()
aba_plot_roc <- function(model, drop_facet_labels = FALSE) {
  if ('abaSummary' %in% class(model)) model <- model$model

  # nest data by distinct group - outcome pairs
  plot_df <- model$results %>%
    group_by(.data$group, .data$outcome, .data$stat) %>%
    nest()

  n_combos <- nrow(plot_df)

  # create a plot for each group - outcome pair
  plot_df <- plot_df %>%
    rowwise() %>%
    mutate(
      plots = plot_roc_single(
        models = .data$data$fit,
        stat = .data$data$stat,
        group = .data$group,
        outcome = .data$outcome,
        predictor = .data$data$predictor,
        data = model$data,
        drop_facet_labels = drop_facet_labels
      )
    )

  g <- ggpubr::ggarrange(
    plotlist = plot_df$plots,
    common.legend = ifelse(n_combos > 1, TRUE, FALSE)
  )
  return(g)
}

# helper function for aba_plot_roc
plot_roc_single <- function(models, stat, group, outcome, predictor, data,
                            drop_facet_labels) {
  group.name <- group
  outcome.name <- outcome
  tmp.models <- models


  roc.list <- tmp.models %>%
    set_names(predictor) %>%
    purrr::map(
      function(tmp.model) {
        # get dataset from model
        data <- stats::model.frame(tmp.model)

        data$tmp.outcome <- data[,as.character(tmp.model$formula)[2]]
        data$tmp.outcome.pred <- stats::predict(tmp.model, type = "response")

        res <- pROC::roc(tmp.outcome ~ tmp.outcome.pred,
                         quiet = TRUE, ci=T, data=data, percent=TRUE)
        return(res)
      }
    )

  g <- pROC::ggroc(roc.list) +
    geom_segment(aes(x = 100, xend = 0, y = 0, yend = 100),
                 alpha=0.1, color="grey", linetype="solid")

  if (!drop_facet_labels) g <- g + facet_wrap(.~paste0(outcome,' | ', group))

  g <- g +
    xlab('Specificity (%)') + ylab('Sensitivity (%)') +
    theme_aba(axis_title = TRUE)

  if (length(roc.list) < 8) {
    g <- ggpubr::set_palette(g, 'jama')
  }
  list(g)
}


#' Plot risk density split by binary outcome class
#'
#' This function plots risk density curves across group - outcome - stat
#' combinations and split by binary outcome. It currently supports `stat_glm`.
#'
#' @param model abaModel. A fitted aba model with `stat_glm` type.
#' @param risk_type string. Whether to use absolute or relative risk.
#' @param drop_basic logical. Whether to drop the basic model or not.
#'
#' @return a ggplot with risk density curves split by binary outcome value.
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit glm model with binary outcome variables
#' model <- data %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats(
#'     stat_glm(std.beta = TRUE)
#'   ) %>%
#'   fit()
#'
#' fig <- model %>% aba_plot_risk_density()
aba_plot_risk_density <- function(model,
                                  risk_type = c('absolute', 'relative'),
                                  drop_basic = FALSE) {
  risk_type <- match.arg(risk_type)

  df_risk <- model %>%
    aba_predict(augment=T, merge=F)

  if (drop_basic) df_risk <- df_risk %>% filter(predictor != 'Basic')

  df_risk <- df_risk %>%
    rowwise() %>%
    mutate(
      fig = plot_risk_density_single(
        data = .data$data,
        risk_type = risk_type
      )
    ) %>%
    ungroup() %>%
    select(-data)

  df_risk
}

percentile <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

plot_risk_density_single <- function(data, risk_type) {
  outcome <- colnames(data)[1]
  if (!is.factor(data[[outcome]])) data[[outcome]] <- factor(data[[outcome]])
  if (risk_type == 'relative') {
    data <- data %>% mutate(.fitted = percentile(.fitted))
  }

  data <- data %>% mutate(.fitted = 100 * .fitted)

  g <- data %>%
    ggplot(aes(x = .fitted, group = .data[[outcome]], fill = .data[[outcome]])) +
    geom_density(aes(y = .data$..density.. * 100), alpha = 0.5) +
    theme_aba(axis_title = TRUE) +
    xlab(ifelse(risk_type == 'absolute', 'Absolute risk (%)', 'Relative risk (%)')) +
    ylab('Density (%)')

  g <- ggpubr::set_palette(g, 'jama')

  list(g)
}


#' Plot predictor values versus predicted risk from fitted aba model
#'
#' This function plots real predictor values versus predicted risk (either
#' absolute or relative) from a fitted aba model with glm stats.
#'
#' @param model fitted abaModel. The model to plot
#' @param term_labels list. Names and values of predictors and their labels
#'   to replace them with in the plot.
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#' # fit glm model with binary outcome variables
#' model <- data %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats(
#'     stat_glm(std.beta = FALSE)
#'   ) %>%
#'   fit()
#' g <- model %>% aba_plot_predictor_risk()
aba_plot_predictor_risk <- function(model, term_labels = NULL) {
  res <- model %>% aba_predict(augment=T, merge=F)

  res <- res %>%
    mutate(
      fig = purrr::map(
        data,
        function(res) {

          res <- res %>%
            pivot_longer(-c(1, ncol(res)))

          # replace variable names with user-specified labels if given
          if (!is.null(term_labels)) {
            if (!is.list(term_labels)) stop('term_labels should be a list.')
            res <- res %>%
              mutate(
                name = map_chr(
                  name, ~ifelse(. %in% names(term_labels), term_labels[[.]], .)
                )
              )
          }

          outcome <- colnames(res)[1]
          res$outcome <- factor(res[[outcome]])

          g <- res %>%
            ggplot(aes(x=value, y = 100*.fitted, color=outcome)) +
            geom_point() +
            facet_wrap(.~name, scales = 'free')+
            theme_aba(axis_title = TRUE) +
            ylab('Predicted absolute risk (%)') +
            xlab('Biomarker value')

          g <- ggpubr::set_palette(g, 'jama')
          g
        }
      )
    )
  res
}
