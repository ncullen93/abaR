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
    'formula_fn' = formula_std,
    'fit_fn' = fit_glm,
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
aba_tidy.glm <- function(model, predictors, covariates, ...) {
  # using conf.int = T with broom::tidy gives warning if no covariates present
  x <- suppressWarnings(
    broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
  )
  x
}

# helper function for glm
aba_glance.glm <- function(x, x0, ...) {
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

  # Optimal Cutoff
  cut_model <- OptimalCutpoints::optimal.cutpoints(
    .Predicted ~ .Truth,
    data = data,
    tag.healthy=0, direction='<', methods='Youden'
  )
  cut_val <- cut_model$Youden$Global$optimal.cutoff$cutoff[1]

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
        Cut = cut_val
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
#' @param object abaSummary. A summary of an aba model with `stat_glm` type.
#'
#' @return a ggplot with roc curves for all predictor sets across each
#'   group - outcome - stat combination
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
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats(
#'     stat_glm(std.beta = TRUE)
#'   ) %>%
#'   fit()
#'
#' # summarise glm model
#' model_summary <- model %>% summary()
#'
#' fig <- model_summary %>% aba_plot_roc()
#'
aba_plot_roc <- function(object) {
  model <- object$model

  # nest data by distinct group - outcome pairs
  plot_df <- model$results %>%
    group_by(.data$group, .data$outcome, .data$stat) %>%
    nest()

  # create a plot for each group - outcome pair
  plot_df <- plot_df %>%
    rowwise() %>%
    mutate(
      plots = plot_roc_single(
        models = .data$data$fit,
        stat = .data$data$stat,
        group = .data$group,
        outcome = .data$outcome,
        data = model$data
      )
    )

  g <- ggpubr::ggarrange(
    plotlist = plot_df$plots,
    common.legend = TRUE
  )
  return(g)
}

# helper function for aba_plot_roc
plot_roc_single <- function(models, stat, group, outcome, data, ...) {
  group.name <- group
  outcome.name <- outcome
  tmp.models <- models

  roc.list <- tmp.models %>% purrr::map(
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

  g <- pROC::ggroc(roc.list, size=0.8) +
    geom_segment(aes(x = 100, xend = 0, y = 0, yend = 100),
                 alpha=0.1, color="grey", linetype="solid") +
    facet_wrap(.~paste0(outcome,' | ', group)) +
    xlab('Specificity, %') + ylab('Sensitivity, %') +
    theme_classic(base_size=16) +
    theme(legend.position='top',
          legend.margin = margin(5, 1, 1, 1),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
          legend.key.width = unit(0.75,"cm"),
          legend.text=element_text(size=10),
          panel.spacing = unit(1.5, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 16, vjust = 1.25),
          plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(
            colour = "black",
            size = 0.2, linetype = "dotted"))
  if (length(roc.list) < 8) {
    g <- ggpubr::set_palette(g, 'jama')
  }
  list(g)
}

