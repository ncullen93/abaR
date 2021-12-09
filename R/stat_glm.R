

#' Create a glm stat to use for an aba model.
#'
#' @return
#' list of the following functions:
#'   * `formula_fn`: create a formula
#'   * `fit_fn`: fit a model
#'   * `evaluate_fn`: evaluate a model
#'
#' @export
#'
#' @examples
#' my_stat <- stat_glm()
#'
#' my_formula <- my_stat$formula_fn(
#'   outcome='ConvertedToAlzheimers',
#'   predictors=c('PLASMA_PTAU181_bl','PLASMA_NFL_bl'),
#'   covariates=c('AGE','GENDER','EDUCATION')
#' )
#'
#' my_model <- my_stat$fit_fn(
#'   formula = my_formula,
#'   data = aba::adnimerge %>% dplyr::filter(VISCODE == 'bl')
#' )
stat_glm <- function(std.beta = FALSE,
                    complete.cases = TRUE,
                    include.basic = TRUE,
                    extra.metrics = NULL) {
  fns <- list(
    'formula_fn' = formula_std,
    'fit_fn' = fit_glm,
    'extra.metrics' = extra.metrics,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases,
      'include.basic' = include.basic
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
  withr::local_options(.new = list(warn = -1))

  x <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
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

  roc_obj <- pROC::roc(data, .Truth, .Predicted, ci = T, quiet = T)
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
  s <- stats::anova(fit, fit0)
  null_pval <- 1 - stats::pchisq(abs(s$Deviance[2]), abs(s$Df[2]))

  # combine broom::glance with extra metrics
  glance_df <- glance_df %>%
    dplyr::bind_cols(
      tibble::tibble(
        AUC = auc_val,
        Cut = cut_val,
        Pval = null_pval
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
#' @param model abaModel. Fitted aba model to plot.
#' @param ... other plotting parameters
#'
#' @return gpglot or list of ggplots
#' @export
#'
#' @examples
#' x <- 1
aba_plot_roc <- function(model_summary, ...) {
  model <- model_summary$model
  # nest data by distinct group - outcome pairs
  plot_df <- model$results %>%
    group_by(.data$groups, .data$outcomes, .data$stats) %>%
    nest()

  # create a plot for each group - outcome pair
  plot_df <- plot_df %>%
    rowwise() %>%
    mutate(
      plots = plot_roc_single(
        models = .data$data$stats_fit,
        stat = .data$data$stats,
        group = .data$groups,
        outcome = .data$outcomes,
        data = model$data
      )
    )

  g <- ggpubr::ggarrange(
    plotlist = plot_df$plots,
    common.legend = TRUE
  )
  return(g)
}

# models: list of model fits;
# group: string
# outcome: string
# data: tibble
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
                       quiet = T, ci=T, data=data, percent=T)
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

