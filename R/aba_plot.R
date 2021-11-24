#' Generic plot metric method
#'
#' @param object aba model summary
#' @param ... additional parameters
#'
#' @return ggplot
#' @export
#'
#' @examples
#' x <- 1
aba_plot_metric <- function(object, metric = NULL, model_labels = NULL, ...) {
  UseMethod('aba_plot_metric')
}

#' Generic plot coef method
#'
#' @param object aba model summary
#' @param ... additional parameters
#'
#' @return ggplot
#' @export
#'
#' @examples
#' x <- 1
aba_plot_coef <- function(object, ...) {
  UseMethod('aba_plot_coef')
}


#' Plot performance metrics from an aba model
#'
#' @param model abaModel. Fitted aba model to plot.
#' @param ... other plotting parameters
#'
#' @return ggplot or list of ggplots
#' @export
#'
#' @examples
#' x <- 1
aba_plot_metric.abaSummary <- function(object,
                                       metric = NULL,
                                       model_labels = NULL,
                                       ...) {
  # find main metric - directly after predictors
  if ('AUC' %in% (object$results %>% pull(term) %>% unique())) {
    metric <- 'AUC'
  } else {
    metric <- 'adj.r.squared'
  }

  plot_df <- object$results %>%
    filter(
      form == 'metric',
      term == metric
    )

  if (!is.null(model_labels)) {
    plot_df <- plot_df %>%
      mutate(MID = factor(MID, labels=model_labels))
  }

  g <- ggplot(plot_df,
              aes(x = .data$MID,
                  y = .data$est,
                  color = .data$outcomes)) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    geom_errorbar(
      aes(ymin = .data$lo,
          ymax = .data$hi),
      position=position_dodge(0.5), size=0.5,
      width = 0.2
    ) +
    ylab(metric)

  if (metric == 'AUC') {
    g <- g + ylim(c(0.5, 1))
    g <- g + geom_hline(aes(yintercept=0.5), linetype='dashed')
  }

  g <- g +
    facet_wrap(. ~ .data$groups) +
    theme_classic(base_size = 16) +
    theme(
      legend.position = "top", legend.margin = margin(5, 0, 0, 0),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 16, vjust = 1.25),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = "black",
        size = 0.2, linetype = "dotted"),
      axis.title.x = element_blank()
    )

  return(g)
}

#' aba summary plot coef method
#'
#' @param object aba model summary
#' @param ... additional parameters
#'
#' @return ggplot
#' @export
#'
#' @examples
#' x <- 1
aba_plot_coef.abaSummary <- function(model_summary, ...) {

  model_type <- NA
  if ('AUC' %in% (model_summary$results %>% pull(term) %>% unique())) {
    model_type <- 'glm'
  } else {
    model_type <- 'lm'
  }

  model <- model_summary$model
  all_predictors <- model %>% get_predictors()
  all_covariates <- model_summary$model$spec$covariates
  all_variables <- c(all_covariates, all_predictors)

  plot_df <- model_summary$results %>%
    filter(
      form == 'coef',
      term != '(Intercept)'
    )

  g <- ggplot(plot_df, aes(x = .data$term,
                           y = .data$est,
                           color = .data$MID)) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    geom_errorbar(
      aes(ymin = .data$lo,
          ymax = .data$hi),
      position=position_dodge(0.5), size=0.5,
      width = 0.2
    )

  if (model_type == 'glm') {
    g <- g + geom_hline(aes(yintercept=1), linetype='dashed')
  } else if (model == 'lm') {
    g <- g + geom_hline(aes(yintercept=0), linetype='dashed')
  }

  g <- g +
    facet_wrap(.~paste0(.data$outcomes,' | ', .data$groups)) +
    theme_classic(base_size = 16) +
    theme(
      legend.position = "top", legend.margin = margin(5, 0, 0, 0),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 16, vjust = 1.25),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = "black",
        size = 0.2, linetype = "dotted"),
      axis.title.x = element_blank()
    )

  g <- g + coord_flip()

  return(g)
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
