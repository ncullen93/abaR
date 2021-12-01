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
aba_plot_metric <- function(object,
                            metric = NULL,
                            x = 'predictor_set',
                            group = 'outcome',
                            facet = 'group',
                            coord_flip = FALSE,
                            palette = 'jama',
                            labels = NULL,
                            plotly = FALSE,
                            ...) {
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
aba_plot_coef <- function(object,
                          x = 'predictor',
                          group = 'predictor_set',
                          facet = c('outcome', 'group'),
                          coord_flip = FALSE,
                          palette = 'jama',
                          labels = NULL,
                          plotly = FALSE,
                          ...) {
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
                                       x = 'predictor_set',
                                       group = 'outcome',
                                       facet = 'group',
                                       coord_flip = FALSE,
                                       palette = 'jama',
                                       labels = NULL,
                                       plotly = FALSE,
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
    ) %>%
    rename(
      'x' = {{ x }},
      'group' = {{ group }},
      'facet' = {{ facet }}
    )



  g <- ggplot(plot_df,
              aes(x = .data$x,
                  y = .data$estimate,
                  color = .data$group)) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    geom_errorbar(
      aes(ymin = .data$conf_low,
          ymax = .data$conf_high),
      position=position_dodge(0.5), size=0.5,
      width = 0.2
    ) +
    facet_wrap(. ~ .data$facet) +
    ylab(metric)

  if (metric == 'AUC') {
    g <- g + ylim(c(min(0.5, plot_df$conf_low), 1))
    g <- g + geom_hline(aes(yintercept=0.5), linetype='dashed')
  }

  g <- g +
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

  if (coord_flip) g <- g + coord_flip()

  if (plotly) {
    g <- plotly::ggplotly(g)
  }

  if (!is.null(palette) & !is.na(palette)) {
    g <- ggpubr::set_palette(g, palette)
  }

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
aba_plot_coef.abaSummary <- function(object,
                                     x = 'predictor',
                                     group = 'predictor_set',
                                     facet = c('outcome', 'group'),
                                     coord_flip = FALSE,
                                     palette = 'jama',
                                     labels = NULL,
                                     plotly = FALSE,
                                     ...) {

  model_type <- NA
  if ('AUC' %in% (object$results %>% pull(term) %>% unique())) {
    model_type <- 'glm'
  } else {
    model_type <- 'lm'
  }

  facet_x <- facet[1]
  facet_y <- facet[2]

  plot_df <- object$results %>%
    rename(predictor = term) %>%
    filter(
      form == 'coef',
      predictor != '(Intercept)'
    ) %>%
    rename(
      'x' = {{ x }},
      'group' = {{ group }},
      'facet_x' = {{ facet_x }},
      'facet_y' = {{ facet_y }}
    )

  model <- object$model
  all_predictors <- model %>% get_predictors()
  all_covariates <- model$spec$covariates
  all_variables <- c(all_covariates, all_predictors)

  g <- ggplot(plot_df,
              aes(x = .data$x,
                  y = .data$estimate,
                  color = .data$group)) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    geom_errorbar(
      aes(ymin = .data$conf_low,
          ymax = .data$conf_high),
      position=position_dodge(0.5), size=0.5,
      width = 0.2
    ) +
    facet_wrap(.data$facet_x ~ .data$facet_y)
    #facet_wrap(.~paste0(.data$facet_x,' | ', .data$facet_y))

  if (model_type == 'glm') {
    g <- g + geom_hline(aes(yintercept=1), linetype='dashed')
  } else if (model == 'lm') {
    g <- g + geom_hline(aes(yintercept=0), linetype='dashed')
  }

  g <- g +
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

  if (coord_flip) g <- g + coord_flip()

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
