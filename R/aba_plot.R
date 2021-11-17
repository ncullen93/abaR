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
aba_plot_metric <- function(model_summary, ...) {
  if (!('abaSummary' %in% class(model_summary))) {
    stop('Input should be an aba summary. Use aba_summary().')
  }

  plot_df <- model_summary$results

  g <-
    ggplot(plot_df, aes(x = .data$PID, y = .data$AUC, color = .data$outcomes)) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    facet_wrap(. ~ .data$groups) +
    geom_hline(aes(yintercept=0.5), linetype='dashed') +
    theme_classic(base_size = 16) +
    ylim(c(0.5, 1)) +
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


aba_plot_coef <- function(model_summary, ...) {
  if (!('abaSummary' %in% class(model_summary))) {
    stop('Input should be an aba summary. Use aba_summary().')
  }

  plot_df <- model_summary$results %>%
    pivot_longer(cols = c(PLASMA_ABETA_NTK:PLASMA_NFL_NTK)) %>%
    filter(!is.na(value)) %>%
    select(PID, groups, outcomes, name, value) %>%
    separate(col = value, into = c('Value','Pvalue'), sep=' ', convert=TRUE)

  g <-
    ggplot(plot_df, aes(x = .data$name, y = .data$Value,
                        color = .data$PID)) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    facet_wrap(.data$outcomes ~ .data$groups) +
    geom_hline(aes(yintercept=0), linetype='dashed') +
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
aba_plot_roc <- function(model, ...) {
  # nest data by distinct group - outcome pairs
  plot_df <- model$results %>%
    group_by(.data$groups, .data$outcomes) %>%
    nest()

  # create a plot for each group - outcome pair
  plot_df <- plot_df %>%
    rowwise() %>%
    mutate(
      plots = plot_roc_single(
        models = .data$data$glm,
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
plot_roc_single <- function(models, group, outcome, data, ...) {
  group.name <- group
  outcome.name <- outcome
  tmp.models <- models

  data <- data %>%
    filter(rlang::eval_tidy(rlang::parse_expr(group.name))) %>%
    data.frame()

  roc.list <- tmp.models %>% purrr::map(
    function(tmp.model) {

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
    g <- ggpubr::set_palette(g.roc, 'jama')
  }
  list(g)
}
