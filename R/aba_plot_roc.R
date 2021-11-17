#' Create ROC curves from an aba model
#'
#' @param model abaModel. Model fit to plot.
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
      roc_plot = plot_roc_single(
        models = .data$data$glm,
        group = .data$groups,
        outcome = .data$outcomes,
        data = model$data
      )
    )

  plot <- ggpubr::ggarrange(plotlist = plot_df$roc_plot,
                            common.legend = TRUE)

  return(plot)
}

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

  g.roc <- pROC::ggroc(roc.list, size=0.8) +
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
    g.roc <- ggpubr::set_palette(g.roc, 'jama')
  }
  list(g.roc)
}
