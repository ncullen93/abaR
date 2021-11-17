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
  plot_df <- model$results %>%
    select(groups, outcomes) %>%
    distinct()

}

plot_roc_single <- function(model, group, outcome, ...) {
  group.name <- group
  outcome.name <- outcome

  tmp.models <- model$results %>%
    filter(groups == group.name, outcomes == outcome.name) %>%
    pull('glm')

  data <- model$data %>% filter(rlang::eval_tidy(rlang::parse_expr(group.name)))
  roc.list <- tmp.models %>% purrr::map(
    function(tmp.model) {
      outcome.var <- as.character(tmp.model$formula)[2]

      data[,'tmp.outcome'] <- data[,outcome.var]
      data$tmp.outcome.pred <- stats::predict(tmp.model, type = "response")
      res <- pROC::roc(tmp.outcome ~ tmp.outcome.pred,
                       quiet = T, ci=T, data=data, percent=T)
      return(res)
    }
  )

  g.roc <- pROC::ggroc(roc.list, size=0.8) +
    geom_segment(aes(x = 100, xend = 0, y = 0, yend = 100),
                 alpha=0.1, color="grey", linetype="solid") +
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
  g.roc
}
