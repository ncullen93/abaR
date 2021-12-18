#' Plot an aba object
#'
#' This is a generic function for plotting an aba object. The resulting plot will
#' depend on the type of aba object. The supported objects are the following:
#'   - aba_emmeans
#'   - aba_longpower
#'   - aba_robust
#'
#' @param object aba object. The object to plot.
#' @param ... additional parameters
#'
#' @return a ggplot with plotted results depending on the aba object.
#'
#' @export
aba_plot <- function(object, ...) {
  UseMethod('aba_plot')
}

#' Plot metrics of an aba model summary
#'
#' @param object an aba model summary. The object to plot - this should be the
#'   result of an `aba_summary()` call.
#' @param metric string. The metric to plot.
#' @param axis string. Specifies the x axis variable, color/fill variable, and
#'   facet variable in that order. Should be a vector of length three that
#'   includes only "predictor", "outcome", and "group" as values.
#' @param coord_flip logical. Whether to flip the x and y axes. This can be
#'   useful when there are a large amount of predictor sets and you want to
#'   view metrics vertically.
#' @param include_basic logical. Whether to include basic predictor.
#' @param sort logical. Whether to sort axis labels by metric value
#' @param palette string. Which ggpubr palette to use. See `ggpubr::set_palette`.
#' @param plotly logical. Whether to use plot.ly instead of standard ggplot.
#'   Defaults to false. Using ggplotly can be useful if you want interactivity
#'   on web pages.
#'
#' @return a ggplot of the specified aba model summary metric.
#' @export
#'
#' @examples
#'
#' # fit aba model
#' model <- aba_model() %>%
#'   set_data(adnimerge %>% dplyr::filter(VISCODE == 'bl')) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm') %>%
#'   fit()
#'
#' # summarise aba model to calculate metrics
#' model_summary <- model %>% aba_summary()
#'
#' # plot the metrics using default (defaults to AUC)
#' metric_plot <- model_summary %>% aba_plot_metric()
#'
#' # coord flip
#' metric_plot2 <- model_summary %>% aba_plot_metric(coord_flip=TRUE)
#'
#' # compare predictor sets within each outcome instead of the opposite
#' metric_plot3 <- model_summary %>%
#'   aba_plot_metric(x = 'outcome', group='predictor')
#'
aba_plot_metric <- function(object,
                            metric = NULL,
                            axis = c('predictor', 'outcome', 'group'),
                            coord_flip = FALSE,
                            include_basic = TRUE,
                            sort = FALSE,
                            palette = 'jama',
                            plotly = FALSE) {

  # find main metric - directly after predictors
  metric <- object$results$metrics$term[1]
  plot_df <- object$results$metrics %>% filter(term == metric)
  if (!include_basic) plot_df <- plot_df %>% filter(predictor != 'Basic')

  if (sum(is.na(match(c('predictor', 'outcome', 'group'), axis))) > 0) {
    stop('axis should contain "predictor", "outcome", and "group"')
  }

  x <- axis[1]
  fill <- axis[2]
  facet <- axis[3]

  # auto change 'x' and 'group' if there is a big difference because
  # it is probably what the user wants
  if (n_distinct(plot_df$outcome) > 5*n_distinct(plot_df$predictor)) {
    if (x != 'outcome') {
      warning('Auto exchanging x axis variable and fill variable because
            of large difference between n_distinct(outcome) versus
            n_distinct(predictor). Specify "outcome" as first entry in
            "axis" arg to avoid this warning.')
      x <- 'outcome'
      fill <- 'predictor'
      facet <- 'group'
    }
  }

  plot_df <- plot_df %>%
    rename(
      'x' = {{ x }},
      'fill' = {{ fill }},
      'facet' = {{ facet }}
    )

  # this doesnt take into account groups
  if (sort) {
    plot_df <- plot_df %>% mutate(x = forcats::fct_reorder(x, estimate))
  }

  n_groups <- dplyr::n_distinct(object$results$metrics[[fill]])

  g <- ggplot(plot_df,
              aes(x = .data$x,
                  y = .data$estimate,
                  color = .data$fill)) +
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

  legend_position <- 'top'
  if (n_groups > 6) legend_position <- 'none'
  g <- g +
    theme_classic(base_size = 16) +
    theme(
      legend.position = legend_position,
      legend.margin = margin(5, 0, 0, 0),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 16, vjust = 1.25),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = "black",
        size = 0.2, linetype = "dotted")
    )

  if (coord_flip) {
    g <- g + coord_flip() + theme(axis.title.y = element_blank())
  } else {
    g <- g + theme(axis.title.x = element_blank())
  }

  if (!is.null(palette) & (n_groups <= 6)) {
    g <- ggpubr::set_palette(g, palette)
  }

  if (plotly) {
    g <- plotly::ggplotly(g)
  }

  return(g)
}


#' Plot coefficients of an aba model summary
#'
#' @param object an aba model summary. The object to plot - this should be the
#'   result of an `aba_summary()` call.
#' @param axis string. Specifies the x axis variable, color/fill variable, and
#'   facet variable in that order. Should be a vector of length three that
#'   includes only "predictor", "outcome", and "group" as values.
#' @param coord_flip logical. Whether to flip the x and y axes. This can be
#'   useful when there are a large amount of predictor sets and you want to
#'   view metrics vertically.
#' @param include_covariates logical. Whether to include covariates
#' @param sort logical. Whether to sort axis labels by coefficient value
#' @param palette string. Which ggpubr palette to use. See `ggpubr::set_palette`.
#' @param plotly logical. Whether to use plot.ly instead of standard ggplot.
#'   Defaults to false. Using ggplotly can be useful if you want interactivity
#'   on web pages.
#'
#' @return a ggplot of the specified aba model summary coefficients
#' @export
#'
#' @examples
#'
#' # fit aba model
#' model <- aba_model() %>%
#'   set_data(adnimerge %>% dplyr::filter(VISCODE == 'bl')) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats(stat_glm(std.beta=TRUE)) %>%
#'   fit()
#'
#' # summarise aba model to calculate metrics
#' model_summary <- model %>% aba_summary()
#'
#' # plot the coefficients using default
#' coef_plot <- model_summary %>% aba_plot_coef(coord_flip=TRUE)
#'
#' # compare predictor coefficients across outcomes
#' coef_plot2 <- model_summary %>%
#'   aba_plot_coef(
#'     x = 'outcome', group='predictor',
#'     facet=c('term','group'), coord_flip=TRUE
#'   )
#'
aba_plot_coef <- function(object,
                          axis = c('term', 'predictor', 'outcome', 'group'),
                          coord_flip = FALSE,
                          include_covariates = TRUE,
                          sort = FALSE,
                          palette = 'jama',
                          plotly = FALSE) {

  model <- object$model
  all_preds <- model$predictors %>% unlist() %>% unique()
  all_covars <- model$covariates
  all_vars <- c(all_covars, all_preds)

  if (sum(is.na(match(c('term', 'predictor', 'outcome', 'group'), axis))) > 0) {
    stop('axis should contain "term", "predictor", "outcome", and "group"')
  }

  x <- axis[1]
  fill <- axis[2]
  facet_x <- axis[3]
  facet_y <- axis[4]

  plot_df <- object$results$coefs %>% filter(predictor != '(Intercept)')
  if (!include_covariates) plot_df <- plot_df %>% filter(!term %in% all_covars)

  plot_df <- plot_df %>%
    rename(
      'x' = {{ x }},
      'fill' = {{ fill }},
      'facet_x' = {{ facet_x }},
      'facet_y' = {{ facet_y }}
    )

  if (nrow(plot_df) == 0) stop('There are no predictors to plot.')
  n_groups <- dplyr::n_distinct(plot_df[[fill]])

  # this doesnt take into account groups
  if (sort) plot_df <- plot_df %>% mutate(x = forcats::fct_reorder(x, estimate))

  g <- ggplot(plot_df,
              aes(x = .data$x,
                  y = .data$estimate,
                  color = .data$fill)) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    geom_errorbar(
      aes(ymin = .data$conf_low,
          ymax = .data$conf_high),
      position=position_dodge(0.5), size=0.5,
      width = 0.2
    ) +
    facet_wrap(.~paste0(.data$facet_x,' | ', .data$facet_y)) +
    ylab('Model coefficient') +
    theme_classic(base_size = 16) +
    theme(
      legend.position = ifelse(n_groups > 6, 'none', 'top'),
      legend.margin = margin(5, 0, 0, 0),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 16, vjust = 1.25),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = "black",
        size = 0.2, linetype = "dotted")
    )

  if (coord_flip) {
    g <- g + coord_flip() + theme(axis.title.y = element_blank())
  } else {
    g <- g + theme(axis.title.x = element_blank())
  }

  if (!is.null(palette) & (n_groups <= 6)) g <- ggpubr::set_palette(g, palette)
  if (plotly) g <- plotly::ggplotly(g)

  return(g)
}


#' Custom aba ggplot2 theme
#'
#' @param base_size integer. base font size
#' @param legend.position string. where to place legend ('none' = no legend)
#' @param coord_flip logical. whether to flip x and y axes
#' @param legend_title logical. whether to include legend title.
#'
#' @return ggplot2 theme which can be added to a ggplot object
#' @export
#'
#' @examples
#' data <- data.frame(x=1:10, y=1:10)
#' fig <- ggplot(data, aes(x=x,y=y)) + geom_point() + theme_aba2()
theme_aba <- function(base_size = 16,
                      legend.position = 'top',
                      coord_flip = FALSE,
                      legend_title = FALSE,
                      family = c('Verdana', 'Tahoma', 'Helvetica')) {
  family <- match.arg(family)

  t <- theme_classic(base_size = base_size) %+replace%
    theme(
      text = element_text(size=base_size, family=family, color='black'),
      legend.position = legend.position,
      legend.margin = margin(5, 0, 0, 0),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", color='black',
                                size = base_size, vjust = 1.25),
      plot.title = element_text(hjust = 0.5),
      axis.line.x.bottom=element_line(size=1.2),
      axis.ticks.y = element_line(size=1.2),
      axis.ticks.length.y = unit(0.2,"cm"),
      axis.ticks.x = element_line(size=1.2),
      axis.ticks.length.x = unit(0.2,"cm"),
      axis.line.y.left=element_line(size=1.2),
      axis.text.y = element_text(
        color='black', margin = margin(t = 0, r = 3, b = 0, l = 6)
      ),
      axis.text.x = element_text(
        color='black', margin = margin(t = 3, r = 0, b = 6, l = 0)
      )
    )

  if (!legend_title) t <- t %+replace% theme(legend.title = element_blank())

  if (coord_flip) {
    t <- t %+replace%
      theme(axis.title.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(colour = "gray", size = 0.2,
                                              linetype = "solid"))
  } else {
    t <- t %+replace%
      theme(axis.title.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "gray", size = 0.2,
                                              linetype = "solid"))
  }
}

