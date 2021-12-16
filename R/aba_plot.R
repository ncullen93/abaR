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
#' @param metric string. The performance metric to plot (e.g., `AIC`, `AUC`,
#'   `adj.r.squared`)
#' @param x string. The model spec factor to use as the x axis. Defaults to
#'   predictor sets.
#' @param group string. The model spec factor to use as the group variable in
#'   ggplot - this corresponding to "group", "fill", and "color" in ggplot.
#'   Defaults to outcome.
#' @param facet string. The model spec factor to use as the group variable in
#'   ggplot - this corresponding to "facet_wrap" in ggplot. Defaults to group.
#' @param coord_flip logical. Whether to flip the x and y axes. This can be
#'   useful when there are a large amount of predictor sets and you want to
#'   view metrics vertically.
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
                            x = 'predictor',
                            group = 'outcome',
                            facet = 'group',
                            coord_flip = FALSE,
                            palette = 'jama',
                            plotly = FALSE) {

  # find main metric - directly after predictors
  metric <- object$results$metrics$term[1]

  plot_df <- object$results$metrics %>%
    filter(term == metric) %>%
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


#' Plot coefficients of an aba model summary
#'
#' @param object an aba model summary. The object to plot - this should be the
#'   result of an `aba_summary()` call.
#' @param x string. The model spec factor to use as the x axis. Defaults to
#'   predictor sets.
#' @param group string. The model spec factor to use as the group variable in
#'   ggplot - this corresponding to "group", "fill", and "color" in ggplot.
#'   Defaults to outcome.
#' @param facet string. The model spec factor to use as the group variable in
#'   ggplot - this corresponding to "facet_wrap" in ggplot. Defaults to group.
#' @param coord_flip logical. Whether to flip the x and y axes. This can be
#'   useful when there are a large amount of predictor sets and you want to
#'   view metrics vertically.
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
                          x = 'term',
                          group = 'predictor',
                          facet = c('outcome', 'group'),
                          coord_flip = FALSE,
                          palette = 'jama',
                          plotly = FALSE) {

  model_type <- object$model$stats[[1]]$stat_type

  model <- object$model
  all_predictors <- model$predictors %>% unlist() %>% unique()
  all_covariates <- model$covariates
  all_variables <- c(all_covariates, all_predictors)

  facet_x <- facet[1]
  facet_y <- facet[2]

  plot_df <- object$results$coefs %>%
    filter(
      predictor != '(Intercept)',
      !(predictor %in% all_covariates)
    ) %>%
    rename(
      'x' = {{ x }},
      'group' = {{ group }},
      'facet_x' = {{ facet_x }},
      'facet_y' = {{ facet_y }}
    )

  if (nrow(plot_df) == 0) stop('There are no predictors to plot.')

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
  } else if (model_type == 'lm') {
    g <- g + geom_hline(aes(yintercept=0), linetype='dashed')
  }

  g <- g +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top", legend.margin = margin(5, 0, 0, 0),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 14, vjust = 1.25),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank()
    )


  if (coord_flip) {
    g <- g +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(
          colour = "black",
          size = 0.2, linetype = "dotted"),
        axis.title.x = element_blank()
      )
  } else {
    g <- g +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(
          colour = "black",
          size = 0.2, linetype = "dotted"),
        axis.title.x = element_blank()
      )
  }

  if (!is.null(palette) & !is.na(palette)) {
    g <- ggpubr::set_palette(g, palette)
  }

  return(g)
}
