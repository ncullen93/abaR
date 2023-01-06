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
#' @param model_summary an aba model summary. The object to plot - this should be the
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
#' @param facet_labels logical. Whether to include facet labels or not.
#' @param palette string. Which ggpubr palette to use. See `ggpubr::set_palette`.
#' @param plotly logical. Whether to use plot.ly instead of standard ggplot.
#'   Defaults to false. Using ggplotly can be useful if you want interactivity
#'   on web pages.
#'
#' @return a ggplot of the specified aba model summary metric.
#' @export
#'
#' @examples
#' # fit aba model
#' model <- aba_model() %>%
#'   set_data(adnimerge %>% dplyr::filter(VISCODE == 'bl')) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl,
#'                .labels=c('Conversion to AD', 'CSF Abeta Status')) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl),
#'     .labels = c('A','T','N','ATN')
#'   ) %>%
#'   set_stats(stat_glm(std.beta=TRUE)) %>%
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
#'   aba_plot_metric(axis = c('outcome','predictor', 'group'))
aba_plot_metric <- function(model_summary,
                            metric = NULL,
                            axis = c('predictor', 'outcome', 'group'),
                            coord_flip = FALSE,
                            include_basic = TRUE,
                            sort = FALSE,
                            facet_labels = TRUE,
                            palette = 'jama',
                            plotly = FALSE) {
  object <- model_summary

  # if no covariates, then dont include basic
  if (length(object$model$covariates) == 0) include_basic <- FALSE

  # find main metric - directly after predictors
  metric <- object$results$metrics$term[1]
  plot_df <- object$results$metrics %>% filter(term == metric) %>%
    mutate(
      predictor = factor(predictor,
                         levels=unique(object$results$metrics$predictor))
    )
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

  g <- plot_df %>%
    ggplot(aes(x = .data$x, y = .data$estimate, color = .data$fill)) +
    geom_errorbar(
      aes(ymin = .data$conf_low, ymax = .data$conf_high),
      position=position_dodge(0.5), size=1, width = 0.2
    ) +
    geom_point(position = position_dodge(0.5), size = 4) +
    ylab(toupper(metric)) +
    theme_aba(
      legend.position = ifelse(n_groups > 6, 'none', 'top'),
      coord_flip = coord_flip,
      facet_labels = facet_labels
    )

  # add facet only if facet variable has more than 1 unique value
  if (length(unique(plot_df$facet)) > 1) {
    g <- g + facet_wrap(. ~ .data$facet)
  }

  if (metric == 'auc') {
    g <- g + ylim(c(min(0.5, plot_df$conf_low), 1))
    g <- g + geom_hline(aes(yintercept=0.5), linetype='dashed')
  }

  if (coord_flip) g <- g + coord_flip()
  if (!is.null(palette) & (n_groups <= 6)) g <- ggpubr::set_palette(g, palette)
  if (plotly) g <- plotly::ggplotly(g)

  return(g)
}


#' Plot coefficients of an aba model summary
#'
#' @param model_summary an aba model summary. The object to plot - this should be the
#'   result of an `aba_summary()` call.
#' @param term_labels list. A list where name is equal to a term
#'   variable and value is equal to the label you want to replace it with in
#'   the plot. Useful to exchange variable names with labels. Rememeber that
#'   terms are the data variables/columns which make up predictors.
#' @param axis string. Specifies the x axis variable, color/fill variable, and
#'   facet variable in that order. Should be a vector of length three that
#'   includes only "predictor", "outcome", and "group" as values.
#' @param coord_flip logical. Whether to flip the x and y axes. This can be
#'   useful when there are a large amount of predictor sets and you want to
#'   view metrics vertically.
#' @param include_covariates logical. Whether to include covariates
#' @param sort logical. Whether to sort axis labels by coefficient value
#' @param facet_labels logical. Whether to include facet labels.
#' @param facet_axis logical. Whether to keep axis segment/labels for all
#'   facets or whether to remove them for facets.
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
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl,
#'                .labels=c('Conversion to AD', 'CSF Abeta Status')) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl),
#'     .labels = c('A','T','N','ATN')
#'   ) %>%
#'   set_stats(stat_glm(std.beta=TRUE)) %>%
#'   fit()
#'
#' # summarise aba model to calculate metrics
#' model_summary <- model %>% aba_summary()
#'
#' # plot the coefficients using default
#' coef_plot <- model_summary %>% aba_plot_coef(coord_flip = TRUE)
#'
#' # add term labels
#' term_labels <- list(
#'   'PLASMA_ABETA_bl' = 'Plasma Abeta',
#'   'PLASMA_PTAU181_bl' = 'Plasma P-tau',
#'   'PLASMA_NFL_bl' = 'Plasma NfL'
#' )
#' coef_plot2 <- model_summary %>% aba_plot_coef(coord_flip = TRUE,
#'                                               term_labels = term_labels)
#'
#' # compare predictor coefficients across outcomes
#' coef_plot3 <- model_summary %>%
#'   aba_plot_coef(
#'     axis = c('outcome', 'predictor','term','group'), coord_flip=TRUE
#'   )
aba_plot_coef <- function(model_summary,
                          term_labels = NULL,
                          axis = c('term', 'predictor', 'outcome', 'group'),
                          coord_flip = FALSE,
                          include_covariates = TRUE,
                          sort = FALSE,
                          facet_labels = TRUE,
                          facet_axis = TRUE,
                          palette = c('jama', 'nature', 'lancet', 'none'),
                          plotly = FALSE) {
  object <- model_summary
  palette <- match.arg(palette)
  if (palette == 'nature') palette <- 'npg'

  if (sum(is.na(match(c('term', 'predictor', 'outcome', 'group'), axis))) > 0) {
    stop('axis should contain "term", "predictor", "outcome", and "group"')
  }

  model <- object$model
  all_covars <- model$covariates
  plot_df <- object$results$coefs %>% filter(predictor != '(Intercept)')
  plot_df <- plot_df %>%
    mutate(
      predictor = factor(predictor, levels=unique(plot_df$predictor))
    )

  if (!include_covariates) plot_df <- plot_df %>% filter(!term %in% all_covars)

  if (!is.null(term_labels)) {
    if (!is.list(term_labels)) stop('term_labels should be a list.')
    plot_df <- plot_df %>%
      mutate(
        term = map_chr(
          term, ~ifelse(. %in% names(term_labels), term_labels[[.]], .)
        )
      )
  }
  plot_df <- plot_df %>%
    mutate(term = factor(term, levels=unique(plot_df$term)))


  x <- axis[1]
  fill <- axis[2]
  facet_x <- axis[3]
  facet_y <- axis[4]

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
  if (sort) {
    plot_df <- plot_df %>% mutate(x = forcats::fct_reorder(x, estimate))
  }

  scales <- ifelse(facet_axis, 'free', 'fixed')

  g <- plot_df %>%
    ggplot(aes(x = .data$x, y = .data$estimate, color = .data$fill)) +
    geom_errorbar(
      aes(ymin = .data$conf_low, ymax = .data$conf_high),
      position=position_dodge(0.5), size=0.5, width = 0.2
    ) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    scale_y_continuous(limits=c(min(plot_df$conf_low), max(plot_df$conf_high)))+
    ylab('Coefficient') +
    theme_aba(
      legend.position = ifelse(n_groups > 6, 'none', 'top'),
      coord_flip = coord_flip,
      facet_labels = facet_labels
    )

  # add facets only if facet variables have more than 1 unique value
  if (length(unique(plot_df$facet_x)) > 1) {
    if (length(unique(plot_df$facet_y)) > 1) {
      g <- g + facet_wrap(~paste0(.data$facet_x,' | ', .data$facet_y), scales = scales)
    } else {
      g <- g + facet_wrap(~.data$facet_x, scales = scales)
    }
  } else {
    if (length(unique(plot_df$facet_y)) > 1) {
      g <- g + facet_wrap(~.data$facet_y, scales = scales)
    }
  }

  if (coord_flip) g <- g + coord_flip()
  if ((palette != 'none') & (n_groups <= 6)) g <- ggpubr::set_palette(g, palette)
  if (plotly) g <- plotly::ggplotly(g)

  return(g)
}


#' Custom aba ggplot2 theme
#'
#' @param base_size integer. base font size
#' @param legend.position string. where to place legend ('none' = no legend)
#' @param coord_flip logical. whether to flip x and y axes
#' @param legend_title logical. whether to include legend title.
#' @param facet_labels logical. Whether to include labels of facets.
#' @param axis_title logical. Whether to include axis title for non-primary axis
#' @param family string. which font family to use. Should be one of
#'   Verdana, Tahoma, Helvetica
#'
#' @return ggplot2 theme which can be added to a ggplot object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' data <- data.frame(x=1:10, y=1:10)
#' fig <- ggplot(data, aes(x=x,y=y)) + geom_point() + theme_aba()
theme_aba <- function(base_size = 16,
                      legend.position = 'top',
                      coord_flip = FALSE,
                      legend_title = FALSE,
                      facet_labels = TRUE,
                      axis_title = FALSE,
                      family = c('Helvetica', 'Tahoma', 'Verdana')) {

  family <- match.arg(family)

  t <- theme_classic(base_size = base_size) %+replace%
    theme(
      text = element_text(size=base_size, family=family, color='black'),
      legend.position = legend.position,
      legend.margin = margin(5, 0, 0, 0),
      plot.margin = unit(c(0.5, 1.5, 0.75, 0.5), "lines"),
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(color='black',
                                size = base_size, vjust = 0,
                                margin = margin(b=10)),
      plot.title = element_text(hjust = 0.5),
      axis.line.x.bottom = element_line(linewidth=1.2),
      axis.ticks.y = element_line(linewidth=1.2),
      axis.ticks.length.y = unit(0.2,"cm"),
      axis.ticks.x = element_line(linewidth=1.2),
      axis.ticks.length.x = unit(0.2,"cm"),
      axis.line.y.left=element_line(linewidth=1.2),
      axis.text.y = element_text(
        color='black', margin = margin(t = 0, r = 3, b = 0, l = 0),
        hjust=1
      ),
      axis.text.x = element_text(
        color='black', margin = margin(t = 4, r = 0, b = 6, l = 0)
      )
    )

  if (!facet_labels) {
    t <- t %+replace% theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  }

  if (!legend_title) t <- t %+replace% theme(legend.title = element_blank())

  if (coord_flip) {
    t <- t %+replace%
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(colour = "gray",
                                              linewidth = 0.2,
                                              linetype = "solid"))
    if (!axis_title) t <- t %+replace% theme(axis.title.y = element_blank())
  } else {
    t <- t %+replace%
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "gray",
                                              linewidth = 0.2,
                                              linetype = "solid"))
    if (!axis_title) t <- t %+replace% theme(axis.title.x = element_blank())
  }
  t
}

