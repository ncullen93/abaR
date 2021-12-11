#' Calculated estimated marginal means.
#'
#' This function estimates the estimated marginal means (also known as
#' least-square means) and, if relevant, the treatment effects for mmrm, lme,
#' and ancova models.
#'
#' This function is based on the `emmeans::emmeans` function. This function will
#' only be run for the stats which are supported by emmeans.
#'
#' @param model abaModel. The fitted aba model to run emmeans on.
#'
#' @return an abaEmmeans object. This object contains the emmeans, the paired
#'   comparisons (i.e., treatment effect), and the sample size at each visit.
#' @export
#'
#' @examples
#'
#' # process data: take first 4 visits, only MCI, use CSF abeta as "treatment",
#' # and create endpoint as change from baseline in cognition at each visit
#' df <- adnimerge %>%
#'   dplyr::filter(
#'     VISCODE %in% c('bl','m06','m12','m24'),
#'     !is.na(CSF_ABETA_STATUS_bl),
#'     DX_bl %in% c('MCI')
#'   ) %>%
#'   dplyr::mutate(
#'     TREATMENT = factor(CSF_ABETA_STATUS_bl, levels=c(0,1),
#'                        labels=c('Placebo','Treatment')),
#'     ADAS13 = ADAS13 - ADAS13_bl,
#'     CDRSB = CDRSB - CDRSB_bl,
#'     MMSE = MMSE - MMSE_bl
#'   )
#'
#' # fit mmrm model for different endpoints, adjusted for covariates
#' model <- df %>% aba_model() %>%
#'   set_outcomes(CDRSB, ADAS13, MMSE) %>%
#'   set_covariates(
#'     AGE, GENDER, EDUCATION
#'   ) %>%
#'   set_stats(
#'     stat_mmrm(id = 'RID', time = 'VISCODE', treatment = 'TREATMENT')
#'   ) %>%
#'   aba_fit()
#'
#' # run emmeans
#' model_emmeans <- model %>% aba_emmeans()
#' print(model_emmeans)
#'
aba_emmeans <- function(model) {

  # count df
  count_df <- model$results %>%
    #filter(predictor != '') %>%
    group_by(group, outcome, stat, predictor_set) %>%
    filter(row_number() == 1L) %>%
    rowwise() %>%
    mutate(
      count_data = list(extract_counts(.data$stat_fit, .data$stat_obj$extra_params))
    ) %>%
    select(
      -c(predictor:stat_fit)
    ) %>%
    unnest(count_data) %>%
    ungroup() %>%
    mutate(treatment = factor(treatment, levels = unique(treatment))) %>%
    arrange(treatment, time)

  r <- model$results %>%
    #filter(predictor != '') %>%
    rowwise() %>%
    mutate(
      stat_emmeans = list(
        run_emmeans(
          fit = .data$stat_fit,
          extra_params = .data$stat_obj$extra_params
        )
      )
    ) %>%
    unnest_wider(stat_emmeans)

  emmeans_df <- r %>%
    select(group:predictor_set, emmeans) %>%
    unnest(emmeans) %>%
    select(-c(df, statistic, std.error)) %>%
    rename(
      conf_low = conf.low,
      conf_high = conf.high
    ) %>%
    mutate(
      treatment = factor(treatment, levels = unique(treatment)),
      time = factor(time, levels=unique(time))
    )

  pairs_df <- r %>%
    select(group:predictor_set, pairs) %>%
    unnest(pairs) %>%
    select(-c(term, null.value, df, statistic, std.error)) %>%
    rename(
      conf_low = conf.low,
      conf_high = conf.high
    ) %>%
    select(group:predictor_set, treatment, time, estimate:pval) %>%
    mutate(
      time = factor(time, levels = unique(time))
    )

  s <- list(
    'emmeans' = emmeans_df,
    'pairs' = pairs_df,
    'counts' = count_df
  )
  class(s) <- 'abaEmmeans'
  return(s)
}

extract_counts <- function(fit, extra_params) {
  treatment <- extra_params$treatment
  time <- extra_params$time

  data <- nlme::getData(fit)
  data_original <- fit$data_original

  count_df <- data  %>%
    group_by(.data[[treatment]], .data[[time]]) %>%
    summarise(
      n = n(),
      .groups = 'keep'
    ) %>%
    ungroup() %>%
    rename(
      treatment = {{ treatment }},
      time = {{ time }}
    )

  count_df0 <- data_original %>%
    filter(.data[[time]] == unique(data_original[[time]])[1]) %>%
    group_by(.data[[treatment]], .data[[time]]) %>%
    summarise(n=n(), .groups='keep') %>%
    ungroup() %>%
    rename(
      treatment = {{ treatment }},
      time = {{ time }}
    ) %>%
    mutate(time = factor(time))

  count_df <- count_df0 %>%
    bind_rows(count_df)

  count_df
}


#' @export
aba_plot.abaEmmeans <- function(object,
                                ...) {
  plot_df <- object$emmeans %>%
    group_by(group, outcome, stat, predictor_set) %>%
    nest() %>%
    select(-data) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      fig = list(plot_emmeans_helper(
        .data$group, .data$outcome, .data$stat, .data$predictor_set,
        object
      ))
    )
  plot_df
}

guess_numeric <- function(x) {
  withr::local_options(.new = list(warn = -1))
  x[tolower(x) %in% c('bl','baseline')] <- 0
  x_num <- readr::parse_number(x)
  x_num[is.na(x_num)] <- 0
  x_num <- as.numeric(x_num)
  x_num
}

plot_emmeans_helper <- function(group, outcome, stat, predictor_set, object) {
  group <- group
  outcome <- outcome
  stat <- stat
  predictor_set <- predictor_set

  df1 <- object$emmeans %>%
    filter(
      .data$group == {{ group }},
      .data$outcome == {{ outcome }},
      .data$stat == {{ stat }},
      .data$predictor_set == {{ predictor_set }}
    )
  df2 <- object$pairs  %>%
    filter(
      group == {{ group }},
      outcome == {{ outcome }},
      stat == {{ stat }},
      predictor_set == {{ predictor_set }}
    )
  df3 <- object$counts %>%
    filter(
      group == {{ group }},
      outcome == {{ outcome }},
      stat == {{ stat }},
      predictor_set == {{ predictor_set }}
    )
  ######

  # add zeros for the first time point
  df1 <- df1 %>%
    group_by(group, outcome, stat, predictor_set) %>%
    nest() %>%
    rowwise() %>%
    mutate(
      data = list(
        tibble::tibble(
          treatment = factor(unique(df1$treatment)),
          time = factor(0),
          estimate = 0,
          conf_low = 0,
          conf_high = 0,
          pval = 0
        ) %>%
          bind_rows(
            .data$data
          )
      )
    ) %>%
    unnest(data) %>%
    ungroup() %>%
    mutate(
      time = guess_numeric(as.character(time))
    )

  # add y value height to p-value table
  df2 <- df2 %>%
    mutate(time=guess_numeric(as.character(time))) %>%
    left_join(
      df1 %>%
        filter(time!=0) %>%
        group_by(group, outcome, stat, predictor_set, time) %>%
        summarise(yval = max(conf_high), .groups='keep') %>%
        ungroup(),
      by = c('group','outcome','stat','predictor_set','time')
    )

  dodge_width <- max(df1$time) / 40

  is_neg <- df1[nrow(df1),][['estimate']] < 0
  legend_y <- ifelse(is_neg, 0.1, 0.95)

  nudge_y <- max(abs(df1$estimate)) / 15

  g <- df1 %>%
    ggplot(aes(x=time, y=estimate, group=treatment, color=treatment)) +
    geom_hline(yintercept=0, size=0.5, color='gray', linetype='solid')+
    geom_line(size=1, position=position_dodge(width=dodge_width)) +
    geom_point(aes(shape=treatment), position=position_dodge(width=dodge_width), size=3) +
    geom_errorbar(aes(ymin=conf_low, ymax=conf_high), width=1*dodge_width,
                  size=1, position=position_dodge(width=dodge_width)) +
    geom_text(
      data=df2,
      aes(x=time, y=yval, label=paste0('P=', sprintf('%.4f',pval))),
      size=5, color='black', show.legend = FALSE, nudge_y = nudge_y
    ) +
    scale_x_continuous(breaks=unique(df1$time),
                       expand = c(0.03, 0.03, 0.1, 0))

  if (length(unique(df1$group)) == 1) {
    g <- g + facet_wrap(outcome ~ ., scales = 'free')
  } else {
    g <- g + facet_wrap(group ~ outcome, scales = 'free')
  }

  g <- g +
    xlab('Time from baseline') +
    ylab('Adjusted mean change\nfrom baseline (±SE)') +
    theme_classic(base_size = 18) +
    theme(legend.position=c(0.05, legend_y),
          legend.justification = c(0.05,legend_y),
          legend.title = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 22, vjust = 1.25)) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank())

  g <- ggpubr::set_palette(g, 'jama')

  df3 <- df3 %>%
    mutate(time = guess_numeric(as.character(time)))

  g_count <- df3 %>%
    ggplot(aes(x=time, y=rev(treatment),
               label=n, color=treatment)) +
    geom_text(size = 5.5, show.legend = FALSE) +
    scale_x_continuous(breaks=unique(df3$time),
                       expand = c(0.045, 0.05, 0.115, 0))+
    facet_wrap(group ~ outcome, scales = 'free') +
    theme_void(base_size = 12) +
    theme(
      panel.spacing = unit(1.5, "lines"),
      legend.title = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      plot.margin = margin(-8,0,3,0),
      plot.title = element_text(size = 16)
    ) +
    geom_hline(yintercept=2.55, linetype='solid', color='black')
  g_count <- g_count %>% ggpubr::set_palette('jama')

  g2 <- ggpubr::ggarrange(
    g, g_count + ggtitle('Sample size'), ncol=1, align = 'v',
    heights = c(0.85, 0.15),
    label.y = 1.1, label.x = 0, vjust=0, hjust=-0.8
  )

  g2
}


#print.abaEmmeans <- function(object,
#                             ...) {
#  print(object)
#}