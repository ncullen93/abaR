

#' Create a mmrm stat to use for an aba model.
#'
#' @param id string. id variable
#' @param time string. time variable
#'   (should be discrete visits common to all participants)
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
#' my_stat <- aba_mmrm(id='SUBJECT_ID',
#'                     time='Years_bl')
#'
#' #my_formula <- my_stat$formula_fn(
#' #  outcome='ConvertedToAlzheimers',
#' #  predictors=c('PLASMA_PTAU181_bl','PLASMA_NFL_bl'),
#' #  covariates=c('AGE_bl','GENDER','EDUCAT'),
#' #  id
#' #)
#'
#' #my_model <- my_stat$fit_fn(
#' #  formula = my_formula,
#' #  data = adni_sample
#' #)
aba_mmrm <- function(id,
                     time,
                     treatment = NULL,
                     std.beta = FALSE,
                     complete.cases = TRUE) {
  fns <- list(
    'formula_fn' = aba_formula_lme,
    'fit_fn' = aba_fit_mmrm,
    'treatment' = treatment,
    'extra_params' = list(
      'id' = id,
      'time' = time
    ),
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    )
  )
  fns$stat_type <- 'mmrm'
  class(fns) <- 'abaStat'

  return(fns)
}

# fit a mmrm model
aba_fit_mmrm <- function(formula, data, extra_params) {
  time <- extra_params$time
  id <- extra_params$id
  correlation_form <- glue::glue('~ 1 | {id}')
  weights_form <- glue::glue('~ 1 | {time}')

  # make sure data is in the right format:
  # - time variable should be a factor
  # - first visit should be removed
  unique_visits <- levels(factor(data[[time]]))
  if (length(unique_visits) > 10) {
    stop('10+ unique visits detected... MMRM requires discrete time points!
         Did you accidently use a continuous time variable?')
  }
  first_visit <- unique_visits[1]

  # remove the first visit from the data
  data <- data %>% distinct() %>%
    filter(.data[[time]] != first_visit) %>%
    mutate({{ time }} := factor(.data[[time]]))

  # fit the model
  model <- nlme::gls(
    stats::formula(formula),
    correlation = nlme::corSymm(form = stats::formula(correlation_form)),
    weights = nlme::varIdent(form = stats::formula(weights_form)),
    data = data,
    na.action = na.omit,
    method = 'REML'
  )

  model$call$model <- stats::formula(formula)
  model$call$correlation$form <- stats::formula(correlation_form)
  model$call$weights$form <- stats::formula(weights_form)
  model$call$data <- data

  return(model)
}


aba_tidy.gls <- function(model, predictors, covariates, ...) {

  time_var <- strsplit(as.character(model$call$weights)[2], ' | ')[[1]][3]
  tidy_df <- broom.mixed::tidy(model, conf.int=T) %>%
    select(-c('conf.low', 'conf.high')) %>%
    filter(
      !(.data$term %in% predictors)
    ) %>%
    filter(
      !startsWith(.data$term, time_var) | grepl('\\:', .data$term)
    )
  return(tidy_df)
}

#' @export
aba_glance.gls <- function(x, ...) {
  glance_df <- broom.mixed::glance(x) %>% select(-logLik)
  return(glance_df)
}

#' @export
aba_emmeans.gls <- function(fit, treatment, stats_obj, ...) {
  time <- stats_obj$extra_params$time
  id <- stats_obj$extra_params$id

  emmeans_formula <- formula(glue('~ {treatment} | {time}'))

  emmeans_result <- emmeans::emmeans(fit, emmeans_formula)
  pairs_result <- pairs(emmeans_result)
  return(
    list(
      'emmeans' = emmeans_result %>% broom::tidy(),
      'pairs' = pairs_result %>% broom::tidy()
    )
  )
}

plot_mmrm <- function(model, data, ...) {
  em_res <- emmeans::emmeans(model, specs = ~TREATMENT | VISIT)
  em_test <- emmeans::contrast(em_res, method = 'pairwise')

  em_res_df <- em_res %>% broom::tidy(conf.int=TRUE)
  em_res_df <- em_res_df %>%
    rbind(
      data.frame(
        'TREATMENT' = unique(em_res_df$TREATMENT),
        'VISIT' = 0, 'estimate' = 0, 'std.error' = 0, 'df' = 0,
        'conf.low' = 0, 'conf.high' = 0, 'statistic' = 0, 'p.value' = 0
      )
    ) %>%
    arrange(.data$VISIT) %>%
    mutate(
      TREATMENT = factor(.data$TREATMENT)
    )
  em_test_df <- em_test %>% broom::tidy(conf.int=TRUE)


  count_df <- data  %>%
    group_by(.data$TREATMENT, .data$VISIT) %>% summarise(n=n()) %>%
    mutate(
      #n = ifelse(VISIT == 0, paste0('n = ', n), paste0(n)),
      VISIT=factor(.data$VISIT)
    )

  g_count <- count_df %>%
    ggplot(aes(x=as.numeric(as.character(.data$VISIT)), y=rev(.data$TREATMENT),
               label=.data$n, color=.data$TREATMENT)) +
    geom_text(size = 6, show.legend = FALSE) +
    theme_void(base_size = 20) +
    scale_x_continuous(breaks=as.numeric(as.character(unique(count_df$VISIT))),
                       labels = unique(count_df$VISIT),
                       expand = c(0.04, 0.04)) +
    theme(
      panel.spacing = unit(1.5, "lines"),
      legend.title = element_blank(),
      strip.background = element_blank(),
      plot.margin = margin(0,0,5,0),
      panel.border = element_rect(colour = "black", fill=NA, size=1, linetype='dashed')
    )
  g_count <- g_count %>% ggpubr::set_palette('jama')

  g <- em_res_df %>%
    ggplot(aes(x=as.numeric(.data$VISIT), y=.data$estimate,
               group=.data$TREATMENT, color=.data$TREATMENT)) +
    geom_hline(yintercept=0, size=0.5, color='gray', linetype='solid')+
    geom_line(position=position_dodge(0.05), size=1) +
    geom_point(position=position_dodge(0.05), aes(shape=.data$TREATMENT),size=3) +
    geom_errorbar(aes(ymin=.data$conf.low, ymax=.data$conf.high), width=0.05, size=1,
                  position=position_dodge(0.05)) +
    xlab('Visit') +
    ylab('LS means (SE)') +
    scale_x_continuous(breaks=as.numeric(unique(em_res_df$VISIT)),
                       labels = unique(em_res_df$VISIT),
                       expand = c(0.03, 0.03)) +
    theme_classic(base_size = 20) +
    theme(legend.position=c(0.15, 0.9),
          legend.title = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 22, vjust = 1.25)) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank())

  g <- g %>% ggpubr::set_palette('jama')


  g0 <- ggpubr::ggarrange(
    g, g_count, ncol=1, align = 'v', heights = c(0.88, 0.12),
    labels=c('','Sample size'), label.y = 1.1, label.x = 0, vjust=0, hjust=-0.9
  )


}



