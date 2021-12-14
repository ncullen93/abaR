#' @title Automated biomarker analysis in R
#' @name aba
#'
#' @importFrom stats AIC coef confint logLik na.omit sd
#' @importFrom glue glue
#' @importFrom rlang :=
#'
#' @import dplyr tidyr purrr ggplot2
#'
#' @keywords internal
"_PACKAGE"
#' @importFrom rlang .data
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL


utils::globalVariables(
  names = c(
    'group', 'outcome', 'stat', 'predictor',
    'count_data', 'treatment', 'time', 'stat_emmeans',
    'emmeans', 'df', 'statistic', 'std.error', 'conf.low', 'conf.high',
    'pairs', 'term', 'null.value', 'estimate', 'pval',
    '.Included', '.Predicted', '.Truth', 'coef_summary', 'coefs',
    'conf.high', 'conf.lo', 'conf_high', 'conf_low', 'contrast',
    'cost_multiplier', 'data', 'est_diff', 'estimat', 'estimate_adj',
    'fit_basic', 'formula', 'gid', 'group', 'head', 'include_n',
    'inf', 'metric_summary', 'metrics', 'model0_est', 'model_0',
    'name', 'nobs', 'oid', 'outcom', 'p.value', 'pairs', 'pct_change',
    'pid', 'power', 'power_fit', 'predictor', 'predictor_se', 'pval',
    'pval_adj', 'quantile', 'sid', 'tail', 'term', 'threshold',
    'time', 'treatment', 'tria', 'value', 'value_summary',
    'yval', 'info', 'trial', 'where', 'val'
  )
)
