#' Create a demographics table from a fitted aba model.
#'
#' This function allows you to automatically create a demographics table from a
#' fitted aba model. The variables in the table will be inferred from the
#' spec of the model (predictors, covariates, outcomes, etc.), although this
#' can be customized.
#'
#' Note that support is weaker for longitudinal data right now.
#'
#' @param object abaModel. The fitted aba model to create demographics table from.
#' @param strata string (optional). How to stratify the demographics table.
#' @param include_predictors boolean. Whether to include predictors in table.
#' @param include_covariates boolean. Whether to include covariates in table.
#' @param include_outcomes boolean. Whether to include outcomes in table.
#' @param add_vars character vector (optional). Any additional variables to
#'   add to the demographics table. These variables should be present in the
#'   data from the aba model.
#' @param data_filter logical expression (optional). If this is specified, the
#'   data from the aba model will be further filtered before the table is made.
#'
#' @return a TableOne object (see `tableone` package).
#' @export
#'
#' @examples
#'
#' model <- aba_model() %>%
#'   set_data(adnimerge %>% dplyr::filter(VISCODE == 'bl')) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats('glm') %>%
#'   aba_fit()
#'
#' my_table <- model %>% aba_demographics(strata = 'DX_bl')
#' print(my_table)
#'
aba_demographics <- function(object,
                             strata = NULL,
                             include_predictors = TRUE,
                             include_covariates = TRUE,
                             include_outcomes = TRUE,
                             add_vars = NULL,
                             data_filter = NULL) {
  data <- object$data
  if (!is.null(data_filter)){
    data <- data %>% filter(rlang::eval_tidy(rlang::parse_expr(data_filter)))
  }

  predictors <- object$predictors %>% unlist() %>% unique()
  covariates <- object$covariates
  outcomes <- object$outcomes %>% unlist()

  all_vars <- c()
  if (include_covariates) all_vars <- c(all_vars, covariates)
  if (include_outcomes) all_vars <- c(all_vars, outcomes)
  if (include_predictors) all_vars <- c(all_vars, predictors)

  if (!is.null(add_vars)) all_vars <- c(all_vars, add_vars)

  factor_vars <- all_vars[
    all_vars %>% purrr::map_lgl(~class(data[[.]]) %in% c('character', 'factor'))
  ]

  # TODO...
  # check if time variable is present in any stats
  # describe only baseline if longitudinal data is used
  if (is.null(strata)) {
    tbl <- suppressWarnings(
      tableone::CreateTableOne(
        vars = all_vars,
        factorVars = factor_vars,
        data = data,
        test = TRUE, includeNA = TRUE, addOverall = T
      )
    )
  } else {
    tbl <- suppressWarnings(
      tableone::CreateTableOne(
        vars = all_vars,
        factorVars = factor_vars,
        data = data,
        strata = strata,
        test = TRUE, includeNA = TRUE, addOverall = T
      )
    )
  }

  return(tbl)
}
