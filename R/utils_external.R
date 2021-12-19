#' Create all possible combinations of a set of variables
#'
#' This function creates all possible combinations of a set of variables. The
#' variables should be given as strings and sep. This function can be used
#' inside of a call to `set_predictors` when creating an aba model.
#'
#' @param ... strings. Variable names from which all possible combinations
#'   will be created. Each variable string should be separated by a comma.
#'
#' @return A list of vectors of all possible combinations of the variables
#' @export
#'
#' @examples
#'
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit model with all combinations of three variables
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     DX_bl %in% c('MCI', 'AD')
#'   ) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     all_combos('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl')
#'   ) %>%
#'   set_stats(
#'     stat_glm(std.beta = TRUE)
#'   ) %>%
#'   fit()
#'
#' model_summary <- model %>% aba_summary()
#'
all_combos <- function(...) {
  values <- c(...)
  combo_vals <- seq_along(values) %>%
    map(~combn(values, .x, simplify=FALSE)) %>%
    flatten()
  return(combo_vals)
}


#' Create groups from all levels of one or more variables
#'
#' This function should only be used within a call to `set_groups()`. This allows
#' you to create groups for every unique level of a variable or every combination
#' of unique levels of multiple variables. Also, the data of the aba model must
#' already be set.
#'
#' @param ... variables. Variables in data
#'
#' @return N/A. Has implications for aba model groups.
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#' # set groups as all of one variable's levels
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     all_levels(DX_bl)
#'   )
#'
#' # set groups as all combinations of two variables' levels
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     all_levels(DX_bl, CSF_ABETA_STATUS_bl)
#'   )
#'
#' # use all_levels() in combination with additional, independent group statements
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     DX_bl == 'CU',
#'     all_levels(DX_bl, CSF_ABETA_STATUS_bl)
#'   )
all_levels <- function(...) {
  list(...)
}

#' Use all data rows as a group in an aba model.
#'
#' This is a helper function which allows you to specify a group in an aba model
#' that does not have any filtering conditions. This is useful when you want
#' to specify an aba model with one sub-group of the data but also want to
#' fit models on the entire data. This function is really only necessary to be
#' used instead of a call to `set_groups` when building an aba model.
#'
#' @return This function actually just returns a value of TRUE.
#' @export
#'
#' @examples
#'
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit model with one subgroup (DX_bl) and also the entire data
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     DX_bl %in% c('MCI', 'AD')
#'   ) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl) %>%
#'   set_stats(
#'     stat_glm(std.beta = TRUE)
#'   ) %>%
#'   fit()
#'
#' model_summary <- model %>% aba_summary()
#'
everyone <- function() {
  TRUE
}
