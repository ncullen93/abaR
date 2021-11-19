#' @importFrom generics fit
#' @export
generics::fit


#' Generic compile method
#'
#' @param model aba-type model
#'
#' @return aba-type model
#' @export
#'
#' @examples
#' 1 == 1
compile <- function(model) {
  UseMethod('compile')
}

process_dataset <- function(data, group, outcome, predictors, covariates, params) {
  std.beta <- params$std.beta
  complete.cases <- params$complete.cases

  data <- data %>% filter(rlang::eval_tidy(rlang::parse_expr(group)))

  # workaround for empty predictor set
  if (is.null(predictors)) return(list(data))

  predictors <- strsplit(predictors, ' | ', fixed = TRUE) %>% unlist() %>% unique()

  if (std.beta) {
    data[,predictors] <- scale(data[,predictors])
  }
  if (complete.cases) {
    data <- data[complete.cases(data[,predictors]),]
  }

  return(list(data))
}


#' Fit an aba model.
#'
#' This will trigger the fitting of all statistical models
#' (`stats`) on the different parameter combinations (`spec`).
#'
#' @param object abaModel. The aba model to be fitted.
#' @param ... additional parameters.
#'
#' @return abaModel
#' @export
#' @examples
#' m <- aba_model()
fit.abaModel <- function(object, ...) {
  model <- object

  # compile model
  model <- model %>% compile()


  fit_df <- model$results %>%
    group_by(groups,outcomes,stats) %>%
    nest() %>% rename(info = data) %>% rowwise() %>%
    mutate(
      dataset = process_dataset(data = model$data,
                                group = .data$groups,
                                outcome = .data$outcomes,
                                predictors = model$spec$predictors,
                                covariates = model$spec$covariates,
                                params = model$spec$stats[[.data$stats]]$params)
    ) %>%
    unnest(info) %>%
    rowwise() %>%
    mutate(
      stats_fit = parse_then_fit_abaModel(
        data = .data$dataset,
        group = .data$groups,
        outcome = .data$outcomes,
        predictors = .data$predictors,
        covariates = .data$covariates,
        stat_obj = .data$stats_obj
      )
    ) %>%
    select(MID, everything(), -dataset) %>%
    ungroup()

  model$results <- fit_df
  return(model)
}


