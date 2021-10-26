#glance_extra <- function(fit, outcome, ...) {
#  UseMethod('glance_extra')
#}
#
glance_extra <- function(fit, outcome, ...) {

  data <- model.frame(fit) %>% tibble() %>%
    mutate(
      .Predicted = predict(fit, type='response'),
      .Truth := factor(.data[[outcome]])
    )

  auc <- yardstick::roc_auc(
    data,
    .Truth,
    .Predicted,
    event_level = 'second'
  )[['.estimate']]

  res <- tibble::tibble(
    auc
  ) %>%
    set_names(c('AUC'))
}
