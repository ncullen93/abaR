#' Create all possible combinations of a set of variables
#'
#' @param ... strings. Variable names to create combinations of
#'
#' @return list of vectors
#' @export
#'
#' @examples
#' x <- all_combos('a','b','c')
all_combos <- function(...) {
  values <- c(...)
  combo_vals <- seq_along(values) %>%
    map(~combn(values, .x, simplify=F)) %>%
    flatten()
  return(combo_vals)
}

# used to include all rows of dataset (no filtering)
# e.g. aba_model() %>% set_groups(DX_bl=='CU', everyone())
everyone <- function() {
  TRUE
}