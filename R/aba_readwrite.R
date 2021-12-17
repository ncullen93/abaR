
#' Write an aba object to file.
#'
#' This is a generic function for writing an aba object to file. Objects can
#' be written to file as a "table" (formatted), as "raw" (long-form results),
#' or as an "object" (actual aba object).
#'
#' @param object an aba object. The object to save to file.
#' @param filename string. The filename to save to. Supported extensions include
#'   "csv", "xls", and "xlsx".
#' @param format string. How to save the object to file. Options include
#'   "table" (formatted results like you see when you print the object to the
#'   console), "raw" (long-form results like what you see when you call
#'   `object$results`), or "object" (the actual aba object which can be later
#'   be loaded into memory and used again).
#' @param split logical. Whether to save the results in split files (for
#' csv) or split sheets (for excel) based on group - outcome - stat
#' combinations. This argument is ignored if format == "object".
#'
#' @return N/A
#' @export
#'
#' @examples
#'
#' # create temp files to save to
#' tmp_filename_csv <- tempfile(fileext = '.csv')
#' tmp_filename_rda <- tempfile(fileext = '.Rda')
#'
#' # grab built-in data
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit model
#' model <- data %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm') %>%
#'   fit()
#'
#' # summarise model
#' model_summary <- model %>% summary()
#'
#' # save model summary to file as table
#' model_summary %>% aba_write(tmp_filename_csv)
#'
#' # save model summary to file as raw long-form results
#' model_summary %>% aba_write(tmp_filename_csv, format = 'raw')
#'
#' # save model summary as an object which can be loaded back into memory
#' model_summary %>% aba_write(tmp_filename_rda, format = 'object')
#'
#' # load summary back to file to show it works
#' model_summary2 <- aba_read(tmp_filename_rda)
#' print(model_summary2)
#'
#' # delete temp files
#' removed <- file.remove(tmp_filename_csv)
#' removed <- file.remove(tmp_filename_rda)
#'
aba_write <- function(object,
                      filename,
                      format = c('table', 'raw', 'object'),
                      split = FALSE) {
  UseMethod('aba_write')
}


#' @export
aba_write.abaSummary <- function(object,
                                 filename,
                                 format = c('table', 'raw', 'object'),
                                 split = FALSE) {
  format <- match.arg(format)
  file_ext <- stringr::str_split(filename, '\\.')[[1]] %>% tail(1)
  file_base <- stringr::str_split(filename, '\\.')[[1]][1]

  if (format %in% c('table', 'raw')) {
    results <- object$results$coefs %>%
      mutate(form = 'coef') %>%
      bind_rows(
        object$results$metrics %>%
          mutate(form = 'metric')
      )
    if (format == 'table') results <- object %>% as_table()
    save_helper(results, filename, split)
  } else {
    saveRDS(object = object, file = filename)
  }

}

save_helper <- function(results, filename, split) {
  file_ext <- stringr::str_split(filename, '\\.')[[1]] %>% tail(1)
  file_base <- stringr::str_split(filename, '\\.')[[1]][1]
  # if no split, just save entire file
  # otherwise, split and save in separate files
  if (split[1] == FALSE) {
    results %>% utils::write.csv(filename, row.names = FALSE)
  } else {
    if (split[1] == TRUE) {
      split <- c('group', 'outcome')
      if (n_distinct(results$outcome) > 10*n_distinct(results$predictor)) {
        split <- c('group', 'predictor')
      }
    }
    if (length(split) != 2) stop('split must have length == 2.')
    a1 <- split[1]
    a2 <- split[2]

    tbl_nested <- results %>%
      group_by(
        .data[[a1]],
        .data[[a2]],
        .data$stat
      ) %>%
      nest() %>%
      mutate(
        label =
          glue('{tup(a1)} = {.data[[a1]]} | {tup(a2)} = {.data[[a2]]} | Stat = {stat}')
      )

    tbl_split <- stats::setNames(
      split(tbl_nested, 1:nrow(tbl_nested)),
      tbl_nested$label
    )

    if (file_ext == 'csv') {
      tbl_split %>% purrr::iwalk(
        function(x,y) {
          tmp_label <- y
          tmp_data <- x$data[[1]][,colMeans(is.na(x$data[[1]])) < 1]
          tmp_data %>%
            utils::write.csv(
              glue('{file_base} ({tmp_label}).{file_ext}'),
              row.names = FALSE
            )
        }
      )
    } else {
      tbl_split <- tbl_split %>%
        purrr::map(
          ~.$data[[1]][,colMeans(is.na(.$data[[1]])) < 1]
        )
      suppressWarnings(
        tbl_split %>% writexl::write_xlsx(filename)
      )
    }
  }
}

#' @export
aba_write.TableOne <- function(object,
                               filename,
                               format = c('table', 'raw', 'object'),
                               split = FALSE) {
  r <- object

  if (endsWith(filename, '.csv')) {
    utils::write.csv(
      print(r, showAllLevels=TRUE),
      filename,
      fileEncoding = 'UTF-8'
    )
  } else {
    stop('Filename must end in .csv')
  }
}

#' Read an aba object from file
#'
#' This function allows you to read back into memory an aba object which was
#' previously saved. This function is not relevant for loading results tables
#' as you can just use `read.csv` or `read_excel` and the like. Note that this
#' function essential just wraps `readRDS` for reading an Rda object.
#'
#' @param filename string. The filename where the aba object is saved.
#'
#' @return an aba object
#' @export
#'
#' @examples
#' # create temp files to save to
#' tmp_filename_rda <- tempfile(fileext = '.Rda')
#'
#' # grab built-in data
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit a standard aba model
#' model <- data %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm') %>%
#'   fit()
#'
#' # create a model summary
#' model_summary <- model %>% aba_summary()
#'
#' # save model summary as an object which can be loaded back into memory
#' model_summary %>% aba_write(tmp_filename_rda, format = 'object')
#'
#' # load summary back to file to show it works
#' model_summary2 <- aba_read(tmp_filename_rda)
#'
#' # delete temp files
#' removed <- file.remove(tmp_filename_rda)
#'
aba_read <- function(filename) {
  object <- readRDS(file = filename)
  return(object)
}


