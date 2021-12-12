
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
#' @param separate logical. Whether to save the results in separate files (for
#' csv) or separate sheets (for excel) based on group - outcome - stat
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
                      separate = FALSE) {
  UseMethod('aba_write')
}


#' @export
aba_write.abaSummary <- function(object,
                                 filename,
                                 format = c('table', 'raw', 'object'),
                                 separate = FALSE) {
  format <- match.arg(format)

  if (format == 'table') {

    results <- object %>% as_table()
    save_helper(results, filename, separate)

  } else if (format == 'raw') {

    results <- object$results
    save_helper(results$coefs, filename %>% stringr::str_replace('\\.','_coefs.'), separate)
    save_helper(results$metrics, filename %>% stringr::str_replace('\\.','_metrics.'), separate)

  } else if (format == 'object') {

    saveRDS(
      object = object,
      file = filename
    )

  }
}


#' @export
aba_write.TableOne <- function(object,
                               filename,
                               format = c('table', 'raw', 'object'),
                               separate = FALSE) {
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


# helper function for saving results to file
save_helper <- function(results, filename, separate) {
  file_ext <- stringr::str_split(filename, '\\.')[[1]] %>% tail(1)

  if (file_ext == 'csv') {
    results %>%
      utils::write.csv(
        filename,
        row.names = FALSE
      )
  } else if (file_ext %in% c('xls', 'xlsx')) {
    if (!separate) {
      results %>%
        writexl::write_xlsx(filename)
    } else {
      # group and set label
      results <- results %>%
        group_by(
          .data$group,
          .data$outcome,
          .data$stat
        ) %>%
        nest() %>%
        mutate(
          label = glue('{group} | {outcome} | {stat}')
        )

      # split into separate tables
      results <- split(results, 1:nrow(results)) %>%
        set_names(unique(results$label))

      results <- results %>% purrr::map(~.x[['data']][[1]])

      # save to file
      results %>%
        writexl::write_xlsx(filename)
    }
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


