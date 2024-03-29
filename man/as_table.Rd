% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aba_summary.R
\name{as_table}
\alias{as_table}
\title{Convert an aba summary to a nicely formatted table}
\usage{
as_table(object)
}
\arguments{
\item{object}{abaSummary. The aba summary to format as a table.}
}
\value{
a tibble
}
\description{
This function allows you to format an aba summary in the same way
which it is printed to the console using the \code{print} function. However,
only one dataframe will result (i.e., the tables will not be split by
group - outcome - stat combinations).
}
\examples{
# use built-in data
data <- adnimerge \%>\% dplyr::filter(VISCODE == 'bl')

# fit an aba model
model <- data \%>\% aba_model() \%>\%
  set_groups(everyone()) \%>\%
  set_outcomes(PET_ABETA_STATUS_bl) \%>\%
  set_predictors(
    PLASMA_PTAU181_bl,
    PLASMA_NFL_bl,
    c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
  ) \%>\%
  set_covariates(AGE, GENDER, EDUCATION) \%>\%
  set_stats('glm') \%>\%
  fit()

# default aba summary
model_summary <- model \%>\% aba_summary()

# convert summary to table
my_table <- model_summary \%>\% as_table()
}
