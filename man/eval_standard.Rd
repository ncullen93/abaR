% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval_standard.R
\name{eval_standard}
\alias{eval_standard}
\title{Create a standard evaluator}
\usage{
eval_standard()
}
\value{
aba model
}
\description{
Create a standard evaluator
}
\examples{
data <- adnimerge \%>\% dplyr::filter(VISCODE == 'bl')
model <- aba_model() \%>\%
  set_data(data) \%>\%
  set_groups(everyone()) \%>\%
  set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) \%>\%
  set_predictors(
    PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
    c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
  ) \%>\%
  set_stats('glm') \%>\%
  set_evals('standard') \%>\%
  fit()
}
