% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aba_utils.R
\name{set_predictors}
\alias{set_predictors}
\title{Set the predictors of an aba model.}
\usage{
set_predictors(.model, ..., .labels = NULL, .split = FALSE)
}
\arguments{
\item{.model}{An aba model. The model for which you want to set predictors}

\item{...}{strings or variables or tidy-selection functions. Each
comma-separated value will be a new
predictor set. If you supply actual variables, then the data of the aba
model should already be set.}

\item{.labels}{vector of strings. Optional .labels for printing & plotting. If
.labels is set to "self" then the labels will be the predictor values also.}

\item{.split}{boolean. Whether to split all variables into separate predictors
or keep together as one combined predictor. Only relevant when setting
predictors with one vector or using things like "starts_with(...)"}
}
\value{
An aba model with predictors set.
}
\description{
Predictors are the independent variables which you want to vary as a factor
in your statistical models across different groups, outcomes, and stats.
Predictors can be supplied as individual variables or as collections of
variables, so we refer to a unit of predictors as a "predictor".
This function supports both string inputs and actual variables. This function
also supports tidy-selection functions like \code{contains} and \code{starts_with} which
allows convenient selection of many variables at once with common names.
}
\examples{
data <- adnimerge \%>\% dplyr::filter(VISCODE == 'bl')

# set with variables - this will result in four "predictor sets".
model <- aba_model() \%>\%
  set_data(data) \%>\%
  set_predictors(
    PLASMA_ABETA_bl,
    PLASMA_PTAU181_bl,
    PLASMA_NFL_bl,
    c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
  )

# set with tidy selection functions - but this is only one "predictor set",
# not multiple individual predictor sets.
model <- aba_model() \%>\%
  set_data(data) \%>\%
  set_predictors(
    starts_with('PLASMA')
  )

# automatically generate all possible combinations of variables
model <- aba_model() \%>\%
  set_data(data) \%>\%
  set_predictors(
    all_combos(c('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl'))
  )

# supply strings - data does not need to be set first here. But it will
# result in an error if these variables do not éxist in the eventual data.
model <- aba_model() \%>\%
  set_data(data) \%>\%
  set_predictors(
    'PLASMA_ABETA_bl',
    'PLASMA_PTAU181_bl',
    'PLASMA_NFL_bl',
    c('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl')
  )

}
