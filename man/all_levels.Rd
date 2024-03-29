% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_external.R
\name{all_levels}
\alias{all_levels}
\title{Create groups from all levels of one or more variables}
\usage{
all_levels(...)
}
\arguments{
\item{...}{variables. Variables in data}
}
\value{
N/A. Has implications for aba model groups.
}
\description{
This function should only be used within a call to \code{set_groups()}. This allows
you to create groups for every unique level of a variable or every combination
of unique levels of multiple variables. Also, the data of the aba model must
already be set.
}
\examples{
data <- adnimerge \%>\% dplyr::filter(VISCODE == 'bl')
# set groups as all of one variable's levels
model <- data \%>\% aba_model() \%>\%
  set_groups(
    all_levels(DX_bl)
  )

# set groups as all combinations of two variables' levels
model <- data \%>\% aba_model() \%>\%
  set_groups(
    all_levels(DX_bl, CSF_ABETA_STATUS_bl)
  )

# use all_levels() in combination with additional, independent group statements
model <- data \%>\% aba_model() \%>\%
  set_groups(
    DX_bl == 'CU',
    all_levels(DX_bl, CSF_ABETA_STATUS_bl)
  )
}
