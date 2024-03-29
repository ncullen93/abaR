% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aba_robust.R
\name{aba_robust}
\alias{aba_robust}
\title{Evaluate the robustness of an aba model to systematic and random error.}
\usage{
aba_robust(model, bias = NULL, variation = NULL, ntrials = 100, verbose = TRUE)
}
\arguments{
\item{model}{an aba model. The fitted aba model to perform robustness
analysis on.}

\item{bias}{double or list of doubles. If one value is given, this is the
percent value added or subtracted to all predictor values at each trial. If
this is a list, the names of the list should be the predictors to apply
bias to and the values should be the bias to apply to each predictor.}

\item{variation}{double or list of doubles. This is the percent value which
represents the standard deviation of a normal distribution. The random
error values will be randomly sampled from this normal distribution for
each data row (participant) at each trial.}

\item{ntrials}{integer. Number of trials to run. A trial represents a
different random sampling of the variation distribution. This does not
have any effect for bias because the bias value is always the same.}

\item{verbose}{logical. Whether to include a progress bar to track trials.}
}
\value{
an abaRobust object which contains results from the robustness
analysis that displays how model coefficients and metrics changed when
bias and variation was injected into the predictors.
}
\description{
This function allows you to test how adding bias to predictor values or how
adding random error to predictor values affects the model coefficients and
performance metrics (e.g., AUC, R2, etc) as a result. This function is useful
when you have test-retest estimates of biomarkers and want to test what
effect this has on diagnostic or prognostic modelling.
}
\examples{

# read and process data
data <- adnimerge \%>\% dplyr::filter(VISCODE == 'bl')

# fit a standard model to predict a binary outcome
model <- data \%>\% aba_model() \%>\%
  set_groups(everyone()) \%>\%
  set_outcomes(CSF_ABETA_STATUS_bl) \%>\%
  set_predictors(PLASMA_PTAU181_bl, PLASMA_NFL_bl) \%>\%
  set_stats(stat_roc(method='Youden', direction = '<')) \%>\%
  aba_fit()

# summarise model (these are the original results)
model_summary <- model \%>\% aba_summary()

# specify test-retest variation for predictors (defined as percent change)
# this can be theoretical values (e.g. 5, 10, 15, 20) or derived from
# test-retest studies where you measured the biomarkers twice
variation <- list(
  'PLASMA_PTAU181_bl' = 9.5,
  'PLASMA_NFL_bl' = 20.2
)

# test robustness of the fitted aba model to this robustness
model_robust <- model \%>\%
  aba_robust(
    variation = variation,
    ntrials = 10,
    verbose = TRUE
  )

# plot results using the generic plot function
fig <- model_robust \%>\% aba_plot_metric()

}
