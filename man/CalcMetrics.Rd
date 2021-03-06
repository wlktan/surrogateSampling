% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalcMetrics.R
\name{CalcMetrics}
\alias{CalcMetrics}
\title{CalcMetrics}
\usage{
CalcMetrics(test.vec, truth.vec, control = 0, case = 1)
}
\arguments{
\item{test.vec}{Vector of 0/1 labels of test}

\item{truth.vec}{Vector of 0/1 labels of truth}

\item{control}{Value for control, defaults to 0}

\item{case}{Value for case, defaults to 1}
}
\value{
two.by.two: Vector for numbers of TP,FP,TN,FN,N
  metrics.list: Vector with prev,sens,spec,ppv,npv,fscore,auc of test.vec for truth.vec
}
\description{
This function takes two vectors, tabulates 2x2 table,
and calculates various accuracy metrics
}
\examples{
CalcMetrics(c(1,0), c(1,0))
}
\keyword{npv}
\keyword{ppv,}
\keyword{sensitivity,}
\keyword{specificity,}
