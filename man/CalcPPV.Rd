% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalcPPV.R
\name{CalcPPV}
\alias{CalcPPV}
\title{CalcPPV}
\usage{
CalcPPV(sens, spec, prev)
}
\arguments{
\item{sens}{numeric; sensitivity of the binary test}

\item{spec}{numeric; specificity of the binary test}

\item{prev}{numeric; prevalence of the finding}
}
\value{
the positive predicted value of a binary test
}
\description{
This function returns the positive predictive value of a binary test
}
\examples{
CalcPPV(0.2,0.8,0.13)
}
\keyword{ppv}
