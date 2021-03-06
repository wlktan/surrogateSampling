% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalcFisherInfo.R
\name{CalcFisherInfo}
\alias{CalcFisherInfo}
\title{CalcFisherInfo}
\usage{
CalcFisherInfo(X, true.beta)
}
\arguments{
\item{X}{design matrix}

\item{true.beta}{vector of true betas}
}
\value{
the fisher information of a design matrix
}
\description{
This function returns the fisher information of a design matrix
}
\examples{
CalcFisherInfo(matrix(data=c(1,2,3,2),nrow=2),true.beta=c(1,2))
}
\keyword{fisherinfo}
