% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{boot_escapement}
\alias{boot_escapement}
\title{Calculate bootstrapped 95 percent confidence intervals around salmon escapement estimates}
\usage{
boot_escapement(dat, models)
}
\arguments{
\item{dat}{a data frame containing formatted salmon count data returned by \code{import_format()}}

\item{models}{a list of model output returned from \code{model_escapement}}
}
\value{
a list containing a summary of the bootstrapped estimates (\code{summary}) and the raw bootstrapped results (\code{raw_boots})
}
\description{
Calculate bootstrapped 95 percent confidence intervals around salmon escapement estimates
}
\examples{
\dontrun{
boot_escapement(dat, models)
}
}
