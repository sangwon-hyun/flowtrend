% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{underfit_flowmeans}
\alias{underfit_flowmeans}
\title{Pool the entire series of cytograms, fit a flowMeans model, and re-aggregate parameters}
\usage{
underfit_flowmeans(ylist, numclust)
}
\arguments{
\item{ylist}{Data.}

\item{numclust}{Number of clusters.}
}
\value{
List object with flowtrend model estimates using l=lprob=0 and
extremely large regularization parameters so that all cytograms are the
same across time.
}
\description{
Pool the entire series of cytograms, fit a flowMeans model, and re-aggregate parameters
}