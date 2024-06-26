% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{get_max_lambda}
\alias{get_max_lambda}
\title{A wrapper for \code{calc_max_lambda}. Saves the two maximum lambda values in
a file.}
\usage{
get_max_lambda(
  destin,
  maxres_file = "maxres.Rdata",
  ylist,
  countslist,
  numclust,
  maxdev,
  max_lambda_mean,
  max_lambda_prob,
  ...
)
}
\arguments{
\item{destin}{Where to save the output (A two-lengthed list called
"maxres").}

\item{maxres_file}{Filename for output. Defaults to maxres.Rdata.}

\item{ylist}{List of responses.}

\item{numclust}{Number of clusters.}

\item{max_lambda_mean}{Defaults to 4000.}

\item{max_lambda_prob}{Defaults to 1000.}

\item{...}{Additional arguments to \code{flowtrend()}.}
}
\value{
No return
}
\description{
A wrapper for \code{calc_max_lambda}. Saves the two maximum lambda values in
a file.
}