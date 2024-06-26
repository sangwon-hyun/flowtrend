% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{Mstep_prob}
\alias{Mstep_prob}
\title{The M step for the cluster probabilities}
\usage{
Mstep_prob(
  resp,
  H_tf,
  countslist = NULL,
  lambda_prob = NULL,
  l_prob = NULL,
  x = NULL
)
}
\arguments{
\item{resp}{Responsibilities.}

\item{H_tf}{Trend filtering matrix.}

\item{countslist}{Particle multiplicities.}

\item{lambda_prob}{Regularization.}

\item{l_prob}{Trend filtering degree.}
}
\value{
(T x k) matrix containing the alphas, for \code{prob = exp(alpha)/
        rowSums(exp(alpha))}.
}
\description{
The M step for the cluster probabilities
}