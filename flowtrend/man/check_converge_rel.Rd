% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{check_converge_rel}
\alias{check_converge_rel}
\title{Checks numerical improvement in objective value. Returns TRUE if |old-new|/|old| is smaller than tol.}
\usage{
check_converge_rel(old, new, tol = 1e-06)
}
\arguments{
\item{old}{Objective value from previous iteration.}

\item{new}{Objective value from current iteration.}

\item{tol}{Numerical tolerance.}
}
\description{
Checks numerical improvement in objective value. Returns TRUE if |old-new|/|old| is smaller than tol.
}