\name{nnls-package}
\alias{nnls-package}
\docType{package}
\title{The Lawson-Hanson NNLS algorithm for non-negative least squares}
\description{
  An R interface to the NNLS algorithm for non-negative least squares
  that solves the least squares problem 
  \eqn{A x = b} with the constraint \eqn{x >= 0}. 
}

\author{Katharine M. Mullen <kate@nat.vu.nl> }

\references{
Lawson CL, Hanson RJ (1974). Solving Least Squares Problems. Prentice
Hall, Englewood Cliffs, NJ.

Lawson CL, Hanson RJ (1995). Solving Least Squares Problems. Classics
in Applied Mathematics. SIAM, Philadelphia.
}

\keyword{ package }
\seealso{\link{nnls}, 
  the method \code{"L-BFGS-B"} for \link{optim}
} 

