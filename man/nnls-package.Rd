\name{nnls-package}
\alias{nnls-package}
\docType{package}
\title{The Lawson-Hanson NNLS algorithm for non-negative least squares}
\description{
  An R interface to the NNLS algorithm for non-negative least squares
  that solves the least squares problem
  \eqn{Ax = b} with the constraint \eqn{x \geq 0} where
  where
  \eqn{x \in R^n, b \in R^m}  and \eqn{A} is an
  \eqn{m \times n} matrix. 
  Also allows the combination of non-negative and non-positive
  constraints on \eqn{x}. 
}

\author{Katharine M. Mullen <kate@nat.vu.nl> }

\references{
Lawson CL, Hanson RJ (1974). Solving Least Squares Problems. Prentice
Hall, Englewood Cliffs, NJ.

Lawson CL, Hanson RJ (1995). Solving Least Squares Problems. Classics
in Applied Mathematics. SIAM, Philadelphia.
}

\keyword{ package }
\seealso{ \link{nnls},\link{nnnpls},
  the method \code{"L-BFGS-B"} for \link{optim},
   \link[pkg]{quadprog}, \link[pkg]{bvls}
} 

