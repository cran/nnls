\name{nnls}
\alias{nnls}
\title{The Lawson-Hanson NNLS algorithm for non-negative least squares}
\description{
  An R interface to the Lawson-Hanson
  NNLS algorithm for non-negative least squares
  that solves the least squares problem 
  \eqn{A x = b} with the constraint \eqn{x >= 0}. 
}
\usage{
nnls(A, b)
}
\arguments{
 \item{A}{numeric matrix with \code{m} rows and \code{n} columns}
 \item{b}{numeric vector of length \code{m} }
} 
\value{
numeric vector of length \code{n} containing the solution. 
}
\references{
Lawson CL, Hanson RJ (1974). Solving Least Squares Problems. Prentice
Hall, Englewood Cliffs, NJ.

Lawson CL, Hanson RJ (1995). Solving Least Squares Problems. Classics
in Applied Mathematics. SIAM, Philadelphia.
}
\author{Katharine M. Mullen <kate@nat.vu.nl> }

\source{
  This is an R interface to the unmodified Fortran77 code distributed
  with the book referenced below by Lawson CL, Hanson RJ (1995),
  obtained from Netlib (file \file{lawson-hanson/all}).
  
}
\seealso{\link{nnls}, 
the method \code{"L-BFGS-B"} for \link{optim}
} 
\examples{
## simulate a matrix A 
t <- seq(0, 2, by = .04)
k <- c(.5, .6, 1)
A <- matrix(nrow = 51, ncol = 3)
Acolfunc <- function(k, t) exp(-k*t)
for(i in 1:3) A[,i] <- Acolfunc(k[i],t)

## simulate a data vector b based on A 
E <- matrix(nrow = 51, ncol = 3)
wavenum <- seq(18000,28000, by=200)
location <- c(25000, 22000, 20000) 
delta <- c(3000,3000,3000)
Ecolfunc <- function(wavenum, location, delta)
  exp( - log(2) * (2 * (wavenum - location)/delta)^2)
for(i in 1:3) E[,i] <- Ecolfunc(wavenum, location[i], delta[i])
set.seed(3312)
matdat <- A \%*\% t(E) + .01 * rnorm(nrow(A) * nrow(E))
## this is the data vector b
b <- matdat[,20]

## these are the x values to recover
realx <- E[20,] 

## by least squares criteria, there are negative values in x 
qr.coef(qr(A),b)  

## -------- CALL TO NNLS ------
## solve A x = b subject to x >= 0 
(x <- nnls(A = A, b = b))
## ----------------------------

\dontrun{

## can solve the same problem with L-BFGS-B algorithm
## but need starting values for x 
startval <- rep(0, ncol(A))
fn1 <- function(par1) sum( ( b - A \%*\% par1)^2)
(x_lbfgsb <- optim(startval, fn = fn1, lower = rep(0,3),
                  method="L-BFGS-B")$par)

## RMS deviation (xreal - x_lbfgsb) > RMS deviation (xreal - x)
sqrt(sum((realx - x)^2)) < sqrt(sum((realx - x_lbfgsb)^2))

## and L-BFGS-B is much slower 
nnlsrep <- function() for(i in 1:100) nnls(A = A, b = b)
lbfgsbrep <- function() for(i in 1:100) optim(startval, fn = fn1,
                                  lower = rep(0, 3), method="L-BFGS-B")
(nnls_time <- system.time(nnlsrep()))
(lbfgsb_time <- system.time(lbfgsbrep()))

## can also solve the same problem by reformulating it as a
## quadratic program (this requires the quadprog package; if you
## have quadprog installed, uncomment lines below starting with
## only 1 "#" )

# library(quadprog)
# dvec <- crossprod(A,b) 
# bvec <- rep(0, ncol(A))
# Dmat <- crossprod(A,A)
# Amat <- diag(ncol(A))
# (x_qp <- solve.QP(dvec = dvec, bvec = bvec, Dmat=Dmat,
         Amat=Amat)$solution)

## RMS deviation (xreal - x_qp) > RMS deviation (xreal - x)
# sqrt(sum((realx - x)^2)) < sqrt(sum((realx - x_qp)^2)) 

# qprep <- function() for(i in 1:100) solve.QP(dvec = dvec, bvec = bvec,
#                                     Dmat = Dmat, Amat = Amat)
## and quadprog is also slower, even not taking into account time
## for problem reformulation
# (qp_time <- system.time(qprep()))
}

}
\keyword{optimize}
