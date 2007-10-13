nnls <- function(A, b) {
  MDA <- M <- nrow(A) 
  N <- ncol(A)
  RNORM <- MODE <- 0
  W <- INDEX <- X <- rep(0, N)
  ZZ <- rep(0, M)
  sol <- .Fortran("nnls", A = as.numeric(A), MDA = as.integer(MDA), M =
                 as.integer(M), N = as.integer(N), B = as.numeric(b),
                 X = as.numeric(X), RNORM = as.numeric(RNORM), W =
                 as.numeric(W), ZZ = as.numeric(ZZ), INDEX =
                 as.integer(INDEX), MODE = as.integer(MODE),
                 PACKAGE="nnls")
  fitted <- A %*% sol$X
  resid <-  b - fitted 
  nnls.out <- list(x=sol$X, deviance=sol$RNORM^2,
              residuals=resid, fitted = fitted,mode=sol$MODE)
  class(nnls.out) <- "nnls"
  nnls.out 
}
nnnpls <- function(A, b, con) {
  MDA <- M <- nrow(A) 
  N <- ncol(A)
  RNORM <- MODE <- 0
  W <- INDEX <- X <- rep(0, N)
  ZZ <- rep(0, M)
  sol <- .Fortran("nnnpls", A = as.numeric(A), MDA = as.integer(MDA), M =
                  as.integer(M), N = as.integer(N), 
                  CON = as.numeric(con), 
                  B = as.numeric(b),
                  X = as.numeric(X), 
                  RNORM = as.numeric(RNORM), W =
                  as.numeric(W), ZZ = as.numeric(ZZ), INDEX =
                  as.integer(INDEX), MODE = as.integer(MODE),
                  PACKAGE="nnls")
  fitted <- A %*% sol$X
  resid <-  b - fitted 
  nnnpls.out <- list(x=sol$X, deviance=sol$RNORM^2,
                    residuals=resid, fitted = fitted,mode=sol$MODE)
  class(nnnpls.out) <- "nnnpls"
  nnnpls.out 
}
print.nnnpls <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("Nonnegative-nonpositive least squares model\n")

    cat("x estimates:", x$x, "\n")
    cat("residual sum-of-squares: ", format(x$deviance, digits = digits),
	"\n", sep = '')
    stopmess <- switch(x$mode, "The solution has been computed sucessfully.",
                       "The dimensions of the problem are bad",
                       "Iteration count exceded.  More than 3*N iterations.")
        
    cat("reason terminated: ", stopmess, "\n", sep='')
    invisible(x)
}
print.nnls <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("Nonnegative least squares model\n")

    cat("x estimates:", x$x, "\n")
    cat("residual sum-of-squares: ", format(x$deviance, digits = digits),
	"\n", sep = '')
    stopmess <- switch(x$mode, "The solution has been computed sucessfully.",
                       "The dimensions of the problem are bad",
                       "Iteration count exceded.  More than 3*N iterations.")
        
    cat("reason terminated: ", stopmess, "\n", sep='')
    invisible(x)
}

residuals.nnls <- residuals.nnnpls <- function(object, ...)  object$residuals
coef.nnls <- coef.nnnpls <- function(object, ...) object$x
fitted.nnls <- fitted.nnnpls <- function(object, ...) object$fitted
deviance.nnls <- deviance.nnnpls <- function(object, ...) object$deviance
