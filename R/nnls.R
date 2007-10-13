`nnls` <- function(A, b) {
  MDA <- M <- nrow(A) 
  N <- ncol(A)
  RNORM <- MODE <- 0
  W <- INDEX <- X <- rep(0, N)
  ZZ <- rep(0, M)
  X <- .Fortran("nnls", A = as.numeric(A), MDA = as.integer(MDA), M =
                 as.integer(M), N = as.integer(N), B = as.numeric(b),
                 X = as.numeric(X), RNORM = as.numeric(RNORM), W =
                 as.numeric(W), ZZ = as.numeric(ZZ), INDEX =
                 as.integer(INDEX), MODE = as.integer(MODE),
                 PACKAGE="nnls")$X
}

