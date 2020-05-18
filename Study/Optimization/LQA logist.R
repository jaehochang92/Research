# Primitive functions -----------------------------------------------------

rm(list = ls())
pos <- function(x) {
  x*(x > 0)
}
expit <- function(x) exp(x)/(1 + exp(x))

# Design ------------------------------------------------------------------

set.seed(1001)
n <- 1e1
p <- 1e2

x.mat <- matrix(rnorm(n*p), nrow = n)
b.vec <- 1/1:(p)
y.vec <- rbinom(n, 1, expit(x.mat %*% b.vec))

# loss function -----------------------------------------------------

lss <- function(B, lmd = 1, y = y.vec, X = x.mat) { # loss functions
  xb <- drop(X %*% B)
  - sum(y*xb) + sum(log(1 + exp(xb))) + lmd*sum(abs(B))
}
lss(rep(0, p))
lss(b.vec)

# Gradient ----------------------------------------------------------------

G <- function(B, y = y.vec, X = x.mat) {
  pi = expit(X %*% B)
  drop(t(X) %*% (-y + pi))
}
b0 <- rep(0, p) ; lss(b0)
b1 <- b0 - 0.001*G(b0) ; lss(b1)

# Hessian -----------------------------------------------------------------

H <- function(B, y = y.vec, X = x.mat) {
  pi <- expit(drop(X %*% B)) 
  t(X) %*% diag(pi*(1 - pi)) %*% X
}
eigen(H(rnorm(p)))

# main ----------------------------------------------------------------------

lmd <- 1
eps <- 1e-10 ; it.max <- 1e2
b.vec <- rep(0, p)
for (i1 in 1:it.max) {
  Hess <- H(b.vec) ; Grd <- G(b.vec)
  ocb.vec <- cb.vec <- b.vec
  print(lss(b.vec, lmd))
  for (i2 in 1:it.max) {
    for (j in 1:p) {
      a <- Hess[j, j]/2
      b <- drop(t(Hess[-j, j]) %*% cb.vec[-j] + Grd[j] - t(Hess[, j]) %*% b.vec)
      cb.vec[j] <- sign(-b)*pos(abs(b) - lmd)/2/a
    }
    if (sum(abs(cb.vec - ocb.vec)) < eps) break
    ocb.vec <- cb.vec
  }
  nb.vec <- cb.vec
  if (sum(abs(b.vec - nb.vec)) < eps) break
  b.vec <- nb.vec
}


# For general Use -------------------------------------------------------

LQA <- function(b.v, A.m, a.v, lmd) {
  cb.vec <- rep(0, p)
  for (j in 1:p) {
    gj <- -2*t(A.m[j, -j]) %*% b.v[-j] - a.v[j]
    cb.vec[j] <- sign(gj/A.m[j, j])*pos(abs(gj/2/A.m[j, j]) - lmd/2/A.m[j, j])
  }
  cb.vec
}