rm(list = ls())
set.seed(1001)
n = 5; p = 15
x.mat = matrix(rnorm(n*p), ncol = p); b.vec <- ifelse(1:p %% 2 == 1, 1/(1:p), 0)
exb.vec = exp(drop(x.mat %*% b.vec)); p.vec = exb.vec/(1 + exb.vec)
y.vec = rbinom(n, 1, prob = p.vec)

loss.fun = function(y.vec,x.mat,b.vec) {
  xb.vec = drop(x.mat %*% b.vec); ret = -sum(y.vec*xb.vec) + sum(log(1 + exp(xb.vec)))  
  return(ret)
}
grad.fun = function(y.vec, x.mat, b.vec) {
  exb.vec = exp(drop(x.mat %*% b.vec)); p.vec = exb.vec/(1 + exb.vec)
  ret = -drop(t(x.mat) %*% (y.vec - p.vec))
  return(ret)
}
hess.fun = function(y.vec, x.mat, b.vec) {
  exb.vec = exp(drop(x.mat %*% b.vec)); p.vec = exb.vec/(1 + exb.vec)
  ret = t(x.mat) %*% diag(p.vec*(1 - p.vec)) %*% x.mat 
  return(ret)
}
kkt.fun <- function(g.vec, b.vec, lam, eps) {
  kkt1 = prod(abs((g.vec[b.vec != 0] + lam*sign(b.vec[b.vec != 0]))) < eps)
  kkt2 = prod(abs(g.vec[b.vec == 0]) - lam < eps)
  return(kkt1 & kkt2)
}

Jtd.fun <- function(b, a, lam) {
  if (lam <= abs(b) & abs(b) <= a*lam) {
    p <- sign(b)*(a*lam - abs(b))/(a - 1)
  } else if (0 <= abs(b) & abs(b) < lam) {
    p <- lam*sign(b)
  } else {
    p <- 0
  }
  return(p - lam*sign(b))
}
Jtd.fun <- Vectorize(Jtd.fun, 'b')

quad.lasso.fun = function(q.mat, l.vec, lam, b.vec, iter.max, eps) {
  for (iter in 1:iter.max) {
    for (j in 1:length(b.vec)) {
      a = 2*q.mat[j,j] ; b = -2*sum(q.mat[j,-j]*b.vec[-j]) - l.vec[j]
      if (abs(b) < lam) { b.vec[j] = 0
      } else {b.vec[j] = sign(b)*(abs(b) - lam)/a}
    }
    grad = 2*q.mat %*% b.vec + l.vec
    conv <- kkt.fun(grad, b.vec, lam, eps)
    if (conv) break
  }
  return(list(b.vec = b.vec, conv = conv, lam = lam))
}

scad.fun <- function(y.vec, x.mat, b.vec, a, lam, iter.max = 1e3, eps = 1e-8) {
  for (iter in 1:iter.max) {
    h.mat <- hess.fun(y.vec, x.mat, b.vec) ; g.vec <- grad.fun(y.vec, x.mat, b.vec)
    q.mat <- h.mat/2 ; l.vec <- g.vec - drop(h.mat %*% b.vec) + Jtd.fun(b.vec, a, lam)
    b.vec <- quad.lasso.fun(q.mat, l.vec, lam, b.vec, iter.max, eps)$b.vec # update
    g.vec <- grad.fun(y.vec, x.mat, b.vec)
    if (kkt.fun(g.vec + Jtd.fun(b.vec, a, lam), b.vec, lam, eps)) break
  }
  return(list(b.vec = b.vec, g.vec = g.vec + Jtd.fun(b.vec, a, lam), lam = lam))
}

scad.path.fun <- function(y.vec, x.mat, a, lam.vec, iter.max = 1e3, eps = 1e-5) { # SCAD with set control
  b.mat <- NULL ; b.vec <- rep(0,ncol(x.mat))
  g.vec <- grad.fun(y.vec, x.mat, b.vec)
  for (lam in lam.vec) {
    for (iter in 1:iter.max) {
      v.vec <- (abs(g.vec) >= (lam - eps)) | (b.vec != 0)
      ax.mat <- x.mat[, v.vec, drop = F] # preserve as matrix
      fit <- scad.fun(y.vec, ax.mat, b.vec[v.vec], a, lam)
      b.vec[v.vec] <- fit$b.vec
      g.vec <- grad.fun(y.vec, x.mat, b.vec)
      if (kkt.fun(g.vec + Jtd.fun(b.vec, a, lam), b.vec, lam, eps)) break
    }
    b.mat <- cbind(b.mat, b.vec)
  }
  return(list(b.mat = b.mat, lam.vec = lam.vec))
}

scad.path.fun(y.vec, x.mat, 3.7, lam.vec = .1, iter.max = 1e3, eps = 1e-10)