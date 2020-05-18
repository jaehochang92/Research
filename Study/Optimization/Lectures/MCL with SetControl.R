rm(list = ls())
n = 1e2; p = 1e1
x.mat = matrix(rnorm(n*p), ncol = p); b.vec = 1/(1:p)
exb.vec = exp(drop(x.mat %*% b.vec)); m.vec = exb.vec
y.vec = rpois(n, m.vec)

loss.fun = function(y.vec,x.mat,b.vec) {
  xb.vec = drop(x.mat %*% b.vec)
  ret = -sum(y.vec*xb.vec) + sum(exp(xb.vec)) + sum(log(factorial(y.vec)))
  return(ret)
}
grad.fun = function(y.vec, x.mat, b.vec) {
  exb.vec = exp(drop(x.mat %*% b.vec))
  ret = t(x.mat) %*% (-y.vec + exb.vec)
  return(ret)
}
hess.fun = function(y.vec, x.mat, b.vec) {
  exb.vec = exp(drop(x.mat %*% b.vec))
  ret = t(x.mat) %*% diag(exb.vec) %*% x.mat 
  return(ret)
}
kkt.fun <- function(g.vec, b.vec, lam, eps) {
  kkt1 = prod(abs((g.vec[b.vec != 0] + lam*sign(b.vec[b.vec != 0]))) < eps)
  kkt2 = prod(abs(g.vec[b.vec == 0]) - lam < eps)
  return(kkt1 & kkt2)
}

grad.J.fun <- function(t, a, gam, lam) (sign(t)*max(gam, lam - abs(t)/a))
plot(Vectorize(function(t) grad.J.fun(t, 2, .5, 1)), n = 1e3, -3, 3)

grad.S.fun <- function(t, a, gam, lam) {grad.J.fun(t, a, gam, lam) - lam*sign(t)}
grad.S.fun <- Vectorize(grad.S.fun, 't')
plot(function(t) grad.S.fun(t, 2, .5, 1), n = 1e3, -3, 3)

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

mcl.fun <- function(y.vec, x.mat, b.vec, a, gam, lam, iter.max = 1e3, eps = 1e-8) {
  l <- c()
  for (iter in 1:iter.max) {
    h.mat <- hess.fun(y.vec, x.mat, b.vec) ; g.vec <- grad.fun(y.vec, x.mat, b.vec)
    q.mat <- h.mat/2 ; l.vec <- g.vec - drop(h.mat %*% b.vec) + grad.S.fun(b.vec, a, gam, lam)
    b.vec <- quad.lasso.fun(q.mat, l.vec, lam, b.vec, iter.max, eps)$b.vec # update
    g.vec <- grad.fun(y.vec, x.mat, b.vec)
    l <- c(l, loss.fun(y.vec, x.mat, b.vec))
    if (kkt.fun(g.vec + grad.S.fun(b.vec, a, gam, lam), b.vec, lam, eps)) break
    print(iter)
  }
  return(list(b.vec = b.vec, g.vec = g.vec + grad.S.fun(b.vec, a, gam, lam), par.vec = c(a, gam, lam),
              loss.vec = l))
}
test <- mcl.fun(y.vec, x.mat, b.vec, 2, .5, 1)

mcl.path.fun <- function(y.vec, x.mat, a, gam, lam.vec, iter.max = 1e3, eps = 1e-8) { # MCL with set control
  b.mat <- NULL ; b.vec <- rep(0,ncol(x.mat))
  g.vec <- grad.fun(y.vec, x.mat, b.vec)
  for (lam in lam.vec) {
    for (iter in 1:iter.max) {
      v.vec <- (abs(g.vec) >= (lam - eps)) | (b.vec != 0)
      ax.mat <- x.mat[, v.vec, drop = F] # preserve as matrix
      fit <- mcl.fun(y.vec, ax.mat, b.vec[v.vec], a, gam, lam)
      b.vec[v.vec] <- fit$b.vec
      g.vec <- grad.fun(y.vec, x.mat, b.vec)
      if (kkt.fun(g.vec + grad.S.fun(b.vec, a, gam, lam), b.vec, lam, eps)) break
    }
    b.mat <- cbind(b.mat, b.vec)
  }
  return(list(b.mat = b.mat, lam.vec = lam.vec))
}

mcl.path.fun(y.vec, x.mat, 2, 1/2, 1:3)