pos.fun <- function(x) x*I(x >= 0)

loss.fun = function(y.vec,x.mat,b.vec) {
  n <- length(y.vec)
  xb.vec = drop(x.mat %*% b.vec)
  ret = -sum(y.vec*xb.vec) + sum(exp(xb.vec))
  return(ret/n)
}
grad.fun = function(y.vec, x.mat, b.vec) {
  n <- length(y.vec)
  exb.vec = exp(drop(x.mat %*% b.vec))
  ret = crossprod(x.mat, -y.vec + exb.vec)
  return(ret/n)
}
hess.fun = function(y.vec, x.mat, b.vec) {
  n <- length(y.vec)
  exb.vec = exp(drop(x.mat %*% b.vec))
  ret = crossprod(x.mat, diag(exb.vec) %*% x.mat )
  return(ret/n)
}
kkt.fun <- function(g.vec, b.vec, lam, eps, prnt = F) {
  kkt1 = prod(abs(g.vec[b.vec != 0] + lam*sign(b.vec[b.vec != 0])) < eps)
  kkt2 = prod(abs(g.vec[b.vec == 0]) - lam < eps)
  return(kkt1 & kkt2)
}

# lasso -------------------------------------------------------------------


quad.lasso.fun = function(q.mat, l.vec, lam, b.vec, iter.max, eps) {
  for (iter in 1:iter.max) {
    grad = 2*q.mat %*% b.vec + l.vec
    if (kkt.fun(grad, b.vec, lam, eps)) break
    for (j in 1:length(b.vec)) {
      a = 2*q.mat[j,j] ; b = -2*sum(q.mat[j,-j]*b.vec[-j]) - l.vec[j]
      if (abs(b) < lam) { b.vec[j] = 0
      } else {b.vec[j] = sign(b)*(abs(b) - lam)/a}
    }
  }
  return(list(b.vec = b.vec, lam = lam))
}


lasso.fun <- function(y.vec, x.mat, b.vec, lam, iter.max, eps) {
  for (iter in 1:iter.max) {
    h.mat <- hess.fun(y.vec, x.mat, b.vec) ; g.vec <- grad.fun(y.vec, x.mat, b.vec)
    if (kkt.fun(g.vec, b.vec, lam, eps)) break
    q.mat <- h.mat/2 ; l.vec <- g.vec - drop(h.mat %*% b.vec)
    b.vec <- quad.lasso.fun(q.mat, l.vec, lam, b.vec, iter.max, eps)$b.vec # update
  }
  return(list(b.vec = b.vec, g.vec = g.vec, lam = lam))
}


# SCAD --------------------------------------------------------------------

grad.Jtd.fun <- function(b, a, lam) {
  t <- abs(b)
  if (t <= lam) {
    p <- lam
  } else {
    p <- pos.fun(a*lam - t)/(a - 1)
  }
  return(sign(b)*(p - lam))
}
grad.Jtd.fun <- Vectorize(grad.Jtd.fun, 'b')

scad.fun <- function(y.vec, x.mat, b.vec, a, lam, iter.max, eps) {
  for (iter in 1:iter.max) {
    h.mat <- hess.fun(y.vec, x.mat, b.vec) ; g.vec <- grad.fun(y.vec, x.mat, b.vec)
    if (kkt.fun(g.vec + grad.Jtd.fun(b.vec, a, lam), b.vec, lam, eps)) break
    q.mat <- h.mat/2 ; l.vec <- g.vec - drop(h.mat %*% b.vec) + grad.Jtd.fun(b.vec, a, lam)
    b.vec <- quad.lasso.fun(q.mat, l.vec, lam, b.vec, iter.max, eps)$b.vec # update
  }
  return(list(b.vec = b.vec, g.vec = g.vec + grad.Jtd.fun(b.vec, a, lam), lam = lam))
}


# MCL ---------------------------------------------------------------------

grad.J.fun <- Vectorize(function(t, a, gam, lam) (sign(t)*max(gam, lam - abs(t)/a)), 't')
# plot(function(t) grad.J.fun(t, 2, .5, 1), n = 1e3, -3, 3)

grad.S.fun <- function(t, a, gam, lam){grad.J.fun(t, a, gam, lam) - lam*sign(t)}
# plot(function(t) grad.S.fun(t, 2, .5, 1), n = 1e3, -3, 3)

mcl.fun <- function(y.vec, x.mat, b.vec, a, gam, lam, iter.max, eps) {
  for (iter in 1:iter.max) {
    h.mat <- hess.fun(y.vec, x.mat, b.vec);g.vec <- grad.fun(y.vec, x.mat, b.vec)
    if (kkt.fun(g.vec + grad.S.fun(b.vec, a, gam, lam), b.vec, lam, eps)) break
    q.mat <- h.mat/2 ; l.vec <- g.vec - drop(h.mat %*% b.vec) + grad.S.fun(b.vec, a, gam, lam)
    b.vec <- quad.lasso.fun(q.mat, l.vec, lam, b.vec, iter.max, eps)$b.vec
  }
  return(list(b.vec = b.vec, g.vec = g.vec + grad.S.fun(b.vec, a, gam, lam), lam = lam))
}


# lasso path --------------------------------------------------------------


lasso.path.fun <- function(y.vec, x.mat, lam.vec, iter.max = 1e3, eps = 1e-8) {

  b.mat <- NULL ; b.vec <- drop(solve(t(x.mat) %*% x.mat + diag(1, length(b.vec))) 
                                %*% t(x.mat) %*% y.vec)
  g.vec <- grad.fun(y.vec, x.mat, b.vec)
  l.vec <- c()
  for (lam in lam.vec) {
    for (iter in 1:iter.max) {
      if (kkt.fun(g.vec, b.vec, lam, eps)) break
      v.vec <- (abs(g.vec) >= (lam - eps)) | (b.vec != 0)
      ax.mat <- x.mat[, v.vec, drop = F] # preserve as matrix
      fit <- lasso.fun(y.vec, ax.mat, b.vec[v.vec], lam, iter.max, eps)
      b.vec[v.vec] <- fit$b.vec
      g.vec <- grad.fun(y.vec, x.mat, b.vec)
    }
    l.vec <- c(l.vec, loss.fun(y.vec, x.mat, b.vec))
    b.mat <- cbind(b.mat, b.vec)
  }
  return(list(b.mat = b.mat, lam.vec = lam.vec, l.vec = l.vec))
}

# SCAD path ---------------------------------------------------------------


scad.path.fun <- function(y.vec, x.mat, a, lam.vec, iter.max = 1e3, eps = 1e-8) {
  # SCAD with set control
  
  b.mat <- NULL ; b.vec <- drop(solve(t(x.mat) %*% x.mat + diag(1, length(b.vec))) 
                                %*% t(x.mat) %*% y.vec)
  g.vec <- grad.fun(y.vec, x.mat, b.vec)
  l.vec <- c()
  for (lam in lam.vec) {
    for (iter in 1:iter.max) {
      Jtd <- grad.Jtd.fun(b.vec, a, lam)
      if (kkt.fun(g.vec + Jtd, b.vec, lam, eps)) break
      v.vec <- (abs(g.vec + Jtd) >= (lam - eps)) | (b.vec != 0)
      ax.mat <- x.mat[, v.vec, drop = F] # preserve as matrix
      fit <- scad.fun(y.vec, ax.mat, b.vec[v.vec], a, lam, iter.max, eps)
      b.vec[v.vec] <- fit$b.vec
      g.vec <- grad.fun(y.vec, x.mat, b.vec)
    }
    l.vec <- c(l.vec, loss.fun(y.vec, x.mat, b.vec))
    b.mat <- cbind(b.mat, b.vec)
  }
  return(list(b.mat = b.mat, lam.vec = lam.vec, l.vec = l.vec))
}

# MCL path ----------------------------------------------------------------


mcl.path.fun <- function(y.vec, x.mat, a, gam, lam.vec, iter.max = 1e3, eps = 1e-8) { 
  # MCL with set control
  
  b.mat <- NULL ; b.vec <- drop(solve(t(x.mat) %*% x.mat + diag(gam, length(b.vec))) 
                                %*% t(x.mat) %*% y.vec)
  g.vec <- grad.fun(y.vec, x.mat, b.vec)
  l.vec <- c()
  for (lam in lam.vec) {
    for (iter in 1:iter.max) {
      gS <- grad.S.fun(b.vec, a, gam, lam)
      if (kkt.fun(g.vec + gS, b.vec, lam, eps)) break
      v.vec <- (abs(g.vec + gS) > (lam - eps)) | (b.vec != 0)
      ax.mat <- x.mat[, v.vec, drop = F] # preserve as matrix
      fit <- mcl.fun(y.vec, ax.mat, b.vec[v.vec], a, gam, lam, iter.max, eps)
      b.vec[v.vec] <- fit$b.vec
      g.vec <- grad.fun(y.vec, x.mat, b.vec)
    }
    l.vec <- c(l.vec, loss.fun(y.vec, x.mat, b.vec))
    b.mat <- cbind(b.mat, b.vec)
  }
  return(list(b.mat = b.mat, lam.vec = lam.vec, l.vec = l.vec))
}