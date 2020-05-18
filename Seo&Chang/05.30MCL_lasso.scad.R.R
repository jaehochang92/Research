loss.fun = function(y.vec,x.mat,b.vec) {
  xb.vec = drop(x.mat %*% b.vec)
  # f <- Vectorize(function(x) ifelse(x == 0, 1, sum(log(1:x))))
  ret = -sum(y.vec*xb.vec) + sum(exp(xb.vec))
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

grad.scad.penalty.fun = function(b.vec,a,lam){ #gradient(scad penalty - lasso penalty)
  k.vec = rep(NA,length(b.vec))
  for(i in 1:length(b.vec)){
    b = abs(b.vec[i])
    if(b==0){k.vec[i] = 0}
    else if(b<=lam) {k.vec[i] = 0}
    else if(b>a*lam) {k.vec[i] = -lam*sign(b.vec[i])}
    else{k.vec[i] = ((a*lam-(b))/(a-1)-lam)*sign(b.vec[i])}
  }
  return(k.vec)
}

grad.J.fun <- function(t, a, gam, lam) (sign(t)*max(gam, lam - abs(t)/a))
# plot(Vectorize(function(t) grad.J.fun(t, 2, .5, 1)), n = 1e3, -3, 3)

grad.S.fun <- function(t, a, gam, lam){grad.J.fun(t, a, gam, lam) - lam*sign(t)}
grad.S.fun <- Vectorize(grad.S.fun, 't')
# plot(function(t) grad.S.fun(t, 2, .5, 1), n = 1e3, -3, 3)

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

### LASSO
lasso.fun <- function(y.vec, x.mat, b.vec, iter.max, eps){
  l <- c()
  for (iter in 1:iter.max) {
    h.mat <- hess.fun(y.vec, x.mat, b.vec); g.vec <- grad.fun(y.vec, x.mat, b.vec)
    q.mat <- h.mat/2 ; l.vec <- g.vec - drop(h.mat %*% b.vec)
    b.vec <- quad.lasso.fun(q.mat, l.vec, lam, b.vec, iter.max, eps)$b.vec
    g.vec <- grad.fun(y.vec, x.mat, b.vec)
    l <- c(l, loss.fun(y.vec, x.mat, b.vec))
    if (kkt.fun(g.vec, b.vec, lam, eps)) break
  }
  return(list(b.vec = b.vec, g.vec = g.vec, lam = lam, loss.vec = l))
}

lasso.path.fun <- function(y.vec, x.mat, a, gam, lam.vec, iter.max = 1e3, eps = 1e-6) {
  b.mat <- NULL ; b.vec <- rep(0,ncol(x.mat))
  g.vec <- grad.fun(y.vec, x.mat, b.vec)
  l.vec <- c()
  for (lam in lam.vec) {
    for (iter in 1:iter.max) {
      v.vec <- (abs(g.vec) >= (lam - eps)) | (b.vec != 0)
      ax.mat <- x.mat[, v.vec, drop = F] # preserve as matrix
      fit <- lasso.fun(y.vec, ax.mat, b.vec[v.vec], lam, iter.max, eps)
      b.vec[v.vec] <- fit$b.vec
      g.vec <- grad.fun(y.vec, x.mat, b.vec)
      if (kkt.fun(g.vec, b.vec, lam, eps)) 
        break
    }
    l.vec <- c(l.vec, loss.fun(y.vec, x.mat, b.vec))
    b.mat <- cbind(b.mat, b.vec)
  }
  return(list(b.mat = b.mat, lam.vec = lam.vec, l.vec = l.vec))
}

### SCAD
scad.fun <- function(y.vec, x.mat, b.vec, a, lam, iter.max, eps){
  l <- c()
  for (iter in 1:iter.max) {
    h.mat <- hess.fun(y.vec, x.mat, b.vec); g.vec <- grad.fun(y.vec, x.mat, b.vec)
    q.mat <- h.mat/2
    l.vec <- g.vec-drop(h.mat%*%b.vec)+grad.scad.penalty.fun(b.vec,a,lam)
    b.vec <- quad.lasso.fun(q.mat, l.vec, lam, b.vec, iter.max, eps)$b.vec
    g.vec <- grad.fun(y.vec, x.mat, b.vec)
    l <- c(l, loss.fun(y.vec, x.mat, b.vec))
    if (kkt.fun(g.vec + grad.scad.penalty.fun(b.vec,a,lam), b.vec, lam, eps)) break
  }
  return(list(b.vec = b.vec, g.vec = g.vec+grad.scad.penalty.fun(b.vec,a,lam),
              par.vec=c(a,lam),loss.vec=l))
}

scad.path.fun <- function(y.vec, x.mat, a, lam.vec, iter.max = 1e3, eps = 1e-6) { 
  b.mat <- NULL ; b.vec <- rep(0,ncol(x.mat))
  g.vec <- grad.fun(y.vec, x.mat, b.vec)
  l.vec <- c()
  for (lam in lam.vec) {
    for (iter in 1:iter.max) {
      v.vec <- (abs(g.vec) >= (lam - eps)) | (b.vec != 0)
      ax.mat <- x.mat[, v.vec, drop = F] # preserve as matrix
      fit <- scad.fun(y.vec, ax.mat, b.vec[v.vec], a, lam, iter.max, eps)
      b.vec[v.vec] <- fit$b.vec
      g.vec <- grad.fun(y.vec, x.mat, b.vec)
      if (kkt.fun(g.vec + grad.scad.penalty.fun(b.vec, a, lam), b.vec, lam, eps)) 
        break
    }
    l.vec <- c(l.vec, loss.fun(y.vec, x.mat, b.vec))
    b.mat <- cbind(b.mat, b.vec)
  }
  return(list(b.mat = b.mat, lam.vec = lam.vec, l.vec = l.vec))
}

### MCL
mcl.fun <- function(y.vec, x.mat, b.vec, a, gam, lam, iter.max, eps) {
  l <- c()
  for (iter in 1:iter.max) {
    h.mat <- hess.fun(y.vec, x.mat, b.vec); g.vec <- grad.fun(y.vec, x.mat, b.vec)
    q.mat <- h.mat/2 
    l.vec <- g.vec - drop(h.mat %*% b.vec) + grad.S.fun(b.vec, a, gam, lam)
    b.vec <- quad.lasso.fun(q.mat, l.vec, lam, b.vec, iter.max, eps)$b.vec
    g.vec <- grad.fun(y.vec, x.mat, b.vec)
    l <- c(l, loss.fun(y.vec, x.mat, b.vec))
    if (kkt.fun(g.vec + grad.S.fun(b.vec, a, gam, lam), b.vec, lam, eps)) break
  }
  return(list(b.vec = b.vec, g.vec = g.vec + grad.S.fun(b.vec, a, gam, lam), 
              par.vec = c(a, gam, lam), loss.vec = l))
}

mcl.path.fun <- function(y.vec, x.mat, a, gam, lam.vec, iter.max = 1e3, 
                         eps = 1e-6) { # MCL with set control
  b.mat <- NULL ; b.vec <- rep(0,ncol(x.mat))
  g.vec <- grad.fun(y.vec, x.mat, b.vec)
  l.vec <- c()
  for (lam in lam.vec) {
    for (iter in 1:iter.max) {
      v.vec <- (abs(g.vec) >= (lam - eps)) | (b.vec != 0)
      ax.mat <- x.mat[, v.vec, drop = F] # preserve as matrix
      fit <- mcl.fun(y.vec, ax.mat, b.vec[v.vec], a, gam, lam, iter.max, eps)
      b.vec[v.vec] <- fit$b.vec
      g.vec <- grad.fun(y.vec, x.mat, b.vec)
      if (kkt.fun(g.vec + grad.S.fun(b.vec, a, gam, lam), b.vec, lam, eps)) 
        break
    }
    l.vec <- c(l.vec, loss.fun(y.vec, x.mat, b.vec))
    b.mat <- cbind(b.mat, b.vec)
  }
  return(list(b.mat = b.mat, lam.vec = lam.vec, l.vec = l.vec))
}
