rm(list = ls())

set.seed(1001)
n = 1e1
p = 1e2
x.mat = matrix(rnorm(n*p), ncol = p); b.vec = 1/(1:p)
exb.vec = exp(drop(x.mat %*% b.vec)); 
p.vec = exb.vec/(1 + exb.vec)
y.vec = rbinom(n, 1, prob = p.vec)

loss.fun = function(y.vec,x.mat,b.vec){
  xb.vec = drop(x.mat %*% b.vec)
  ret = -sum(y.vec*xb.vec) + sum(log(1 + exp(xb.vec)))  
  return(ret)
}

grad.fun = function(y.vec,x.mat,b.vec){
  exb.vec = exp(drop(x.mat %*% b.vec))
  p.vec = exb.vec/(1 + exb.vec)
  ret = -drop(t(x.mat) %*% (y.vec - p.vec))
  return(ret)
}

hess.fun = function(y.vec,x.mat,b.vec){
  exb.vec = exp(drop(x.mat %*% b.vec))
  p.vec = exb.vec/(1 + exb.vec)
  ret = t(x.mat) %*% diag(p.vec*(1 - p.vec)) %*% x.mat
  return(ret)
}

eps = 1e-10; iter.max = 100
b.vec = rep(0,p); lam = 1
for(iter in 1:iter.max){
  f.val = loss.fun(y.vec,x.mat,b.vec)+lam*sum(abs(b.vec))
  print(f.val)
  g.vec = grad.fun(y.vec,x.mat,b.vec)
  h.mat = hess.fun(y.vec,x.mat,b.vec)
  ocb.vec = cb.vec = b.vec 
  for(iiter in 1:iter.max){
    for(j in 1:p){
      a = h.mat[j,j]/2 
      b = sum(h.mat[j,-j]*cb.vec[-j])+g.vec[j]-sum(h.mat[j,]*b.vec)
      if(abs(b)<lam){ cb.vec[j] = 0 
      } else { cb.vec[j] = sign(-b)*(abs(b)-lam)/2/a } 
    }
    if(sum(abs(cb.vec-ocb.vec))<eps) break 
    ocb.vec = cb.vec 
  }
  nb.vec = cb.vec 
  if(sum(abs(b.vec-nb.vec))<eps) break 
  b.vec = nb.vec 
}