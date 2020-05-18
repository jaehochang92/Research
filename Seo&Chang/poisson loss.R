#poisson loss

loss.fun = function(y.vec,x.mat,b.vec){
  xb.vec = drop(x.mat%*%b.vec); ret = sum(y.vec*xb.vec-exp(xb.vec)-log(factorial(y.vec)))
  return(ret)
}
grad.fun = function(y.vec,x.mat,b.vec){
  exb.vec = exp(drop(x.mat%*%b.vec)); exb.vec = pmin(exb.vec,1e+07)
  ret = -drop(t(x.mat)%*%(y.vec-exb.vec))
  return(ret)
}
hess.fun = function(y.vec,x.mat,b.vec){
  exb.vec = exp(drop(x.mat%*%b.vec)); exb.vec = pmin(exb.vec,1e+07)
  ret = -t(x.mat)%*%diag(exb.vec)%*%x.mat 
  return(ret)
}
