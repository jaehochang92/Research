rm(list = ls())
p <- 1e3
n <- 5*1e3
y <- sample(0:1,n,replace = T)
X <- matrix(rnorm(n*p),ncol = p)
fit <- glm(y~X,family = binomial())
glmcoef <- coef(fit)
G <- function(b.vec,y.vec,X.mat){
  b.vec <- as.matrix(b.vec)
  y.vec <- as.matrix(y.vec)
  X.mat <- as.matrix(cbind(1,X.mat))
  t(X.mat) %*% (1/(1 + exp(-X.mat %*% b.vec)) - y.vec)
}
H <- function(b.vec,y.vec,X.mat){
  b.vec <- as.matrix(b.vec)
  y.vec <- as.matrix(y.vec)
  X.mat <- as.matrix(cbind(1,X.mat))
  res <- matrix(rep(0,length(b.vec)^2),nrow = length(b.vec))
  for (i in 1:nrow(X.mat)) {
    res <- res + c(exp(-X.mat[i,] %*% b.vec)/(1 + exp(-X.mat[i,] %*% b.vec))^2)*outer(X.mat[i,],X.mat[i,])
  }
  return(res)
}
NR <- function(y.vec,X.mat,init,threshold = 1e-12){
  init <- as.matrix(init)
  while (sqrt(sum(G(init,y.vec,X.mat)^2)) >= threshold) {
    init <- init - chol2inv(chol(H(init,y.vec,X.mat))) %*% G(init,y.vec,X.mat)
  }
  return(init)
}
print('NR') ; system.time(NRcoef <- NR(y,X,rep(0,1 + p)))
print('glm') ; system.time(coef(glm(y~X,family = binomial())))
max(abs(c(NRcoef) - glmcoef))