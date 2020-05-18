rm(list=ls())

# Local Polynomial Modeling and its Applications --------------------------
# J.Fan and I.Gijbels -----------------------------------------------------

# Chapter 3. Framework for Local Polynomial Regression --------------------

# Fig 3.1 -----------------------------------------------------------------

epaK <- function(u){(1>u^2)*3/4*(1-u^2)}

mu <- function(r,s,Kern=epaK,a=-Inf,b=Inf){
  integrate(function(u){u^r*Kern(u)^s},a,b)$value
}

mu <- Vectorize(mu,'r')
mu <- Vectorize(mu,'s')

S <- function(c,s,p,Kern=epaK){
  mat <- matrix(rep(0,(p+1)^2),nrow = p+1)
  for(i in 0:p){for(j in 0:p){
    mat[i+1,j+1] <- mu(i+j+c,s,Kern)
  }}
  return(mat)
}

Kst <- function(t,nu,p,Kern=epaK){
  e <- matrix(c(rep(0,nu),1,rep(0,p-nu)),ncol = 1)
  tv <- matrix(rep(t,p+1)^(0:p),ncol = 1)
  return(c(t(e)%*%solve(S(0,1,p))%*%tv*Kern(t)))
}
Kst <- Vectorize(Kst,'t')

setEPS()
postscript('Fig-3-1.eps',height = 10)
par(mfrow=c(3,2))
plot(function(u)epaK(u),-1,1) ; title('Optimal weighting scheme')
plot(function(u)Kst(u,0,3),-1,1) ; abline(h=0) ; title(expression(paste(nu,'=',0,', ',p,'=',3)))
plot(function(u)Kst(u,0,5),-1,1) ; abline(h=0) ; title(expression(paste(nu,'=',0,', ',p,'=',5)))
plot(function(u)Kst(u,1,2),-1,1) ; abline(h=0) ; title(expression(paste(nu,'=',1,', ',p,'=',2)))
plot(function(u)Kst(u,1,4),-1,1) ; abline(h=0) ; title(expression(paste(nu,'=',1,', ',p,'=',4)))
plot(function(u)Kst(u,2,3),-1,1) ; abline(h=0) ; title(expression(paste(nu,'=',2,', ',p,'=',3)))
dev.off()


# Fig 3.2 -----------------------------------------------------------------

Sc <- function(c,p){
  mat <- matrix(rep(0,(p+1)^2),nrow = p+1)
  for(i in 0:p){for(j in 0:p){
    mat[i+1,j+1] <- mu(r = i+j,s = 1,a = -c)
  }}
  return(mat)
}

BdKst <- function(t,nu,p,c,Kern=epaK){
  e <- matrix(c(rep(0,nu),1,rep(0,p-nu)),ncol = 1)
  tv <- matrix(rep(t,p+1)^(0:p),ncol = 1)
  return(c(t(e)%*%solve(Sc(c,p))%*%tv*Kern(t)))
}
BdKst <- Vectorize(BdKst,'t')

setEPS()
postscript('Fig-3-2.eps',height = 10)
par(mfrow=c(3,2))
plot(function(u)BdKst(u,0,1,.3),-1,1,lty=1) ; abline(h=0) ; title(expression(paste(nu,'=',0,', ',p,'=',1)))
plot(function(u)BdKst(u,0,1,.7),-1,1,lty=3,add=T)
plot(function(u)BdKst(u,0,1,1),-1,1,lty=2,add=T)
plot(function(u)BdKst(u,0,3,.3),-1,1,lty=1) ; abline(h=0) ; title(expression(paste(nu,'=',0,', ',p,'=',3)))
plot(function(u)BdKst(u,0,3,.7),-1,1,lty=3,add=T)
plot(function(u)BdKst(u,0,3,1),-1,1,lty=2,add=T)
plot(function(u)BdKst(u,0,5,.3),-1,1,lty=1) ; abline(h=0) ; title(expression(paste(nu,'=',0,', ',p,'=',5)))
plot(function(u)BdKst(u,0,5,.7),-1,1,lty=3,add=T)
plot(function(u)BdKst(u,0,5,1),-1,1,lty=2,add=T)
plot(function(u)BdKst(u,1,2,.3),-1,1,lty=1) ; abline(h=0) ; title(expression(paste(nu,'=',1,', ',p,'=',2)))
plot(function(u)BdKst(u,1,2,.7),-1,1,lty=3,add=T)
plot(function(u)BdKst(u,1,2,1),-1,1,lty=2,add=T)
plot(function(u)BdKst(u,1,4,.3),-1,1,lty=1) ; abline(h=0) ; title(expression(paste(nu,'=',1,', ',p,'=',4)))
plot(function(u)BdKst(u,1,4,.7),-1,1,lty=3,add=T)
plot(function(u)BdKst(u,1,4,1),-1,1,lty=2,add=T)
plot(function(u)BdKst(u,2,3,.3),-1,1,lty=1) ; abline(h=0) ; title(expression(paste(nu,'=',2,', ',p,'=',3)))
plot(function(u)BdKst(u,2,3,.7),-1,1,lty=3,add=T)
plot(function(u)BdKst(u,2,3,1),-1,1,lty=2,add=T)
dev.off()


# Fig 3.3 -----------------------------------------------------------------

GaussK <- function(u){exp(-u^2/2)/sqrt(2*pi)}
UnifK <- function(u){u^2<.25}

b <- function(c,Kern){(mu(2,1,Kern,-c)^2-mu(1,1,Kern,-c)*mu(3,1,Kern,-c))/(mu(2,1,Kern,-c)*mu(0,1,Kern,-c)-mu(1,1,Kern,-c)^2)}
v <- function(c,Kern){(integrate(function(u)(mu(2,1,Kern,-c)-u*mu(1,1,Kern,-c))^2*Kern(u)^2,-c,Inf)$value)/(mu(2,1,Kern,-c)*mu(0,1,Kern,-c)-mu(1,1,Kern,-c)^2)^2}
b <- Vectorize(b,'c')
v <- Vectorize(v,'c')

setEPS()
postscript('Fig-3-3.eps',height = 10)
par(mfrow=c(3,2),mar=c(2,5,4,2))
plot(function(c)b(c,GaussK)^2,0,5, ylab = expression(paste(b^2,'(c)',sep = ''))) ; title('Gaussian Kernel')
plot(function(c)v(c,GaussK),0,5, ylab = expression(v(c))) ; title('Gaussian Kernel')
plot(function(c)b(c,epaK)^2,0,1.5, ylab = expression(paste(b^2,'(c)',sep = ''))) ; title('Epanechnikov Kernel')
plot(function(c)v(c,epaK),0,1.5, ylab = expression(v(c))) ; title('Epanechnikov Kernel')
plot(function(c)b(c,UnifK)^2,0,.6, ylab = expression(paste(b^2,'(c)',sep = ''))) ; title('Uniform Kernel')
plot(function(c)v(c,UnifK),0,.6, ylab = expression(v(c))) ; title('Uniform Kernel')
dev.off()


# Fig 3.4 -----------------------------------------------------------------

BetafK <- function(t,gam){(1-t^2)^gam/beta(.5,gam+1)*(1>t^2)}

V <- function(p,Kern){
  mat <- solve(S(0,1,p,Kern))%*%S(0,2,p,Kern)%*%solve(S(0,1,p,Kern))
  return(c(mat[1,1]))
}
V <- Vectorize(V,'p')

df <- data.frame(p=1:10,Gaussian=rep(0,10),Uniform=rep(0,10),Epanechnikov=rep(0,10),Biweight=rep(0,10),Triweight=rep(0,10))
df[,2] <- V(1:10,GaussK)/V(0,GaussK)
df[,3] <- V(1:10,function(u)BetafK(u,0))/V(0,function(u)BetafK(u,0))
df[,4] <- V(1:10,function(u)BetafK(u,1))/V(0,function(u)BetafK(u,1))
df[,5] <- V(1:10,function(u)BetafK(u,2))/V(0,function(u)BetafK(u,2))
df[,6] <- V(1:10,function(u)BetafK(u,3))/V(0,function(u)BetafK(u,3))

setEPS()
postscript('Fig-3-4.eps',height = 8,width = 8)
par(mfrow=c(1,1))
plot(rep(0,5),df[1,-1],xlim = c(0,10),ylim = c(1,8),pch=1:5,xlab = 'order',ylab = 'variance increase')
for(pi in 1:10){
  points(rep(pi,5),df[pi,-1],pch=1:5,add=T)
}
for(c in 2:6){
  lines(df[,c(1,c)],lty=c-1)
}
legend('topleft',pch=1:5,legend = c('Gaussian','Uniform','Epanechnikov','Biweight','Triweight'))
dev.off()


# Figure 3.5 --------------------------------------------------------------

# Design ------------------------------------------------------------------

require(tictoc) ; require(dplyr)
rm(list=ls())

mxgen <- function(nr,nc,FUN){
  ans <- as.matrix(outer(1:nr,1:nc,'FUN'))
  return(ans)
}

# functions ---------------------------------------------------------------

Kh <- function(u,h){3/4*(1 - (u/h)^2)*(1 >= (u/h)^2)/h}
sample.n <- 200

estm <- function(h,p,estm.Y,estm.X,estm.x0){
  bf.X <- mxgen(sample.n, p + 1, function(i, j, X.tmp = estm.X, x0.tmp = estm.x0){(X.tmp[i] - x0.tmp)^(j - 1)})
  W <- diag(c(Kh(estm.X - estm.x0,h)))
  hatbeta <- chol2inv(chol(t(bf.X) %*% W %*% bf.X)) %*% (t(bf.X) %*% W %*% estm.Y)
  return(hatbeta)
}
estm.v <- Vectorize(estm,'estm.x0')

# MSE ---------------------------------------------------------------------

MSE <- function(MSE.x0, MSE.h, MSE.nu, MSE.p,
                MSE.X, MSE.Y, a=2){
  # MSE.x0 <- 0 ; MSE.h <- .3 ; MSE.nu <- 0 ; MSE.p <- 3 ; MSE.X <- X ; MSE.Y <- Y
  
  # functions
  MSE.Kh <- function(u){Kh(u,h = MSE.h)}
  
  Snj <- function(j) sum(MSE.Kh(MSE.X - MSE.x0) * (MSE.X - MSE.x0)^j)
  Snj <- Vectorize(Snj,'j')
  # ---
  bf.X <- mxgen(sample.n, MSE.p+1, function(i, j, X.tmp = MSE.X, x0.tmp = MSE.x0){(X.tmp[i]-x0.tmp)^(j-1)})
  W <- diag(c(MSE.Kh(MSE.X-MSE.x0)))
  Sn <- t(bf.X) %*% W %*% bf.X
  beta_a <- estm(MSE.h,MSE.p+a,MSE.Y,MSE.X,MSE.x0)
  bias <- chol2inv(chol(Sn)) %*% 
    mxgen(MSE.p+1,a,function(i,j){Snj(MSE.p+i+j-1) * (MSE.p>=2) *!((MSE.p+i+j-1) %in% (MSE.p+a+1):(MSE.p+a+MSE.p))}) %*% 
    beta_a[(MSE.p+2):(MSE.p+a+1),1]
  bnu <- bias[MSE.nu + 1]
  hY <- estm.v(MSE.h,MSE.p,MSE.Y,MSE.X,MSE.X)[1,]
  
  bf.X_a <- mxgen(sample.n, MSE.p+a+1, function(i,j,X.tmp=MSE.X,x0.tmp=MSE.x0){(X.tmp[i]-x0.tmp)^(j-1)})
  sgsq <- sum((MSE.Y - hY)^2*MSE.Kh(MSE.X - MSE.x0)) /
    sum(diag(W - W %*% bf.X_a %*% chol2inv(chol(t(bf.X_a) %*% W %*% bf.X_a)) %*% t(bf.X_a) %*% W))
  Vnu <- (chol2inv(chol(Sn)) %*% (t(bf.X) %*% W^2 %*% bf.X) %*% chol2inv(chol(Sn)))[MSE.nu+1,MSE.nu+1] * sgsq
  return(bnu^2 + Vnu)
}
MSE <- Vectorize(MSE,'MSE.x0')
MSE <- Vectorize(MSE,'MSE.p')
test <- MSE(seq(-.5,.5,length.out = 21),.2,0,1:4,X,Y)
apply(test, 1, which.min)

# order selection ---------------------------------------------------------


nu0 <- 0

order_selector <- function(os.xj,os.R,os.h,os.X,os.Y,os.nu=nu0){
  tmp <- MSE(os.xj,os.h,os.nu,(os.nu + 1):os.R,os.X,os.Y)
  return(apply(tmp, 1, which.min))
}

setEPS()
postscript('Fig-3-5(400).eps',height = 10)
layout(matrix(c(1,1,2:5), 3, 2, byrow = TRUE))

runif(sample.n,-2,2) %>% sort() -> X
m <- function(x){x + 2*exp(-16*x^2)}
Y <- m(X) + rnorm(sample.n,0,.5)
X <- as.matrix(X) ; Y <- as.matrix(Y)

plot(X,Y, pch = 16, cex = .4)
plot(function(u)m(u),-2,2,add = T)
x.grd <- -1.8 + .036*(0:100)
hv <- c(.2,.3,.4,.6)
for(j in 1:4){
  orders <- order_selector(x.grd,3,hv[j],X,Y)
  hY <- x.grd
  for(i in 1:length(hY)){
    hY[i] <- estm(hv[j],orders[i],Y,X,x.grd[i])[1]
  };lines(x.grd,hY,lty=j+1)
  print(j/4*100)
} ; legend('topleft',legend = c('true',paste('h=',hv,sep = '')),lty = 1:5)

# MADEr --------------------------------------------------------------------

# sim.size <- 400
# MADE <- function(a,b){abs(a-b)/sim.size}
# 
# for (h0 in hv) {
#   MADE_adp <- MADE_f_lcs <- MADE_f_lln <- MADE_f_lqd <- MADE_f_lcb <- rep(0,length(x.grd))
#   simi <- 1
#   layout(matrix(c(1,2), ncol = 2))
# 
#   while(simi < sim.size){
#     tic()
#     X <- runif(sample.n,-2,2)
#     Y <- m(X) + rnorm(sample.n,0,.5)
#     X <- as.matrix(X) ; Y <- as.matrix(Y)
# 
#     possibleError <- tryCatch({
#       MADE_f_lcs <- MADE_f_lcs + MADE(estm.v(h = h0,p = 0,Y,X,x.grd),m(x.grd))
#       MADE_f_lln <- MADE_f_lln + MADE(estm.v(h = h0,p = 1,Y,X,x.grd)[1,],m(x.grd))
#       MADE_f_lqd <- MADE_f_lqd + MADE(estm.v(h = h0,p = 2,Y,X,x.grd)[1,],m(x.grd))
#       MADE_f_lcb <- MADE_f_lcb + MADE(estm.v(h = h0,p = 3,Y,X,x.grd)[1,],m(x.grd))
# 
#       orders <- order_selector(xj = x.grd,h = h0,R = 3,X,Y)
#       hY <- x.grd
#       for(i in 1:length(x.grd)){hY[i] <- estm(h0,orders[i],Y,X,x.grd[i])[1]}
#       MADE_adp <- MADE_adp + MADE(hY,m(x.grd))
#       
#       plot(x.grd,MADE_f_lcs/MADE_adp,type = 'l',lty = 1,
#            main = paste('Ratio of MADE for h=.',h0,sep = ''),ylim = c(0,3))
#       lines(x.grd,MADE_f_lln/MADE_adp,lty = 2)
#       lines(x.grd,MADE_f_lqd/MADE_adp,lty = 3)
#       lines(x.grd,MADE_f_lcb/MADE_adp,lty = 4)
#       abline(h=1)
#       
#       simi <- simi + 1
#       print(simi/sim.size*100);toc()
#     }, error = function(e) {print(e)}
#     )
#     if(inherits(possibleError, "error")) {next}
#   }
#   l <- list(MADE_adp=MADE_adp,MADE_f_lcs=MADE_f_lcs,MADE_f_lln=MADE_f_lln,
#             MADE_f_lqd=MADE_f_lqd,MADE_f_lcb=MADE_f_lcb)
#   write.csv(l,paste('MADEr_h0',h0*10,'times',sim.size,'.csv',sep = ''))
# }

for (hi in c(2,3,4,6)) {
  l <- read.csv(paste('Desktop/LaTex/LocPol/MADEr_h0',hi,'times400.csv',sep = ''))
  plot(x.grd,l$MADE_f_lcs/l$MADE_adp,type = 'l',lty = 1,
       main = paste('Ratio of MADE for h=.',hi,sep = ''),ylim = c(0,3))
  lines(x.grd,l$MADE_f_lln/l$MADE_adp,lty = 2)
  lines(x.grd,l$MADE_f_lqd/l$MADE_adp,lty = 3)
  lines(x.grd,l$MADE_f_lcb/l$MADE_adp,lty = 4)
  legend('topleft',lty=1:4,legend = c('const./adp.','linear/adp.','quad./adp.','cub./adp.'))
  abline(h=1)
}

dev.off()