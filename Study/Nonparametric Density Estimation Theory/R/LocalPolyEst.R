rm(list=ls())
par(ask=F,mfrow=c(2,2))
require(dplyr) ; require(locpol)

X <- function(x,Xm = Xm0, h = h0) {cbind(1,(Xm - x)/h)}
Kh <- function(x,Xm = Xm0, h = h0, opt = 'ep') {
  t = (Xm - x)/h # Here!
  if (opt == 'ep') {
    a <- 3/4*(1-t^2)*(abs(t)<=1)
  } else if (opt == 'norm') {
    a <- exp(-t^2)/sqrt(2*pi)
  } else if (opt == 'un') {
    a <- (abs(t)<=1)/2
  } else {print('no matched kernel')}
  return(a)
}
Wh <- function(x,Xm,h=h0) {diag(Kh(x,Xm,h))}

for (hi in c(.1,.3,.5,.7)) {
  set.seed(1001)
  n <- 200
  h0 <- hi
  Y <- (seq(0,5,length.out = n)-2)^2+2 + rnorm(n,0,1)
  Xm0 <- seq(0,5,length.out = n)
  
  xgrd <- seq(min(Xm0)+.1,max(Xm0)-.1,length.out = n) ; pred.y <- c()
  plot(Xm0,Y,main=paste('h = ',hi,sep = ''))
  
  pred.y <- c()
  for(i in 1:length(xgrd)) {
    x0 <- xgrd[i]
    b <- chol2inv(chol(t(X(x0,Xm0))%*%Wh(x0,Xm0)%*%X(x0,Xm0)))%*%t(X(x0,Xm0))%*%Wh(x0,Xm0)%*%Y
    pred.y <- append(pred.y,b[1])
  }
  lines(xgrd,pred.y)
  
  locp <- locpol(Y~Xm0,data = as.data.frame(cbind(Y,Xm0)))
  abline(lm(Y~Xm0),lty=2)
  lines(locp$lpFit[,1:2],lty=4)
  legend('topleft',lty=c(1,2,4),
         legend = c('hands-on','lm','locpol'),cex = .8)
}