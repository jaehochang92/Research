require(ggplot2) ; require(Rcpp)
rm(list = ls())
sourceCpp('~/Desktop/Statistics/NPDE/Rcpp/ASH.cpp')
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
set.seed(30)

# ASH
Rfx <- function(x, data, K = 20, m = 100){
  rg <- range(data)
  n <- length(data)
  # h <- (rg[2]-r[1]+2*eps)/K
  eps <- h <- (rg[2] - rg[1])/K
  sum <- 0
  for (i in 1:n) {
    for (k in 1:(K*m)) {
      for (l in (1 - m):(m - 1)) {
        if (k == 1) {
          sum %+=% ((1 - abs(l)/m)*((k - 1)*h/m - eps <= x & x < k*h/m) *
                      ((k + l - 1)*h/m <= data[i] & data[i] < (k + l)*h/m))
        }else if (k == K*m) {
          sum %+=% ((1 - abs(l)/m)*((k - 1)*h/m <= x & x < k*h/m) * 
                      ((k + l - 1)*h/m <= data[i] & data[i] < (k + l)*h/m + eps))
        }else {
          sum %+=% ((1 - abs(l)/m)*((k - 1)*h/m <= x & x < k*h/m) * 
                      ((k + l - 1)*h/m <= data[i] & data[i] < (k + l)*h/m)) 
        }
      }
    }
  }
  return(sum/(n*h))
}

n <-  200
M <- rmultinom(n,1,c(.2,.5,.3))
data <- M[1,]*rnorm(n) + M[2,]*rnorm(n,5) + M[3,]*rnorm(n,10)

range(data)
x <- sort(data + rnorm(n,0,0.01))
system.time(f <- Cfx(x, data = data, K = 40, m = 20))
system.time(f2 <- Rfx(x, data = data, K = 40, m = 20))
df <- data.frame(x = x, data = sort(data), f = f)
ggplot(df) +
  geom_histogram(aes(x = data,y= ..density..),colour = "black",fill = "cadetblue3",stat = 'bin')+
  geom_line(aes(x = x,y = f,lty = 'ASH'))