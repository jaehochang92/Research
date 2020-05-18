require(stargazer)
set.seed(2018)

# Exact MISE --------------------------------------------------------------

MISE <- function(h,n) {   # MISE(h;n)
  1/sqrt(4*pi)-sqrt(2)/sqrt(pi*(h^2+2))+
    sqrt(2)*(n-1)/(2*n*sqrt(pi*(2*h^2+2)))+1/(2*n*h*sqrt(pi))
}
n <- c(200,400,800)
hopt <- c() ; MISEv <- c()

for(sz in n){
  hopt <- append(hopt,optim(0.01, MISE, n=sz, control = list(abstol=1e-10),
                            method = 'Brent', lower = 0, upper = 10)$par)
  MISEv <- append(MISEv,MISE(
    optim(0.01, MISE, n=sz, control = list(abstol=1e-10),
          method = 'Brent', lower = 0, upper = 100)$par,sz))
}

df <- data.frame(n=c(200,400,800),h=round(hopt,3),
                 MISE=MISEv)
# stargazer(df, type = 'latex', summary = F, digits = 6)

# Simulation --------------------------------------------------------------

phi = function(x){exp(-x^2/2)/sqrt(2*pi)}
fh = function(x,X,h){
  v <- c()
  for(j in x){
    r <- phi((j-X)/h)/h
    v <- append(v,mean(r))
  }
  return(v)
}
f = function(x){(phi(x-1.5)+phi(x)+phi(x+1.5))/3}
num.intg <- function(f,l,u,itv){ # Numerical Integration
  grid <- seq(l,u,itv)
  sum((f(x=grid[-length(grid)])+f(x=grid[-1]))*itv/2)
}

mus <- c(-1.5,0,1.5)
sds <- sqrt(c(1,1,1))
tab <- c(0,0,0)
for(j in 1:3){
  tmp <- 0
  rep <- 500
  n <- df[j,1] ; h <- df[j,2]
  for(i in 1:rep){
    components <- sample(1:3,prob=c(1/3,1/3,1/3),size=n,replace=TRUE)
    samples <- rnorm(n=n,mean=mus[components],sd=sds[components])
    AMISE = num.intg(function(x){(fh(x,samples,h)-f(x))^2},-4.5,4.5,.01)
    tmp <- tmp + AMISE/rep
  }
  tab <- rbind(tab,c(n,h,tmp))
}
tab <- tab[-1,]
colnames(tab) <- c('n','h','MM')
# stargazer(tab, summary = F, digits = 6)