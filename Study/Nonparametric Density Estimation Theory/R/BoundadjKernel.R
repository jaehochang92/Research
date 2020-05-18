rm(list=ls())
#############################################
n = 200
h = 0.1
############################################
iter = 1000
x1.mat = matrix(rep(0,iter*99),iter,99)
A.mat = matrix(rep(0,iter*99),iter,99)
for(z in 1:iter){
#data
data = rbeta(n,0.5,0.5)
d = density(data,from = 0.01, to = 0.99 , n = 99 )
x1.mat[z,] = d$y
C.function = function(x){
  q = 0
  for(i in 1: n){
    q = q+3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
  }
  return(q/(n*h))
}


L.function = function(x){
  a = x/h
  u0 = 3/4*a-1/4*(a^3)+1/2
  u1 = 3/8*(a^2)-3/16*(a^4)-3/16
  u2 = 1/4*(a^3)-3/20*(a^5)-1/10
  p = 0
  for(i in 1:n){
    p = p + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
  }
  return(p/(n*h))
}

R.function = function(x){
  a = (1-x)/h
  u0 = 3/4*a-1/4*(a^3)+1/2
  u1 = 3/16*(a^4)-3/8*(a^2)+3/16 
  u2 = 1/4*(a^3)-3/20*(a^5)+1/10
  q = 0
  for(i in 1:n){
    q = q + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
  }
  return(q/(n*h))
}
A.mat[z,] = c((L.function(seq(0.01,0.09,0.01))),C.function(seq(0.1,0.9,0.01)),((R.function(seq(0.91,0.99,0.01)))))
}

dy = colMeans(x1.mat)
dx = d$x
f.hat = colMeans(A.mat)

plot(dx,dy,type="l",xlim = c(-0,1),ylim = c(0,4))
lines(c(0,dx,1),c(0,f.hat,0),col="red")

a=integrate(L.function,0,0.1)
b=integrate(C.function,0.1,0.9)
c=integrate(R.function,0.9,1)
a$value+b$value+c$value
######################################################################################################
############------------22222222222222222222222222222222222222222222--------------####################
######################################################################################################
rm(list=ls())
#############################################
n = 200
h = 0.1
############################################
iter = 1000
x1.mat = matrix(rep(0,iter*99),iter,99)
A.mat = matrix(rep(0,iter*99),iter,99)
for(z in 1:iter){
  #data
  data = rbeta(n,2,2)
  d = density(data,from = 0.01, to = 0.99 , n = 99 )
  x1.mat[z,] = d$y
  C.function = function(x){
    q = 0
    for(i in 1: n){
      q = q+3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  
  
  L.function = function(x){
    a = x/h
    u0 = 3/4*a-1/4*(a^3)+1/2
    u1 = 3/8*(a^2)-3/16*(a^4)-3/16
    u2 = 1/4*(a^3)-3/20*(a^5)-1/10
    p = 0
    for(i in 1:n){
      p = p + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(p/(n*h))
  }
  
  R.function = function(x){
    a = (1-x)/h
    u0 = 3/4*a-1/4*(a^3)+1/2
    u1 = 3/16*(a^4)-3/8*(a^2)+3/16 
    u2 = 1/4*(a^3)-3/20*(a^5)+1/10
    q = 0
    for(i in 1:n){
      q = q + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  A.mat[z,] = c((L.function(seq(0.01,0.09,0.01))),C.function(seq(0.1,0.9,0.01)),((R.function(seq(0.91,0.99,0.01)))))
}

dy = colMeans(x1.mat)
dx = d$x
f.hat = colMeans(A.mat)

plot(dx,dy,type="l",xlim = c(-0,1),ylim = c(0,4))
lines(c(0,dx,1),c(0,f.hat,0),col="red")
######################################################################################################
############------------33333333333333333333333333333333333333333333--------------####################
######################################################################################################
rm(list=ls())
#############################################
n = 200
h = 0.1
############################################
iter = 1000
x1.mat = matrix(rep(0,iter*99),iter,99)
A.mat = matrix(rep(0,iter*99),iter,99)
for(z in 1:iter){
  #data
  data = rbeta(n,2,5)
  d = density(data,from = 0.01, to = 0.99 , n = 99 )
  x1.mat[z,] = d$y
  C.function = function(x){
    q = 0
    for(i in 1: n){
      q = q+3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  
  
  L.function = function(x){
    a = x/h
    u0 = 3/4*a-1/4*(a^3)+1/2
    u1 = 3/8*(a^2)-3/16*(a^4)-3/16
    u2 = 1/4*(a^3)-3/20*(a^5)-1/10
    p = 0
    for(i in 1:n){
      p = p + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(p/(n*h))
  }
  
  R.function = function(x){
    a = (1-x)/h
    u0 = 3/4*a-1/4*(a^3)+1/2
    u1 = 3/16*(a^4)-3/8*(a^2)+3/16 
    u2 = 1/4*(a^3)-3/20*(a^5)+1/10
    q = 0
    for(i in 1:n){
      q = q + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  A.mat[z,] = c((L.function(seq(0.01,0.09,0.01))),C.function(seq(0.1,0.9,0.01)),((R.function(seq(0.91,0.99,0.01)))))
}

dy = colMeans(x1.mat)
dx = d$x
f.hat = colMeans(A.mat)

plot(dx,dy,type="l",xlim = c(-0,1),ylim = c(0,4))
lines(c(0,dx,1),c(0,f.hat,0),col="red")
######################################################################################################
############------------44444444444444444444444444444444444444444444--------------####################
######################################################################################################
rm(list=ls())
#############################################
n = 200
h = 0.05
#############################################
iter = 1000
x1.mat = matrix(rep(0,iter*99),iter,99)
A.mat = matrix(rep(0,iter*99),iter,99)
for(z in 1:iter){
  #data
  data = rbeta(n,0.5,0.5)
  d = density(data,from = 0.01, to = 0.99 , n = 99 )
  x1.mat[z,] = d$y
  #################################################################################
  C.function= function(x){
    q = 0
    for(i in 1: n){
      q = q+3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  
  L.function = function(x){
    a = x/h
    u0 = 3/4*a-1/4*(a^3)+1/2
    u1 = 3/8*(a^2)-3/16*(a^4)-3/16
    u2 = 1/4*(a^3)-3/20*(a^5)-1/10
    p = 0
    for(i in 1:n){
      p = p + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(p/(n*h))
  }
  
  R.function = function(x){
    a = (1-x)/h
    u0 = +3/4*a-1/4*(a^3)+1/2
    u1 = 3/16*(a^4)-3/8*(a^2)+3/16 
    u2 = 1/4*(a^3)-3/20*(a^5)+1/10
    q = 0
    for(i in 1:n){
      q = q + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  A.mat[z,] = c(L.function(c(0.01,0.02,0.03,0.04,0.049)),C.function(seq(0.06,0.94,0.01)),R.function(c(0.95,0.96,0.97,0.98,0.99)))
  
}

dy = colMeans(x1.mat)
dx=d$x
f.hat = colMeans(A.mat)
plot(dx,dy,type="l",xlim = c(-0,1),ylim = c(0,3))
lines(c(0,dx,1),c(0,f.hat,0),col="red")
a=integrate(L.function,0,0.05)
b=integrate(C.function,0.05,0.95)
c=integrate(R.function,0.95,1)
a$value+b$value+c$value
######################################################################################################
############------------55555555555555555555555555555555555555555555--------------####################
######################################################################################################
rm(list=ls())
#############################################
n = 200
h = 0.05
#############################################
iter = 1000
x1.mat = matrix(rep(0,iter*99),iter,99)
A.mat = matrix(rep(0,iter*99),iter,99)
for(z in 1:iter){
  #data
  data = rbeta(n,2,2)
  d = density(data,from = 0.01, to = 0.99 , n = 99 )
  x1.mat[z,] = d$y
  #################################################################################
  C.function= function(x){
    q = 0
    for(i in 1: n){
      q = q+3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  
  L.function = function(x){
    a = x/h
    u0 = 3/4*a-1/4*(a^3)+1/2
    u1 = 3/8*(a^2)-3/16*(a^4)-3/16
    u2 = 1/4*(a^3)-3/20*(a^5)-1/10
    p = 0
    for(i in 1:n){
      p = p + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(p/(n*h))
  }
  
  R.function = function(x){
    a = (1-x)/h
    u0 = +3/4*a-1/4*(a^3)+1/2
    u1 = 3/16*(a^4)-3/8*(a^2)+3/16 
    u2 = 1/4*(a^3)-3/20*(a^5)+1/10
    q = 0
    for(i in 1:n){
      q = q + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  A.mat[z,] = c(L.function(c(0.01,0.02,0.03,0.04,0.049)),C.function(seq(0.06,0.94,0.01)),R.function(c(0.95,0.96,0.97,0.98,0.99)))
  
}

dy = colMeans(x1.mat)
dx=d$x
f.hat = colMeans(A.mat)
plot(dx,dy,type="l",xlim = c(-0,1),ylim = c(0,3))
lines(c(0,dx,1),c(0,f.hat,0),col="red")
######################################################################################################
############------------66666666666666666666666666666666666666666666--------------####################
######################################################################################################
rm(list=ls())
#############################################
n = 200
h = 0.05
#############################################
iter = 1000
x1.mat = matrix(rep(0,iter*99),iter,99)
A.mat = matrix(rep(0,iter*99),iter,99)
for(z in 1:iter){
  #data
  data = rbeta(n,2,5)
  d = density(data,from = 0.01, to = 0.99 , n = 99 )
  x1.mat[z,] = d$y
  #################################################################################
  C.function= function(x){
    q = 0
    for(i in 1: n){
      q = q+3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  
  L.function = function(x){
    a = x/h
    u0 = 3/4*a-1/4*(a^3)+1/2
    u1 = 3/8*(a^2)-3/16*(a^4)-3/16
    u2 = 1/4*(a^3)-3/20*(a^5)-1/10
    p = 0
    for(i in 1:n){
      p = p + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(p/(n*h))
  }
  
  R.function = function(x){
    a = (1-x)/h
    u0 = +3/4*a-1/4*(a^3)+1/2
    u1 = 3/16*(a^4)-3/8*(a^2)+3/16 
    u2 = 1/4*(a^3)-3/20*(a^5)+1/10
    q = 0
    for(i in 1:n){
      q = q + (u2-u1*((x-data[i])/h))/(u0*u2-u1^2)*3/4*(1-((x-data[i])/h)^2)*ifelse(-h+data[i]<x&x<h+data[i],1,0)
    }
    return(q/(n*h))
  }
  A.mat[z,] = c(L.function(c(0.01,0.02,0.03,0.04,0.049)),C.function(seq(0.06,0.94,0.01)),R.function(c(0.95,0.96,0.97,0.98,0.99)))
  
}

dy = colMeans(x1.mat)
dx=d$x
f.hat = colMeans(A.mat)
plot(dx,dy,type="l",xlim = c(-0,1),ylim = c(0,3))
lines(c(0,dx,1),c(0,f.hat,0),col="red")