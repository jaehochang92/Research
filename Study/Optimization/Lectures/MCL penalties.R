a0 <- 2
lam0 <- 1
gam0 <- 0


J <- Vectorize(function(t, a = a0, gam = gam0, lam = lam0) {
  c1 <- a*(lam - gam)^2/2
  c2 <- 0
  if (abs(t) >= a*(lam - gam)) {
    abs(t)*gam + c1
  } else {
    abs(t)*(lam - abs(t)/2/a) + c2
  }
})

na <- Vectorize(function(t, a = a0, gam = gam0, lam = lam0) {
  c1 <- a*(lam - gam)^2/2
  c2 <- 0
  if (abs(t) >= a*(lam - gam)) {
    p <- abs(t)*(gam - lam) + c1
  } else {
    p <- -abs(t)^2/2/a + c2
  }
  return(p)
})

plot(function(x) J(x, 2), n = 1e3, -4, 4)