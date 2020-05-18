# LSE ---------------------------------------------------------------------

rm(list = ls())
p <- 5
n <- 1000
e.vec <- rnorm(n)
x.mat <- matrix(rnorm(n * p), ncol = p)
b.vec <- exp(1:p)
y.vec <- x.mat %*% b.vec + e.vec


max.iter <- 200
eps <- 1e-3
b.vec <- rep(1, p)
for (iter in 1:max.iter) {
  old.b.vec <- b.vec
  for (j in 1:p) {
    temp0 <- drop(x.mat[,-j] %*% old.b.vec[-j])
    b.vec[j] <- sum(x.mat[,j]*(y.vec - temp0))/sum(x.mat[,j]^2)
  }
  if (sum((b.vec - old.b.vec)^2) < eps) break
  if (iter >= max.iter) break
}
plot(b.vec, b.vec) ; abline(0, 1)

# Logi --------------------------------------------------------------------

p <- 4
n <- 500
e.vec <- rnorm(n)
x.mat <- matrix(rnorm(n * p), ncol = p)
tb.vec <- 1:4
y.vec <- x.mat %*% tb.vec + e.vec > 0
eps <- 1e-5

loss <- function(b.vec) {
  xb.vec <- x.mat %*% b.vec
  -sum(y.vec*xb.vec) + sum(log(1 + exp(xb.vec)))
}

iter.max <- 1e2
eps <- 1e-7
b.vec <- rep(0, p)
for (iter in 1:iter.max) {
  old.b.vec <- b.vec
  for (j in 1:p) {
    a <- -100 ; b <- 100 ; lmd <- .6
    for (k in 1:iter.max) {
      c <- lmd*a + (1 - lmd)*b
      d <- lmd*b + (1 - lmd)*a
      b.vec[j] <- c
      c.loss <- loss(b.vec)
      b.vec[j] <- d
      d.loss <- loss(b.vec)
      if (c.loss < d.loss) {b <- d} else {a <- c}
      if (abs(a - b) < eps) break
    }
    print(loss(b.vec))
  }
  if (sum((b.vec - old.b.vec)^2) < eps) break
  if (iter == iter.max) break
}
plot(tb.vec, b.vec) ; abline(0, 1)