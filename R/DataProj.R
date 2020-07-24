
# Linear Regression and Projection ----------------------------------------


require(rgl)
options(scipen = 1e2)
ng <- 7e1
s <- 4
u <- rep(1, 3)
x <- c(1, 0, 0)
X <- cbind(u, x)
b <- seq(-1 * s, 1 * s, length.out = ng)
B <- t(expand.grid(b, b))
XB <- X %*% B # Span!
y <- XB[, sample(1:ncol(XB), 1)] + rnorm(3, 0, 3)
hy <- X %*% solve(t(X) %*% X) %*% t(X) %*% y
open3d()
points3d(XB[1, ],
         XB[2, ],
         XB[3, ],
         ticktype = 'detailed',
         col = 'green',
         cex = .2)
for (i in 1:ncol(X)) {
  lines3d(
    x = c(0, X[1, i]),
    y = c(0, X[2, i]),
    z = c(0, X[3, i]),
    lwd = 1,
    add = T
  )
}
lines3d(
  x = c(0, y[1]),
  y = c(0, y[2]),
  z = c(0, y[3]),
  lwd = 1.3,
  add = T,
  col = 'blue'
)
lines3d(
  x = c(0, hy[1]),
  y = c(0, hy[2]),
  z = c(0, hy[3]),
  lwd = 1.3,
  add = T,
  col = 'red'
)
lines3d(
  x = c(y[1], hy[1]),
  y = c(y[2], hy[2]),
  z = c(y[3], hy[3]),
  lwd = 1.3,
  add = T,
  col = 'red',
  lty = 2
)
title3d(
  main = 'Data Space',
  xlab = 'd1',
  ylab = 'd2',
  zlab = 'd3'
)
# axis3d('y', pos = c(0, NA, 0))
axes3d()
# title3d('main', 'sub', 'xlab', 'ylab', 'zlab')

# Orthogonal Decomposition ------------------------------------------------


ng <- 4e1
s <- 4
u <- rep(1, 3)
X <- as.matrix(cbind(u, c(1, 0, 0), c(0, 1, 0)))
b <- seq(-1 * s, 1 * s, length.out = ng)
B <- t(expand.grid(b, b, b))
XB <- X %*% B # Span!

X0 <- X[, 1:2]
X1 <- X[, 2:3]
P0 <- X0 %*% solve(t(X0) %*% X0) %*% t(X0)
X1_0 <- (diag(1, 3) - P0) %*% X1
XB0 <- X0 %*% B[1:2, ]
XB1_0 <- X1_0 %*% B[2:3, ]

P1_0 <- X1_0 %*% solve(t(X1_0) %*% X1_0) %*% t(X1_0)
y <- XB[, sample(1:ncol(XB), 1)] + rnorm(3, 0, 5)
open3d()
points3d(XB0[1, ],
         XB0[2, ],
         XB0[3, ],
         ticktype = 'detailed',
         col = 'green',
         cex = .2)
points3d(XB1_0[1, ],
         XB1_0[2, ],
         XB1_0[3, ],
         ticktype = 'detailed',
         col = 'green',
         cex = .2)
lines3d(
  x = c(0, y[1]),
  y = c(0, y[2]),
  z = c(0, y[3]),
  lwd = 1.3,
  add = T,
  col = 'blue'
)
lines3d(
  x = c(0, hy[1]),
  y = c(0, hy[2]),
  z = c(0, hy[3]),
  lwd = 1.3,
  add = T,
  col = 'red'
)
lines3d(
  x = c(y[1], hy[1]),
  y = c(y[2], hy[2]),
  z = c(y[3], hy[3]),
  lwd = 1.3,
  add = T,
  col = 'red',
  lty = 2
)
title3d(
  main = 'Data Space',
  xlab = 'd1',
  ylab = 'd2',
  zlab = 'd3'
)
# axis3d('y', pos = c(0, NA, 0))
axes3d()
# title3d('main', 'sub', 'xlab', 'ylab', 'zlab')