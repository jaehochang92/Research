rm(list=ls())
options(scipen = 10)
require(stargazer)

# Linear vs Nonlinear ------------------------------------------------
# Design Examples
n <- 2e1
b0 <- 1
b1 <- 2
b2 <- 3
e <- rnorm(n, 0, .25)
X <- log(2:(n + 1))
 
# postscript('6-1.eps', width = 4*2, height = 4*3)
# par(mfrow = c(3, 2))
# plot(X, b0 + b1*X + e, xlab = 'X', ylab = 'Y', 
#      main = expression(Y==beta[0]+beta[1]~X+epsilon))
# plot(X, b0 + b1*X + b2*X^2 + e, xlab = 'X', ylab = 'Y', 
#      main = expression(Y==beta[0]+beta[1]~X+beta[2]~X^2+epsilon))
# plot(X, b0 + b1*log(X) + e, xlab = 'X', ylab = 'Y', 
#      main = expression(Y==beta[0]+beta[1]~logX+epsilon))
# plot(X, b0 + b1*sqrt(X) + e, xlab = 'X', ylab = 'Y', 
#      main = expression(Y==beta[0]+beta[1]~sqrt(X)+epsilon))
# plot(X, b0 + exp(b1*X) + e, xlab = 'X', ylab = 'Y', 
#      main = expression(Y==beta[0]+e^{beta[1]~X}+epsilon)) # Nonlinear!
# dev.off()


postscript('Nonlinears.eps', width = 4*2, height = 4*5)
par(mfrow = c(5, 2))
plot(X, b0*X^b1) ; plot(log(X), log(b0) + b1*log(X), xlab = "X'", ylab = "Y'")
plot(X, b0*exp(X*b1)) ; plot(X, log(b0) + b1*X, xlab = "X", ylab = "Y'")
plot(X, b0 + b1*log(X)) ; plot(log(X), b0 + b1*log(X), xlab = "X'", ylab = "Y")
plot(X, X/(b0*X-b1)) ; plot(1/X, b0 - b1/X, xlab = "X'", ylab = "Y'")
plot(X, exp(b0 + b1*X)/(1+exp(b0 + b1*X))) ; plot(X, b0 + b1*X, xlab = "X", ylab = "Y'")
dev.off()


# Bacteria deaths due to X-ray radiation (BD data) ------------------------

bact <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P168.txt',
                 header = T, sep = '\t')

ordfit <- lm(N_t ~ t, data = bact)
trsfit <- lm(log(N_t) ~ t, data = bact)

postscript('BDfigs1.eps', width = 8*1, height = 8*1)
plot(bact, ylab = expression(eta[t]), pch = 19)
dev.off()

postscript('BDfigs2.eps', width = 8*1, height = 8*1)
plot(bact$t, log(bact$N_t), ylab = expression(log~eta[t]), xlab = 't', pch = 19)
dev.off()


postscript('BDfigs3.eps', width = 8*1, height = 8*1)
plot(bact$t, scale(ordfit$residuals), ylab = expression(log~eta[t]), xlab = 't', pch = 19)
dev.off()

postscript('BDfigs4.eps', width = 8*1, height = 8*1)
plot(bact$t, scale(trsfit$residuals), pch = 19, xlab = 't', ylab = 'Std. residuals')
dev.off()


stargazer(trsfit, type = 'latex', style = 'all', single.row = T, digits = 5)


# Heteroscadacity example -------------------------------------------------


postscript('Hetero.eps', width = 8*1, height = 8*1)
set.seed(30)
n <- 70
X <- as.matrix(data.frame(i = 1, x = 1:n))
B <- as.matrix(data.frame(B = c(1, .5)))
y <- X %*% B + rnorm(n, sd = seq(0, 1, length.out = n)) # Heteroscadastic design
H <- X %*% solve(t(X) %*% X) %*% t(X) # Projection matrix (Hat matrix)
plot(X[, 2], scale((diag(1, n) - H) %*% y), xlab = 'X', ylab = 'std. Residuals', pch = 19)
lines(c(0, 40), c(0.2, -3))
lines(c(0, 60), c(0.2, 4))
dev.off()

# Injury incidents in airlines --------------------------------------------

inj <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P174.txt',
                 header = T, sep = '\t')
init <- lm(Y ~ N, data = inj) # Initial model
trsf <- lm(sqrt(Y) ~ N, data = inj)
stargazer(init, type = 'latex', style = 'all', single.row = T, digits = 5)
stargazer(trsf, type = 'latex', style = 'all', single.row = T, digits = 5)

postscript('Injfigs1.eps', width = 8*1, height = 8*1)
with(inj, plot(N, Y, pch = 19, ylab = 'residuals')) # The residuals increase with n_i.
dev.off()
postscript('Injfigs2.eps', width = 8*1, height = 8*1)
with(inj, plot(N, scale(init$residuals), pch = 19, ylab = 'residuals')) # The residuals do not increase with n_i.
dev.off()
postscript('Injfigs3.eps', width = 8*1, height = 8*1)
with(inj, plot(N, scale(trsf$residuals), pch = 19, ylab = 'residuals')) # The residuals do not increase with n_i.
dev.off()

# Supervised Workers and Supervisors --------------------------------------


sup <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P176.txt',
                header = T, sep = '\t')
init <- lm(Y ~ X, data = sup) # Initial model
trsup <- transform(sup, Y = Y/X, X = 1/X)
trsf <- lm(Y ~ X, data = trsup)
stargazer(init, type = 'latex', style = 'all', single.row = T, digits = 5)
stargazer(trsf, type = 'latex', style = 'all', single.row = T, digits = 5)

postscript('Supfigs1.eps', width = 8*1, height = 8*1)
with(sup, plot(X, Y, pch = 19, ylab = 'Y'))
dev.off()
postscript('Supfigs2.eps', width = 8*1, height = 8*1)
with(sup, plot(X, scale(init$residuals), pch = 19, ylab = 'residuals'))
dev.off()
postscript('Supfigs3.eps', width = 8*1, height = 8*1)
with(trsup, plot(X, scale(trsf$residuals), pch = 19, xlab =  '1/X', ylab = 'residuals'))
dev.off()

logfit <- lm(log(Y) ~ X, data = sup)
postscript('Supfigs4.eps', width = 8*1, height = 8*1)
with(sup, plot(X, scale(logfit$residuals), pch = 19, ylab = 'residuals'))
dev.off()

# Brain data --------------------------------------------------------------

brain <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P184.txt',
                  header = T, sep = '\t')
postscript('Brainfigs1.eps', width = 8*1, height = 8*1)
with(brain, plot(BodyWeight, BrainWeight, pch = 19))
dev.off()
postscript('Brainfigs2.eps', width = 8*2, height = 8*2)
par(mfrow = c(2, 2))
with(brain, plot(BodyWeight^.5, BrainWeight^.5, pch = 19, 
                 xlab = expression(X^0.5), ylab = expression(Y^0.5)))
with(brain, plot(log(BodyWeight), log(BodyWeight), pch = 19, 
                 xlab = expression(ln~X), ylab = expression(ln~Y)))
with(brain, plot(BodyWeight^-.5, BrainWeight^-.5, pch = 19, 
                 xlab = expression(X^-.5), ylab = expression(Y^-.5)))
with(brain, plot(BodyWeight^-1, BrainWeight^-1, pch = 19, 
                 xlab = expression(X^-1), ylab = expression(Y^-1)))
dev.off()