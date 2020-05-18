rm(list=ls())
options(scipen = 10)
require(stargazer)
setwd('/Users/statconsult/Documents')
datum <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P103.txt', header = T,
               sep = '\t')


# Getting the fitted table ------------------------------------------------

model <- lm(Y~., data = datum)
stargazer(model, type = 'latex', style = 'all', single.row = T)

# plot ----------------------------------------------------------------

plot(datum) # Check whether X's have collinearity


# A qqplot example --------------------------------------------------------

n <- 2e2
ex <- rnorm(n)

set.seed(101)
setEPS()
postscript('qq1.eps', width = 16, height = 8)
par(mfrow = c(1,2))
qqplot(qnorm(ppoints(n)), ex, main = expression("Q-Q plot for" ~~ {N(0,1)}),
       ylab = 'Sample Quantiles', xlab = 'Theoretical Quantiles')
qqline(ex, distribution = function(p) qnorm(p), col = 2)
qqplot(qt(ppoints(n), 3), ex, main = expression("Q-Q plot for" ~~ {t}[3]),
       ylab = 'Sample Quantiles', xlab = 'Theoretical Quantiles')
qqline(ex, distribution = function(p) qt(p, df = 3), col = 2)
dev.off()

setEPS()
postscript('qq2.eps', width = 16, height = 8)
par(mfrow = c(1,2))
qqplot(qunif(ppoints(n)), ex, main = expression("Q-Q plot for" ~~ {U(0,1)}),
       ylab = 'Sample Quantiles', xlab = 'Theoretical Quantiles')
qqline(ex, distribution = function(p) qunif(p), col = 2)
qqplot(qchisq(ppoints(n), 3), ex, main = expression("Q-Q plot for" ~~ {chi^2}[3]),
       ylab = 'Sample Quantiles', xlab = 'Theoretical Quantiles')
qqline(ex, distribution = function(p) qchisq(p, df = 3), col = 2)
dev.off()


# NY Rivers : Observation deleted ---------------------------------------------------------------

datum <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P010.txt', header = T,
                  sep = '\t')
model1 <- lm(Nitrogen~. - River, data = datum)
model2 <- lm(Nitrogen~. - River, data = datum, subset = (River != 'Neversink'))
model3 <- lm(Nitrogen~. - River, data = datum, subset = (River != 'Hackensack'))
cbind('None' = summary(model1)$coefficients[,3],
      'Neversink' = summary(model2)$coefficients[,3],
      'Hackensack' = summary(model3)$coefficients[,3])


# Diagnosis ---------------------------------------------------------------

par(mfrow = c(2, 2))
plot(model1)
dev.off()
diagnosis <- ls.diag(model1)

# Matrix manipulation of OLS
Y <- matrix(datum$Nitrogen, ncol = 1) # Response vector
X <- as.matrix(cbind(1, datum[, c("Agr", "Forest", "Rsdntial", "ComIndl")])) # Design matrix
P <- X %*% solve(t(X) %*% X) %*% t(X) # P matrix
p <- 4
n <- nrow(datum)

SSE <- c(t(Y) %*% (diag(1, n) - P) %*% Y) # Matrix Expression of SSE
SSEi <- Vectorize(function(i) { # SSE_(i)
  Xi <- X[-i,] ; Yi <- Y[-i,]
  hYi <- Xi %*% solve(t(Xi) %*% Xi) %*% t(Xi) %*% Yi
  c(t(Yi - hYi) %*% (Yi - hYi))
})
MSE <- SSE/(n - p - 1)
cSSE <- SSEi(1:n)
cMSE <- cSSE/(n - p - 2)

# Detecting High Leverage points
datum$River[diag(P) > 2*(p + 1)/n] # Compare the result with the plotted one

# Cook's D
r <- model1$residuals/sqrt(MSE*(1 - diag(P)))
C <- r^2/(p + 1)*diag(P)/(1 - diag(P))
datum$River[C > 1] # influential points

# DFITS (Welsch and Kuh measure)
extr <- model1$residuals/sqrt(cMSE*(1 - diag(P)))
DFITS <- extr*sqrt(diag(P)/(1 - diag(P)))
datum$River[abs(DFITS) > 2*sqrt((p + 1)/(n - p - 1))] # influential points

# Hadi's influence measure
d <- model1$residuals/sqrt(SSE)
H <- diag(P)/(1 - diag(P)) + (p + 1)/(1 - diag(P))*d^2/(1 - d^2)
plot(1:n, H, xlab = 'observation index')
datum$River[order(H, decreasing = T)][1:5] # Top 5 observations w.r.t. the magnitude of Hadi's measure

# P - R plot
plot(diag(P)/(1 - diag(P)), H - diag(P)/(1 - diag(P)), main = 'P - R plot')
