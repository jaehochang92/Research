
# Consumer Expenditure and Money Stock (CEMS) -----------------------------

require(stargazer) ; require(tibble)
cems <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P211.txt', sep = '\t')
glimpse(cems)
n <- nrow(cems)

ols.fit <- lm(Expenditure ~ Stock, data = cems)
diag <- ls.diag(ols.fit)
stargazer(ols.fit, type = 'latex', style = 'all', single.row = T, digits = 5) # Table 8.2

plot(1:n, diag$std.res, pch = 19, ylab = 'Residuals', xlab = 'Index')
lines(1:n, diag$std.res) ; abline(h = 0, lty = 2) # Figure 8.1

# Durbin-Watson statistic -------------------------------------------------

e <- diag$std.res
sum((e[2:n] - e[1:(n - 1)])^2)/sum(e^2)

# Removal of autocorrelation by transformation ----------------------------

hatrho <- sum(e[2:n]*e[1:(n - 1)])/sum(e^2)

attach(cems)
yst <- Expenditure[2:n] - hatrho*Expenditure[1:(n - 1)]
xst <- Stock[2:n] - hatrho*Stock[1:(n - 1)]
trs.fit <- lm(yst ~ xst)

n <- n - 1 # Differencing lose a sample.
diag <- ls.diag(trs.fit)
e <- diag$std.res
sum((e[2:n] - e[1:(n - 1)])^2)/sum(e^2) # DW statistic

plot(1:n, diag$std.res, pch = 19, ylab = 'Residuals', xlab = 'Index')
lines(1:n, diag$std.res) ; abline(h = 0, lty = 2) # Figure 8.2

# Housing Starts	 ---------------------------------------------------------

Housing <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P219.txt', sep = '\t')
glimpse(Housing)
n <- nrow(Housing)

tent.fit <- lm(H ~ P, data = Housing)
stargazer(tent.fit, type = 'latex', style = 'all', single.row = T, digits = 5) # Table 8.5
diag <- ls.diag(tent.fit)
plot(1:n, diag$std.res, pch = 19, ylab = 'Residuals', xlab = 'Index')
lines(1:n, diag$std.res) ; abline(h = 0, lty = 2) # Figure 8.3

tent.fit2 <- lm(H ~ P + D, data = Housing)
stargazer(tent.fit2, type = 'latex', style = 'all', single.row = T, digits = 5) # Table 8.6
diag <- ls.diag(tent.fit2)
plot(1:n, diag$std.res, pch = 19, ylab = 'Residuals', xlab = 'Index')
lines(1:n, diag$std.res) ; abline(h = 0, lty = 2) # Figure 8.4

# Ski Sales ---------------------------------------------------------------

Ski <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P224.txt', sep = '\t')
glimpse(Ski)
n <- nrow(Ski)
Q14 <- Ski[,"Season"] == 1
Q23 <- Ski[,"Season"] == 0

ols.fit <- lm(Sales ~ PDI, data = Ski)
stargazer(ols.fit, type = 'latex', style = 'all', single.row = T, digits = 5) # Table 8.7
diag <- ls.diag(ols.fit)
plot(1:n, diag$std.res, type = 'n', ylab = 'Residuals', xlab = 'Index')
points((1:n)[Q14], diag$std.res[Q14], pch = 1)
points((1:n)[Q14], diag$std.res[Q23], pch = 19) # Figure 8.5

ols.fit2 <- lm(Sales ~ PDI + Season, data = Ski)
stargazer(ols.fit2, type = 'latex', style = 'all', single.row = T, digits = 5) # Table 8.9
diag <- ls.diag(ols.fit2)
plot(1:n, diag$std.res, type = 'n', ylab = 'Residuals', xlab = 'Index')
points((1:n)[Q14], diag$std.res[Q14], pch = 1)
points((1:n)[Q14], diag$std.res[Q23], pch = 19) # Figure 8.7