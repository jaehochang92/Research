
# Education expenditure data - continued ----------------------------------

require(stargazer)
edu <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P198.txt',
                sep = '\t')

fit <- lm(Y ~ . - State - Region, data = edu)
stargazer(fit, type = 'latex', style = 'all', single.row = T, digits = 5)
diag <- ls.diag(fit)

plot(fitted.values(fit), diag$std.res, pch = 19, xlab = 'Predicted', ylab = 'Residuals')
text(447, 3.15, 'AK')
plot(edu$Region, diag$std.res, pch = 19, xlab = 'Region', ylab = 'Residuals')
plot(edu$X1, diag$std.res, pch = 19, xlab = 'Region', ylab = 'Residuals')
plot(edu$X2, diag$std.res, pch = 19, xlab = 'Region', ylab = 'Residuals')
plot(edu$X3, diag$std.res, pch = 19, xlab = 'Region', ylab = 'Residuals')

edu.om <- subset(edu, (State != 'AK  '))
fit.om <- lm(Y ~ . - State - Region, data = edu.om)
stargazer(fit.om, type = 'latex', style = 'all', single.row = T, digits = 5)

diag <- ls.diag(fit.om)
plot(fitted.values(fit.om), diag$std.res, pch = 19, xlab = 'Predicted', ylab = 'Residuals')
plot(edu.om$Region, diag$std.res, pch = 19, xlab = 'Region', ylab = 'Residuals')

ols.fit.om <- lm(Y ~ . - State - Region, data = edu.om)

# Weights estimate
ws <- c()
for (j in unique(edu.om$Region)) {
  indx <- edu.om$Region == j
  nj <- sum(indx)
  for (i in 1:nj) {
    ws <- c(ws, sum((ols.fit.om$residuals[indx])^2)/(nj - 1))
  }
}
wls.fit.om <- lm(Y ~ . - State - Region, data = edu.om, weights = (1/ws))
stargazer(ols.fit.om, wls.fit.om, type = 'latex', style = 'all', single.row = T, digits = 5)

diag <- ls.diag(wls.fit.om)
plot(fitted.values(wls.fit.om), diag$std.res, pch = 19, xlab = 'Predicted', ylab = 'Residuals')
plot(edu.om$Region, diag$std.res, pch = 19, xlab = 'Region', ylab = 'Residuals')

plot(function(x) 1/(1 + exp(-x)), -4, 4, xlab = 'X', ylab = expression(pi))