rm(list=ls())
require(stargazer)

SP <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P060.txt', header = T,
               sep = '\t')


# Getting the fitted table ------------------------------------------------

model <- lm(Y~., data = SP)
stargazer(model, type = 'latex', style = 'all', single.row = T)

# F - test ----------------------------------------------------------------

# significance of model
M0 <- lm(Y ~ 1, data = SP)
M1 <- lm(Y ~ 1 + ., data = SP)
anova(M0, M1)
qf(.99, 6, 30 - 6 -1) # F-value for alpha = 0.01

# comparison between models
M0 <- lm(Y ~ 1 + X1 + X3, data = SP)
M1 <- lm(Y ~ 1 + ., data = SP)
anova(M0, M1)
qf(.95, 4, 30 - 4 -1) # F-value for alpha = 0.01

# Testing subset of regression coef. are zero
M0 <- lm(Y ~ 1 + I(X1 + X3), data = SP)
M1 <- lm(Y ~ 1 + X1 + X3, data = SP)
anova(M0, M1)

M0 <- lm(Y ~ 1 + I(X1 + X3), data = SP)
M1 <- lm(Y ~ ., data = SP)
anova(M0, M1)

# Constrained regression : 결과가 본문내용과 다름.
M0 <- lm(Y - X3 ~ 1 + I(X1 - X3), data = SP)
M1 <- lm(Y ~ 1 + X1 + X3, data = SP)
Rq2 <- summary(M0)$r.squared
Rp2 <- summary(M1)$r.squared
((Rp2 - Rq2)/(2 - 1))/((1 - Rp2)/(30 - 2 - 1))