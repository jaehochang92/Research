
# A STUDY OF SUPERVISOR PERFORMANCE : Using packages ---------------------------------------

libs <- c('stargazer', 'dplyr', 'MASS', 'car', 'leaps', 'purrr')
sapply(libs, require, character.only = T) # Attaching several packages at once
sp <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P060.txt', sep = '\t')
n <- nrow(sp)
glimpse(sp)

cor(sp[,-1]) # Table 11.1

null.fit <- lm(Y ~ 1, data = sp)
ols.fit <- lm(Y ~ ., data = sp)

fwd.fit <- regsubsets(x = sp[, -1], y = sp[, 1], method = 'forward', 
                      nbest = 1) # Choose only one best model per each selection size

# regsubsets returns an object of class "regsubsets" containing no 
# user-serviceable parts. It is designed to be processed by summary.regsubsets.
# 
# summary.regsubsets returns an object with elements ;
# 
# which   A logical matrix indicating which elements are in each model
# rsq     The r-squared for each model
# rss	    Residual sum of squares for each model
# adjr2	  Adjusted r-squared
# cp	    Mallows' Cp
# bic	    Schwartz's information criterion or Bayesian Inforamtion Criterion
# outmat  A version of the which component that is formatted for printing
# obj	    A copy of the regsubsets object

sum.fwd <- summary(fwd.fit)
p <- rowSums(sum.fwd$which)

RMS <- sqrt(fwd.fit$rss[-1]/(fwd.fit$nn - p))

b <- lapply(coef(fwd.fit, 1:(fwd.fit$np - 1)), function(x) x[-1]) 
# getting coefficients of each submodels
se.b <- lapply(vcov(fwd.fit, 1:(fwd.fit$np - 1)), function(x) sqrt(diag(x)[-1])) 
# getting s.e. of ßs of each submodels
t <- map2_dbl(b, se.b, function(x, y) min(abs(x/y)))
# function from a package purrr ; mapping functions to elements of lists

Cp <- sum.fwd$cp

ic <- c()
form <- c()
for (k0 in c(2, log(n))) {
  ic.added <- null.fit
  
  ic.added <- add1(ic.added, ~ . + X1, k = k0) # Add X1
  ic <- c(ic, ic.added$AIC[2])
  ic.added <- lm(Y ~ X1, sp)
  form <- c(form, formula(ic.added))
  
  ic.added <- add1(ic.added, ~ . + X3, k = k0)
  ic <- c(ic, ic.added$AIC[2])
  ic.added <- lm(Y ~ X1 + X3, sp)
  form <- c(form, formula(ic.added))
  
  ic.added <- add1(ic.added, ~ . + X6, k = k0)
  ic <- c(ic, ic.added$AIC[2])
  ic.added <- lm(Y ~ X1 + X3 + X6, sp) 
  form <- c(form, formula(ic.added))
  
  ic.added <- add1(ic.added, ~ . + X2, k = k0)
  ic <- c(ic, ic.added$AIC[2])
  ic.added <- lm(Y ~ X1 + X3 + X6 + X2, sp)
  form <- c(form, formula(ic.added))
  
  ic.added <- add1(ic.added, ~ . + X4, k = k0)
  ic <- c(ic, ic.added$AIC[2])
  ic.added <- lm(Y ~ X1 + X3 + X6 + X2 + X4, sp)
  form <- c(form, formula(ic.added))
  
  ic.added <- add1(ic.added, ~ . + X5, k = k0)
  ic <- c(ic, ic.added$AIC[2])
  ic.added <- lm(Y ~ ., sp)
  form <- c(form, formula(ic.added))
}
ic
aic <- ic[1:(length(ic)/2)] ; bic <- ic[ic != aic]
df <- data.frame(Eq = as.character(form[1:6]), min.t = t, RMS, Cp, p, Rank = c(rep(1, 5), NA), aic, bic)

stargazer(df, type = 'latex', style = 'all', single.row = T, digits = 5, summary = F) # Table 11.2

# Table 11.3, 11.5 remains as exercises for learners.
# (Hint : use 'drop1' function to get a backward elimination procedure.
# You may combine the procedures to conduct a stepwise selection.)

# All possible equations --------------------------------------------------

all.fit <- regsubsets(x = sp[, -1], y = sp[, 1], method = 'exhaustive', 
                      nbest = max(choose(6, 0:6)))
# 'exhaustive' method fits all possible candidate models for given design matrix.
# 'nbest' option fixes the no. of maximum best models per each size of fitting.
# For example, for nbest = 4, in the stage of selecting 5 predictors, 
# regsubsets finds 4 best models with 5 predictors based on criterion.
# To fulfill the all possible regression, nbest must obtain the maximum combinations
# of predictors, in this example, 6C3 = 20.

sum.all <- summary(all.fit)
allCp <- cbind(sum.all$outmat, Cp = round(sum.all$cp, 2)) # Table 11.4

admss <- sum.all$cp < 10
Cpplot <- data.frame(p = as.numeric(substr(rownames(allCp), 1, 1)[admss]) + 1, 
                     Cp = round(sum.all$cp, 2)[admss])
plot(Cpplot, pch = 19, xlab = expression(p), ylab = expression(C[p]), xlim = c(1, 7))
abline(0, 7/7) # Figure 11.1


# THE HOMICIDE DATA -------------------------------------------------------

hom <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P315-6.txt', sep = '\t')
n <- nrow(hom)
glimpse(hom)

sdf <- as_data_frame(sapply(hom, scale))
ols.fit <- lm(H ~ G + M + W - 1, data = sdf)
stargazer(ols.fit, type = 'latex', style = 'all', single.row = T, digits = 5)

f <- ols.fit
a <- lm(H ~ G - 1, data = sdf)
b <- lm(H ~ M - 1, data = sdf)
c <- lm(H ~ W - 1, data = sdf)
d <- lm(H ~ G + M - 1, data = sdf)
e <- lm(H ~ G + W - 1, data = sdf)
g <- lm(H ~ M + W - 1, data = sdf)
stargazer(a, b, c, d, e, f, g, type = 'latex', report = 'vct', single.row = T, digits = 3) # Table 11.10


# AN AIR POLLUTION STUDY -------------------------------------------------

ap <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P320-1.txt', sep = '\t')
n <- nrow(ap)
glimpse(ap)
data_frame(Var = names(ap), mean = sapply(ap, mean), sd = sapply(ap, sd)) # Table 11.11

sdf <- as_data_frame(sapply(ap, scale))
ols.fit <- lm(Y ~ ., data = sdf)
vifs <- vif(ols.fit)
stargazer(ols.fit, vifs, type = 'latex', report = 'vcst', single.row = T, digits = 5) # Table 11.14

r.ols.fit <- lm(Y ~ . -X7 - X8 - X10 - X11 - X15, data = sdf)
vifs <- vif(r.ols.fit)
stargazer(r.ols.fit, vifs, type = 'latex', report = 'vcst', single.row = T, digits = 5) # Table 11.15

r.ols.fit <- lm(Y ~ . -X7 - X8 - X10 - X11  - X12 - X13 - X15, data = sdf)
vifs <- vif(r.ols.fit)
stargazer(r.ols.fit, vifs, type = 'latex', report = 'vcst', single.row = T, digits = 5) # Table 11.15