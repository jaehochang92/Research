

# Equal Educational Opportunity	 ------------------------------------------

libs <- c('stargazer', 'tidyr', 'car', 'olsrr')
sapply(libs, require, character.only = T)
eeo <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P236.txt', sep = '\t')
glimpse(eeo)

null.fit <- lm(ACHV ~ 1, data = eeo)
ols.fit <- lm(ACHV ~ ., data = eeo)
diag <- ls.diag(ols.fit)
stargazer(ols.fit, type = 'latex', style = 'all', single.row = T, digits = 5) # Table 9.3
stargazer(anova(null.fit, ols.fit), type = 'latex', style = 'all', summary = F, 
          single.row = T, digits = 5) # Table 9.3

plot(ols.fit$fitted.values, diag$std.res, pch = 19, xlab = 'Predicted', ylab = 'Residuals') # Figure 9.1
plot(eeo, pch = 19) # pairwise scatter plot, Figure 9.2
cor(eeo) # correlation
eeo.vifs <- vif(ols.fit)

# French Economy ----------------------------------------------------------

fe <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P241.txt', sep = '\t')
glimpse(fe)

null.fit <- lm(IMPORT ~ 1, data = fe)
ols.fit <- lm(IMPORT ~ . - YEAR, data = fe)
diag <- ls.diag(ols.fit)
stargazer(ols.fit, type = 'latex', style = 'all', single.row = T, digits = 5) # Table 9.6
stargazer(anova(null.fit, ols.fit), type = 'latex', style = 'all', summary = F, 
          single.row = T, digits = 5) # Table 9.6

plot(1:nrow(fe), diag$std.res, pch = 19, xlab = 'Index', ylab = 'Residuals') # Figure 9.3
lines(1:nrow(fe), diag$std.res)
fe.vifs <- vif(ols.fit)

# 1949 ~ 1959
fe <- subset(fe, YEAR %in% 49:59)

null.fit <- lm(IMPORT ~ 1, data = fe)
ols.fit <- lm(IMPORT ~ . - YEAR, data = fe)
diag <- ls.diag(ols.fit)
stargazer(ols.fit, type = 'latex', style = 'all', single.row = T, digits = 5) # Table 9.7
stargazer(anova(null.fit, ols.fit), type = 'latex', style = 'all', summary = F, 
          single.row = T, digits = 5) # Table 9.7
plot(1:nrow(fe), diag$std.res, pch = 19, xlab = 'Index', ylab = 'Residuals') # Figure 9.4
lines(1:nrow(fe), diag$std.res)

# APR

apr <- ols_step_all_possible_betas(ols.fit)
apr %>% spread(predictor, beta) # Table 9.8

# Advertising Data	 -------------------------------------------------------

adv <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P248.txt', sep = '\t')
glimpse(adv)

null.fit <- lm(St ~ 1, data = adv)
ols.fit <- lm(St ~ ., data = adv)
diag <- ls.diag(ols.fit)
stargazer(ols.fit, type = 'latex', style = 'all', single.row = T, digits = 5) # Table 9.10
stargazer(anova(null.fit, ols.fit), type = 'latex', style = 'all', summary = F, 
          single.row = T, digits = 5) # Table 9.10

plot(ols.fit$fitted.values, diag$std.res, pch = 19, xlab = 'Predicted', ylab = 'Residuals') # Figure 9.5
plot(1:nrow(adv), diag$std.res, pch = 19, xlab = 'Index', ylab = 'Residuals') # Figure 9.6
lines(1:nrow(adv), diag$std.res)

cor(adv[,-1]) # Table 9.11
adv.vifs <- vif(ols.fit)

# VIFs --------------------------------------------------------------------

eeo.vifs %>% print %>% mean
fe.vifs %>% print %>% mean
adv.vifs %>% print %>% mean # Table 9.12