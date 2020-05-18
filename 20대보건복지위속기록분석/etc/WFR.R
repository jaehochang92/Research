require(data.table)
require(dplyr)
library(readxl)
options(scipen = 1)
# path <- "~/GoogleDrive/Research/HCLee/data/WFR.xlsx"
path <- "../data/WFR.xlsx"
WFR <- read_excel(
  path,
  sheet = "Sheet1",
  col_types = c("text", "numeric", "numeric")
)
setDT(WFR)

WFR[, `예산내역(조원)` := 예산내역/1e6]
WFR[, `log(예산내역(조원)+1)` := log(`예산내역(조원)`+1)]
# plot(WFR[, .(`예산내역(조원)`, 발의건수)])
plot(WFR[, .(`log(예산내역(조원)+1)`, 발의건수)])

poiss.fit <- glm(발의건수 ~ `log(예산내역(조원)+1)`, 'poisson', WFR,
                     control = list('trace' = T))
poiss.fit <- glm(발의건수 ~ `log(예산내역(조원)+1)`, 'quasipoisson', WFR,
                     control = list('trace' = T))
summary(poiss.fit)


p <- predict(poiss.fit, data.table(
  `log(예산내역(조원)+1)` = log(c(0,10,20)+1)), type = 'response')
p[2]-p[1];p[3]-p[2]

cfs <- coef(poiss.fit)
plot(function(x) exp(cfs[1])*(x+1)^cfs[2], 0, 30, add = T, col = 'blue')
title(main = '산점도 및 회귀분석 적합선')

# plot(WFR[, .(발의건수, 예산내역)], ylim = c(0, 1e7))
# with(WFR, points(발의건수,  exp(2.5148) * 발의건수 ^ 2.8258, pch = '^', col = 'blue'))

stargazer::stargazer(poiss.fit, out = './GoogleDrive/Research/HCLee/result/fit.html')