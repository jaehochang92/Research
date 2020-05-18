require(data.table);require(dplyr)
WFR <- fread('../GoogleDrive/Research/Prof. Hyunchool Lee/data/WFR.csv')

WFR[, `log(예산내역+1)` := log(예산내역+1)]
WFR[, `log(발의건수)` := log(발의건수)]
plot(WFR[, .(`log(발의건수)`, `log(예산내역+1)`)])
trnsf.fit <- lm(`log(예산내역+1)` ~ I(log(발의건수)), WFR)
trnsf.fit %>% {
  abline(., col = 'red')
  summary(.)
  title(main = '산점도 및 회귀분석 적합선')
}

# plot(WFR[, .(발의건수, 예산내역)], ylim = c(0, 1e7))
# with(WFR, points(발의건수,  exp(2.5148) * 발의건수 ^ 2.8258, pch = '^', col = 'blue'))

stargazer::stargazer(trnsf.fit, out = '../GoogleDrive/Research/Prof. Hyunchool Lee/result/logfit.html')