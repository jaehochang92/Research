require(data.table)
hr_data <-
  read.csv("~/GoogleDrive/Research/StatConsult/Soomgo/2006/천경희/hr_data.csv")
omit_these <- hr_data$Sex == ''
hr_data <- hr_data[!omit_these,]
nrow(hr_data)
setDT(hr_data)

t.test(hr_data[Sex == 'M', PayRate], hr_data[Sex == 'F', PayRate])

chisq.test(table(hr_data$MaritalDesc, hr_data$PerformanceScore))

cor(hr_data[, .(PayRate,
                SpecialProjectsCount,
                EngagementSurvey,
                EmpSatisfaction)])
cor.mtest(hr_data[, .(PayRate,
                      SpecialProjectsCount,
                      EngagementSurvey,
                      EmpSatisfaction)])

fit <- lm(PayRate ~ Sex + SpecialProjectsCount, data = hr_data)
summary(fit)
par(mfrow = c(2, 2))
plot(fit, which = 1:4)