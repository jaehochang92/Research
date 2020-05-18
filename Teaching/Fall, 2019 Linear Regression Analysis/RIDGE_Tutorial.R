
# RIDGE regression --------------------------------------------------------

library(MASS)

data("longley")
str(longley)

# Longley's Economic Regression Data
# :
# A macroeconomic data set which provides a well-known example for a highly collinear regression.
# 
# Structure
# A data frame with 7 economical variables, observed yearly from 1947 to 1962 (n=16).
# 
# GNP.deflator :
# GNP implicit price deflator (1954=100) -> y
# 
# GNP :
# Gross National Product.
# 
# Unemployed :
# number of unemployed.
# 
# Armed.Forces :
# number of people in the armed forces.
# 
# Population :
# ‘noninstitutionalized’ population ≥ 14 years of age.
# 
# Year :
# the year (time).
# 
# Employed :
# number of people employed.
# 
# The regression lm(Employed ~ .) is known to be highly collinear.

names(longley)[1] <- "y"

ridge.fit <- lm.ridge(y ~ ., longley, lambda = seq(0, 0.1, 0.001))
plot(ridge.fit)