dev.off()
opar <- par()
methods(plot)

ls(envir = environment(plot))

ls(environment(grid))

plot(mtcars)
class(mtcars)

glm.fit <- glm(vs ~ mpg, data = mtcars, family = 'binomial')
par(mfrow = c(2,2))
plot(glm.fit)

par(opar)

class(glm.fit) <- c('log', class(glm.fit))
plot.log <- function(fit) {
  x.v <- fit$model[,2]
  y.v <- fit$model[,1][order(x.v)]
  plot(x.v[order(x.v)], y.v, xlab = colnames(fit$model)[2], ylab = colnames(fit$model)[1])
  lines(x.v[order(x.v)], fit$fitted.values[order(x.v)], lty = 2)
}
plot(glm.fit)