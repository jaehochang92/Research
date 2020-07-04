set.seed(123)
x=matrix(rnorm(200),100,2)
grp=ifelse(apply(x*x,1,sum)<=1.16,1,2)
table(grp)


# Get started
library(e1071)
y=as.factor(grp)
svm.model=svm(y~x,kernel="radial",scale = F,gamma=0.5)
summary(svm.model)
plot(x, pch=c(20,21)[grp], col=c("blue","red")[svm.model$fitted],
     xlim = c(-3,3), ylim = c(-3,3), xlab = "x1", ylab = "x2",
     main = "Simulated Bivariate Data3")
theta=seq(0,1,0.01)*2*pi ; r=sqrt(1.16)
par(new=T) ; plot(r*cos(theta),r*sin(theta),lty="dotted",type = "l",
                  xlim = c(-3,3), ylim = c(-3,3), xlab = "", ylab = "" )

# Spam Data
library(kernlab)
str(spam)
svm.model=svm(type~.,data = spam, gamma = 1, cost = 1)
addmargins(table(spam$type, svm.model$fitted))


# Cross Validation
n=nrow(spam)
sub=sample(1:n, round(3/4*n))
spam.1=spam[sub,]
spam.2=spam[-sub,]

svm.model.1=svm(type~.,data = spam.1, gamma=1, cost=1)
svm.predict.2=predict(svm.model.1, newdata=spam.2)
addmargins(table(spam.2$type, svm.predict.2))

ptm <- proc.time()
tune.svm=tune(svm,type~.,data = spam.1, ranges = list(gamma=seq(0.1,1,by=0.1),
                                                      cost=seq(0.1,1,by=0.1)))
proc.time() - ptm
summary(tune.svm) # gamma 0.1, cost 0.9


set.seed(123)
x=rnorm(100)
y=.8*x+rnorm(100,0,0.6)
svm.model=svm(y~x, kernel="linear", epsilon=1, scale=F)
summary(svm.model)
plot(y~x, xlim = c(-3,3), ylim = c(-3,3), main = "Simulated Bivariate Data")
par(new=T) ; plot(svm.model$fitted~x, main="",xlim=c(-3,3),ylim=c(-3,3),
                  xlab="", ylab="", col="red", pch=20)
points(x[svm.model$index], y[svm.model$index], pch=21)


set.seed(12345)
x <- rnorm (100); y.fit <- 0.8*x^2
y <- y.fit + rnorm(100, 0, 0.6)
svm.model <- svm(y~x, gamma=0.5, epsilon=1, scale=F, kernel="radial")
summary (svm.model)
plot (y~x, main="simulated bivariate data 2", xlim=c(-3, 3), ylim=c(-2, 6))
par(new=T)
plot(svm.model$fitted~x, main="", xlim=c(-3, 3), ylim=c(-2, 6),
     xlab="", ylab="", col="red", pch=20)
points(x[svm.model$index], y[svm.model$index], pch=3)