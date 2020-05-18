lib <- c('mvnfast', 'dplyr', 'tictoc', 'ncpen')
chck <- sapply(lib, require, character.only = TRUE)
if (sum(!chck) > 0) {install.packages(lib[!chck])}
rm(list = ls())
set.seed(1001)

user <- Sys.getenv("RSTUDIO_USER_IDENTITY")

b1 <- c(.2, -1.2, .1)
q <- c(17, 14, 11, 8, 5, 3)*3
n <- c(10, 15, 60)*1e3

models <- list()
for (ni in 1:length(n)) {
  for (qi in 1:length(q)) {
    eval(paste('n', n[ni], 'q', q[qi], sep = '') %>%
           paste('models$', ., ' <- list(n = n[', ni, '], b.vec = c(b1, rep(0, q[', qi, 
                 '])))', sep = '') %>% parse(text = .))
  }
}

K <- 1e2
cvf <- 5
timer <- c()
for (modeliter in 1:length(models)) {
  k <- 1
  n <- models[[modeliter]]$n
  p <- length(models[[modeliter]]$b.vec)
  b.vec <- models[[modeliter]]$b.vec
  models[[modeliter]]$unres.MSE <- rep(NA, K)
  models[[modeliter]]$unres.coef <- matrix(rep(NA, K*p), nrow = p)
  models[[modeliter]]$lasso.MSE <- rep(NA, K)
  models[[modeliter]]$lasso.hl <- rep(NA, K)
  models[[modeliter]]$lasso.coef <- matrix(rep(NA, K*p), nrow = p)
  models[[modeliter]]$scad.MSE <- rep(NA, K)
  models[[modeliter]]$scad.hl <- rep(NA, K)
  models[[modeliter]]$scad.coef <- matrix(rep(NA, K*p), nrow = p)
  models[[modeliter]]$mcl.MSE <- rep(NA, K)
  models[[modeliter]]$mcl.hl <- rep(NA, K)
  models[[modeliter]]$mcl.hgam <- rep(NA, K)
  models[[modeliter]]$mcl.coef <- matrix(rep(NA, K*p), nrow = p)
  
  while (k <= K) {
    try({
      tic(paste(names(models)[modeliter], '~' , round(k/K*1e2, 1), '%', 
                '(Est. rem. tm.', '=', round((K - k)*median(timer)/60, 1), 'ms)'))
      x.mat0 = rmvn(n, rep(0, p), sigma = diag(1, p))
      exb.vec = exp(drop(x.mat0 %*% b.vec)) ; m.vec = exb.vec
      y.vec0 = rpois(n, m.vec)
      
      x.mat1 = rmvn(n/2, rep(0, p), sigma = diag(1, p))
      exb.vec = exp(drop(x.mat0 %*% b.vec)) ; m.vec = exb.vec
      y.vec1 = rpois(n/2, m.vec)
      
      fold.id <- split(sample(1:n), 1:cvf) ; fid <- 1:n
      for (fi in 1:length(fold.id)) {
        fid[fold.id[[fi]]] <- fi
      } ; fold.id <- fid
      
      cv.lasso.fit <- cv.ncpen(y.vec0, x.mat0, 'poisson', 'lasso', intercept = F, fold.id = fold.id)
      hatgam <- coef(cv.lasso.fit, 'rmse')$lambda # for mcl
      cv.lasso.coef <- coef(cv.lasso.fit, 'rmse')$beta
      haty.vec <- exp(x.mat1 %*% cv.lasso.coef)
      models[[modeliter]]$lasso.MSE[k] <- mean((y.vec1 - haty.vec)^2)
      models[[modeliter]]$lasso.hl[k] <- hatgam
      models[[modeliter]]$lasso.coef[,k] <- cv.lasso.coef
      
      cv.scad.fit <- cv.ncpen(y.vec0, x.mat0, 'poisson', 'scad', intercept = F, fold.id = fold.id)
      cv.scad.coef <- coef(cv.scad.fit, 'rmse')$beta
      haty.vec <- exp(x.mat1 %*% cv.scad.coef)
      models[[modeliter]]$scad.MSE[k] <- mean((y.vec1 - haty.vec)^2)
      models[[modeliter]]$scad.hl[k] <- coef(cv.scad.fit, 'rmse')$lambda
      models[[modeliter]]$scad.coef[,k] <- cv.scad.coef
      
      cv.mcl.fit <- cv.ncpen(y.vec0, x.mat0, 'poisson', 'classo', intercept = F, fold.id = fold.id,
                             gamma = hatgam)
      cv.mcl.coef <- coef(cv.mcl.fit, 'rmse')$beta
      haty.vec <- exp(x.mat1 %*% cv.mcl.coef)
      models[[modeliter]]$mcl.MSE[k] <- mean((y.vec1 - haty.vec)^2)
      models[[modeliter]]$mcl.hgam[k] <- hatgam
      models[[modeliter]]$mcl.hl[k] <- coef(cv.mcl.fit, 'rmse')$lambda
      models[[modeliter]]$mcl.coef[,k] <- cv.mcl.coef
      
      if (n >= p) {
        df0 <- data.frame(y.vec0, x.mat0)
        df1 <- data.frame(y.vec1, x.mat1)
        unres.fit <- glm(y.vec0 ~ . - 1, data = df0, family = 'poisson')
        haty.vec <- predict(unres.fit, df1[,-1])
        models[[modeliter]]$unres.MSE[k] <- mean((y.vec1 - haty.vec)^2)
        models[[modeliter]]$unres.coef[, k] <- coef(unres.fit)
      }
      
      tsq <- toc()
      timer <- c(timer, tsq$toc - tsq$tic)
      k <- k + 1
      boxplot(models[[modeliter]][-(1:2)], main = names(models)[modeliter])})
  }
  save(models, file = paste('/Users/', user, '/Dropbox/Seo&Chang/Simulations2.RData', sep = ''))
}


# Agg ---------------------------------------------------------------------

unres.MSE <- sapply(models, function(x) mean(x$unres.MSE, trim = .1225))
lasso.MSE <- sapply(models, function(x) mean(x$lasso.MSE, trim = .1225))
scad.MSE <- sapply(models, function(x) mean(x$scad.MSE, trim = .1225))
mcl.MSE <- sapply(models, function(x) mean(x$mcl.MSE, trim = .1225))

MCL <- unres.MSE/mcl.MSE
lasso <- unres.MSE/lasso.MSE
SCAD <- unres.MSE/scad.MSE

ML <- lasso.MSE/mcl.MSE
SL <- lasso.MSE/scad.MSE

n <- c(10, 15, 60)*3
df <- t(data.frame(rbind(MCL, lasso, SCAD)))
eg <- expand.grid(q = q, n = n)
df <- cbind(eg, df)
df <- df[, c(2, 1, 3:ncol(df))]
stargazer::stargazer(df, summary = F, rownames = F)

df <- cbind(df[,1:2], ML, SL)
stargazer::stargazer(df, summary = F, rownames = F)

TS <- function(hb, b) {sum(hb[b != 0] != 0)}
FS <- function(hb, b) {sum(hb[b == 0] != 0)}

fs <- ts <- c()
for (ii in 1:length(models)) {
  tb.vec <- models[[ii]]$b.vec
  lass.c <- models[[ii]]$lasso.coef
  mcl.c <- models[[ii]]$mcl.coef
  scad.c <- models[[ii]]$scad.coef
  a <- median(apply(lass.c, 2, TS, b = tb.vec)) ; d <- median(apply(lass.c, 2, FS, b = tb.vec))
  b <- median(apply(mcl.c, 2, TS, b = tb.vec)) ; e <- median(apply(mcl.c, 2, FS, b = tb.vec))
  c <- median(apply(scad.c, 2, TS, b = tb.vec)) ; f <- median(apply(scad.c, 2, FS, b = tb.vec))
  ts <- rbind(ts, c(models[[ii]]$n, length(models[[ii]]$b.vec), a, b, c))
  fs <- rbind(fs, c(models[[ii]]$n, length(models[[ii]]$b.vec), d, e, f))
}
ts <- as.data.frame(ts) ; fs <- as.data.frame(fs)
colnames(fs) <- colnames(ts) <- c('n', 'q', 'MCL', 'lasso', 'SCAD')
stargazer::stargazer(ts, summary = F, rownames = F)
stargazer::stargazer(fs, summary = F, rownames = F)