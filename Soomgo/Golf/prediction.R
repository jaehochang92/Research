wd <- 'GoogleDrive/Research/StatConsult/Soomgo/2006/이민수'
source(file.path(wd, 'methods.R'))

sel <- c(sel, 'wins', 'name')
raw_data[, career := 2020 - rookie_yr]
test.set[, career := 2020 - rookie_yr + 0.5]

# Choose aasociation
ass <- c('KLPGA', 'LPGA')[1]

raw_data_a <- raw_data[association == ass, ..sel] %>% na.omit()
raw_data_a$wins <- (raw_data_a$wins > 0) * 1

test.set <- test.set[association == ass & rank != '-', ]
test.set[, rank := as.numeric(rank)]
test.set <- test.set[rank <= 10 & wins == 0, ]

sel <- sel[sel != 'wins']
train <-
  list(data = raw_data_a[, ..sel], labels = raw_data_a$wins)
test <- list(data = test.set)

# Bagging -----------------------------------------------------------------

train$data %>% select(-name) %>% cbind(., wins = train$labels) %>%
  bagging(
    formula('wins ~ .'),
    .,
    test$data,
    bootstrap_size = nrow(.),
    iterations = 200
  ) -> bagged_prs
colnames(bagged_prs) <- test$data$name

gg <- melt(data.table(bagged_prs)) %>%
  ggplot(aes(x = variable, y = value)) + geom_boxplot() +
  labs(x = '선수명', y = '우승확률') +
  theme_bw(base_size = 15, base_family = 'Iropke Batang Medium')

w <- 10
ggsave(
  file.path(wd, 'results/win-prbs.png'),
  gg,
  width = w,
  height = w / 2,
  dpi = 100
)

win_prbs <- round(apply(bagged_prs, 2, mean) * 100, 2)
win_prbs <-
  data.table(cbind(name = names(win_prbs), win_prbs = win_prbs))
arranged.test.set. <- test.set[, .(name,
                                   putting_avg,
                                   green_in_regulation,
                                   birdies,
                                   driving_accuracy)][win_prbs, on = 'name']
  if (ass == 'LPGA') {
    arranged.test.set. %>% arrange(
            putting_avg,
            desc(green_in_regulation),
            birdies,
            driving_accuracy) -> arranged.test.set.
  } else{
    arrange(., desc(birdies))
  } %>%
  stargazer(.,
            out = file.path(wd, 'results/win-p-t.html'),
            summary = F)

# XGBoost -----------------------------------------------------------------

dtrain <-
  xgb.DMatrix(as.matrix(train$data %>% select(-name)), label = train$label)
xgb.cv.fit <-
  xgb.cv(
    data = dtrain,
    nrounds = 10,
    nthread = 2,
    nfold = 5,
    metrics = list("error"),
    max_depth = 5,
    eta = 1,
    objective = "binary:logistic"
  )
perf_log <-
  xgb.cv.fit$evaluation_log[, .(iter, train_error_mean, test_error_mean)] %>%
  melt(., id.vars = c('iter'))
ggplot(perf_log, aes(x = iter, y = value)) + geom_line(aes(col = variable))

importance_matrix <-
  xgb.importance(colnames(dtrain), model = xgb.cv.fit)

# Ada ---------------------------------------------------------------------

doada <- function(sel,
                  tr_sz,
                  nui,
                  iters,
                  data = for.ada) {
  n <- dim(data)[1]
  trind <- sample(1:n, floor(tr_sz * n), FALSE)
  teind <- setdiff(1:n, trind)
  
  test_x <- data[teind, ..sel]
  test_y <- test_x$wins
  test_x[, wins := NULL]
  
  ##fit 8-split trees
  adalgst <- ada(
    wins ~ .,
    loss = "logistic",
    data = data[trind, ..sel],
    iter = iters,
    nu = nui,
    type = "real"
  )
  
  ##add testing data set
  adalgst = addtest(adalgst, test_x, test_y)
  
  ##plot adalgst
  png(
    file.path(wd, 'results/ada-plot.png'),
    width = w,
    height = h,
    res = resl,
    units = 'cm'
  )
  plot(adalgst, F, TRUE)
  dev.off()
  
  ##variable selection plot
  png(
    file.path(wd, 'results/ada-varplot.png'),
    width = w,
    height = h,
    res = resl,
    units = 'cm'
  )
  varplot(adalgst)
  dev.off()
  
  ##pairwise plot
  png(
    file.path(wd, 'results/ada-pairs.png'),
    width = w,
    height = h,
    res = resl,
    units = 'cm'
  )
  pairs(adalgst, test_x, maxvar = 3)
  dev.off()
  
  return(adalgst)
}

## fit discrete ada boost
##set up testing and training data (60% for training)

raw_data[, career := 2020 - rookie_yr]
for.ada <- raw_data
for.ada$wins <- (for.ada$wins > 0) * 1

tr.err <- ts.err <- 1
while (ts.err >= 0.18 | tr.err >= ts.err) {
  print(ts.err)
  adamodel <- doada(c('wins', 'career', techs), .6, 1e-3, 20)
  ts.err <- tail(adamodel$model$errs[, 3], 1)
  tr.err <- tail(adamodel$model$errs[, 1], 1)
}
saveRDS(adamodel, file.path(wd, '/results/ada-opt-model.rds'))


## predict new obs
adamodel <- readRDS(file.path(wd, '/results/ada-opt-model.rds'))
# new_data??
# predict(adamodel, new_data, type = 'response')