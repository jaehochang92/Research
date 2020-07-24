pckgs <-
  c(
    'corrplot',
    'extrafont',
    'readxl',
    'data.table',
    'ggplot2',
    'dplyr',
    'stargazer',
    'ada',
    'xgboost',
    'doParallel',
    'foreach',
    'caret'
  )
checked <- sapply(pckgs, require, character.only = T)
if (any(checked)) {
  install.packages(names(checked)[checked == F])
}

resl <- 100
w <- 20
h <- 20


# data --------------------------------------------------------------------

ssnresult <- c('rank',
               'prize')
perfs <- c('scoring_avg',
           'wins',
           'plays',
           'top10finish_percentile')
techs <- c(
  'driving_accuracy',
  'driving_distance',
  'green_in_regulation',
  'putting_avg',
  'birdies'
)

read.n.process <- function(f.path = "raw_data.xlsx") {
  if (stringi::stri_detect(f.path, fixed = '.xlsx')) {
    raw_data <-
      read_excel(file.path(wd, f.path)) %>% arrange(name, year)
    raw_data <- data.table(raw_data)
  } else if (stringi::stri_detect(f.path, fixed = '.csv')) {
    raw_data <-
      fread(file.path(wd, f.path), encoding =) %>% arrange(name, year)
  } else {
    break
  }
  raw_data[, prize := round(prize)]
  raw_data[, prize := prize / 10 ^ 8] # 단위 = 억원
  return(raw_data)
}

raw_data <- read.n.process()
test.set <- read.n.process('testset.xlsx')

# data.remove.Autocorr ----------------------------------------------------

vars <-
  c(ssnresult, perfs, techs, 'rookie_yr')
cmmnd <- paste(vars, vars, sep = '=mean(')
cmmnd <- paste(cmmnd, ')', sep = '')
cmmnd <- paste(cmmnd, collapse = ', ')
cmmnd <- paste('.(', cmmnd, ')', sep = '')
cmmnd <-
  paste("raw_data[, ",
        cmmnd,
        ", by = .(name, association)]",
        sep = '')
assign('raw_data', eval(parse(text = cmmnd)))
# assign('test.set', eval(parse(text = cmmnd)))


# Desc --------------------------------------------------------------------
#
# stargazer(
#   raw_data,
#   raw_data[association == 'KLPGA', ],
#   raw_data[association == 'LPGA', ],
#   digits = 2,
#   title = c('KLPGA & LPGA', 'KLPGA', 'LPGA'),
#   out = file.path(wd, 'desc.html')
# )

# Corrs -------------------------------------------------------------------

col1 <- colorRampPalette(c('darkgray', 'white', 'darkgray'))

plot.save.cor <-
  function(for.corr, wd) {
    c.mtest <- for.corr %>% cor.mtest(conf.level = 0.95)
    corr <- cor(for.corr, use = 'complete.obs')
    png(
      file.path(wd, 'corrplot.png'),
      width = w,
      height = h,
      res = resl,
      units = 'cm'
    )
    corrplot(
      corr,
      cl.lim = c(-1, 1),
      col = col1(200),
      p.mat = c.mtest$p,
      order = 'FPC',
      method = c("square", "ellipse", "number", "pie", "shade", "color")[2],
      sig.level = 0.05,
      insig = "blank",
      addCoef.col = 'black'
    )
    dev.off()
  }

# diagnosis of OLS --------------------------------------------------------

diag.save.lm <- function(lm.fit, fname, wd) {
  png(
    file.path(wd, paste(fname, '-lm-diag.png', sep = '')),
    width = w,
    height = h,
    res = resl,
    units = 'cm'
  )
  par(mfrow = c(2, 2))
  plot(lm.fit)
  dev.off()
  capture.output({
    'DW-autocorrelation test' %>% print
    car::durbinWatsonTest(lm.fit) %>% print
    'VIF' %>% print
    car::vif(lm.fit) %>% print
  }, file = file.path(wd, paste(fname, '-lm-diag.txt', sep = '')))
}

# diagnosis of GLM --------------------------------------------------------

diag.save.glm <- function(glm.fit, fname, wd) {
  png(
    file.path(wd, paste(fname, '-glm-diag.png', sep = '')),
    width = w,
    height = h,
    res = resl,
    units = 'cm'
  )
  # glm.fit.diag <- boot::glm.diag(glm.fit)
  # boot::glm.diag.plots(glm.fit, glm.fit.diag, iden = F)
  # car::influencePlot(glm.fit)
  plot(glm.fit, which = 4)
  dev.off()
  if (length(glm.fit$coefficients) > 2) {
    capture.output({
      'VIF' %>% print
      car::vif(glm.fit) %>% print
    }, file = file.path(wd, paste(fname, '-lm-diag.txt', sep = '')))
  }
}


# Bagging -----------------------------------------------------------------

bagging <- function(form,
                    training,
                    testing,
                    bootstrap_size = 100,
                    iterations = 1000)
{
  predictions <- foreach(m = 1:iterations, .combine = rbind) %do% {
    training_positions <-
      sample(nrow(training), size = bootstrap_size, replace = T)
    train_pos <- 1:nrow(training) %in% training_positions
    glm_fit <-
      glm(form, data = training[train_pos, ], family = 'binomial')
    predict(glm_fit, newdata = testing, type = 'response')
  }
  predictions
}