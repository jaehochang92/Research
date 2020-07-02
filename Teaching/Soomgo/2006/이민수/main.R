wd <- 'GoogleDrive/Research/StatConsult/Soomgo/2006/이민수'
source(file.path(wd, 'methods.R'))

# Corrs -------------------------------------------------------------------

vars <- vars[vars != 'rookie_yr']
for.corr <- raw_data[, ..vars]
plot.save.cor(for.corr, file.path(wd, 'results'))

# OLS ---------------------------------------------------------------------

for (ass in list('KLPGA', 'LPGA', c('KLPGA', 'LPGA'))[3]) {
  if (length(ass) > 1) {
    assn <- paste(ass, collapse = '&')
  } else{
    assn <- ass
  }
  for.strgz1 <- list()
  for (y in ssnresult[1]) {
    vars <- c(techs, y, 'name')
    for.reg <-
      raw_data[association %in% ass, ..vars] %>% na.omit %>% as.data.table
    # remove outlier/influential
    for.reg <- for.reg[name != 'Sarah Jane Smith', ]
    
    rownames(for.reg) <- for.reg$name
    lower <- as.formula(paste(y, '~ 1', sep = ''))
    linear <-
      paste(names(for.reg)[!names(for.reg) %in% c(y, 'name')], collapse = '+')
    # interactions <- paste(techs[techs != y], collapse = '*')
    upper <-
      as.formula(paste(y, '~', linear, sep = ''))
    lm.base <- lm(lower, data = na.exclude(for.reg))
    lm.step <-
      MASS::stepAIC(
        lm.base,
        scope = list(lower = lower, upper = upper),
        direction = 'forward',
        trace = F
      )
    steps <- lm.step$anova$Step
    
    for.strgz2 <- list(lm(lower, data = na.exclude(for.reg)))
    form <- lower
    if (length(steps) > 1) {
      for (i in 2:length(steps)) {
        cmmnd <- paste('update(form, ~ . ', steps[i], ')', sep = '')
        assign('form', eval(parse(text = cmmnd)))
        print(form)
        for.strgz2[[i]] <- lm(form, data = na.exclude(for.reg))
      }
    }
    diag.save.lm(for.strgz2[[i]],
                 paste(assn, '-on-', y, sep = ''),
                 file.path(wd, 'results'))
    for.strgz1[[assn]][[y]] <- for.strgz2
  }
  stargazer(
    for.strgz1,
    digits = 2,
    title = assn,
    df = F,
    out = file.path(
      file.path(wd, 'results'),
      paste(assn, '-step-lm.html', sep = '')
    )
  )
}

# Logistic Reg ------------------------------------------------------------

for.lgst <- raw_data
for.lgst$wins <- (for.lgst$wins > 0) * 1
for.strgz1 <- list()
for (ass in list('KLPGA', 'LPGA')) {
  if (length(ass) > 1) {
    assn <- paste(ass, collapse = '&')
  } else{
    assn <- ass
  }
  y <- 'wins'
  vars <- c(techs, y, 'name')
  for.reg <-
    for.lgst[association %in% ass, ..vars] %>% na.omit %>% as.data.table
  # remove outlier/influential
  for.reg <- for.reg[!name %in% c('양희영'),]
  #
  rownames(for.reg) <- for.reg$name
  lower <- as.formula(paste(y, '~ 1', sep = ''))
  linear <-
    paste(names(for.reg)[names(for.reg) != y], collapse = '+')
  # interactions <- paste(techs[techs != y], collapse = '*')
  upper <-
    as.formula(paste(y, '~', linear, sep = ''))
  glm.base <-
    glm(lower, family = binomial, data = na.exclude(for.reg))
  glm.step <-
    MASS::stepAIC(
      glm.base,
      scope = list(lower = lower, upper = upper),
      direction = 'forward',
      trace = F
    )
  steps <- glm.step$anova$Step
  
  for.strgz2 <-
    list(glm(lower, family = binomial, data = na.exclude(for.reg)))
  form <- lower
  if (length(steps) > 1) {
    for (i in 2:length(steps)) {
      cmmnd <- paste('update(form, ~ . ', steps[i], ')', sep = '')
      assign('form', eval(parse(text = cmmnd)))
      print(form)
      for.strgz2[[i]] <-
        glm(form, family = binomial, data = na.exclude(for.reg))
    }
  }
  diag.save.glm(for.strgz2[[i]],
                paste(assn, '-on-', y, sep = ''),
                file.path(wd, 'results'))
  for.strgz1[[assn]] <- for.strgz2
  print(for.strgz1[[assn]])
}
stargazer(
  for.strgz1$KLPGA,
  for.strgz1$LPGA,
  digits = 2,
  title = c('KLPGA', 'LPGA'),
  df = F,
  # ci = TRUE,
  # ci.level = 0.95,
  # single.row = F,
  out = file.path(file.path(wd, 'results'),
                  paste('step-lgst.html', sep = ''))
)