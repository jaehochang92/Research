pkgs <- c('dplyr', 'ggplot2', 'reshape2', 'car', 'stargazer', 'lubridate')

installed <- sapply(pkgs, require, character.only = T)
install.packages(
  pkgs[!installed],
  repos = getOption("repos"),
  available = NULL,
  destdir = NULL,
  dependencies = T,
  type = getOption("pkgType"),
  configure.args = getOption("configure.args"),
  configure.vars = getOption("configure.vars"),
  clean = FALSE,
  Ncpus = getOption("Ncpus", 1L),
  verbose = T,
  quiet = T,
  keep_outputs = FALSE
)
sapply(pkgs, require, character.only = T)