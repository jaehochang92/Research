require(dplyr)
require(stringr)
texmat <- function(x) {
  # ncol;elements...
  ncol <- strsplit(x, ';')[[1]][1] %>% as.numeric
  elm <- strsplit(x, ';')[[1]][2] %>% {
    strsplit(., '')[[1]]
  }
  nrow <- length(elm) / ncol
  begin <- paste(rep('c', ncol), collapse = '') %>%
    paste('\\begin{array}{', ., '}', sep = '')
  body <- ''
  if (ncol == 1) {
    body <- paste(elm, "\\\\", sep = '') %>% paste(., collapse = '')
    body <- substr(body, 1, str_length(body) - 2)
  }
  if (ncol > 1) {
    for (i in 1:nrow) {
      body <- paste('&', elm[1:ncol + ncol * (i - 1)], sep = '') %>%
        paste(., collapse = '') %>% paste(body, ., sep = '\\\\\n')
    }
    body <- gsub('\\n&', '', body) %>% substr(., 3, str_length(.))
  }
  end <- '\\end{array}'
  paste(begin, body, end, sep = '\n') %>% paste('\\left[', ., sep = '') %>%
    paste(., '\\right]', sep = '') %>% message
}

x <- sample(0:2, 12, replace = T)
X <- matrix(x, ncol = 3, byrow = T)
{
  XtX <- t(X) %*% X
} %>% eigen()
paste(x, collapse = '') %>% paste('3;', ., sep = '') %>% texmat()
paste(XtX, collapse = '') %>% paste('3;', ., sep = '') %>% texmat()
eigen(XtX)$values %>% diag %>% paste(., collapse = '') %>% paste('3;', ., sep = '') %>% texmat()