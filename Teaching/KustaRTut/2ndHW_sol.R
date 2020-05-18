require(data.table)
require(dplyr)
ONP <- fread('./Dropbox/KustaRTut/data/OnlineNewsPopularity.csv')
ONP <- sample_n(ONP, 5e2)

summarise_all(ONP, funs(n_distinct))

# ex.3 --------------------------------------------------------------------

ex3.cols <- summarise_all(ONP, funs(n_distinct)) %>% 
  select_if(., function(x) x > 2 & x <= 50) %>% 
  colnames()

ex3 <- select(ONP, ex3.cols)
nc <- ncol(ex3)
par(mfrow = c(3, 3))
for (i in 1:nc) {
  hist(ex3[[i]], main = ex3.cols[i])
}

# ex.4 --------------------------------------------------------------------

trn <- ONP[url %in% sample(url, nrow(ONP)*.7),]
tst <- ONP[!url %in% trn$url,]

(cvids <- rep(1:5, length.out = nrow(trn)) %>% sample)
mutate(trn, cvid = cvids) %>% 
  group_by(cvid) %>% summarise(., n())
trn[, cvid := cvids][, .N, by = cvid]

# ex.6 --------------------------------------------------------------------

select(ONP, contains('LDA')) %>% summary

# ex.7 --------------------------------------------------------------------

cust1 <- function(x) {(x > median(x))*1}
ONP2 <- mutate_at(ONP, vars(LDA_00:LDA_04), funs(dum = cust1))
ONP2 <- select(ONP2, -url, -(LDA_00:LDA_04))
grpAIC <- group_by(ONP2, LDA_01_dum, LDA_00_dum) %>%
  do(model = step(lm(shares ~ 1, data = ONP2),
                  scope = list(upper = lm(shares ~ ., data = ONP2),
                               lower = lm(shares ~ 1, data = ONP2)),
                  direction = 'forward'))

grpAIC %>% do(data.frame(
  LDA_00 = .$LDA_00_dum,
  LDA_01 = .$LDA_01_dum,
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
) %>% View