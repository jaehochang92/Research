libs <- c('wakefield', 'pacman', 'knitr', 'MatchIt', 'data.table', 'IRdisplay', 'dplyr', 'kableExtra')
sapply(libs, require, character.only = T)

tmt <- fread('~/GoogleDrive/Research/StatConsult/2005StephenAhn/TMT_OS_20APR28_final.csv')
tmt[, PFS:=NULL]
tmt$TMT9.2 <- ifelse(tmt$TMT9.2 == 'Y', 1, 0)

pacman::p_load(tableone)
table1 <- CreateTableOne(vars = c('AGE65', 'SURGERY', 'ECOGH'), 
                         data = tmt, 
                         factorVars = c('AGE65', 'SURGERY', 'ECOGH'), 
                         strata = 'TMT9.2')

table1 <- print(table1, 
                printToggle = FALSE, 
                noSpaces = TRUE)

kable(table1[,1:3], 'html', align = 'c', 
      caption = 'Table 1: Comparison of unmatched samples') %>% 
as.character() %>% display_html()

match.it <- matchit(TMT9.2 ~ AGE65 + SURGERY + ECOGH,
                    data = tmt, method = "nearest")
summary(match.it)

methods(matchit)

plot(match.it, type = 'jitter')
plot(match.it, type = 'hist')

df.match <- match.data(match.it)[1:ncol(tmt)]

table4 <- CreateTableOne(vars = c('AGE65', 'SURGERY', 'ECOGH'), 
                         data = df.match, 
                         factorVars = c('AGE65', 'SURGERY', 'ECOGH'), 
                         strata = 'TMT9.2')
table4 <- print(table4, 
                printToggle = FALSE, 
                noSpaces = TRUE)

kable(table4[,1:3],  
      align = 'c', 
      caption = 'Table 4: Comparison of matched samples') %>% 
as.character() %>% display_html()

fwrite(df.match, '~/GoogleDrive/Research/StatConsult/2005StephenAhn/matchedata.csv')
