libs <- c('data.table', 'dplyr', 'stringr')
sapply(libs, require, character.only = T)

party <- fread('Dropbox/HCLee/data/party.csv')
hwc <- fread('Dropbox/HCLee/data/meetinglog20thHWC.csv', encoding = 'UTF-8')
hwc <- hwc[, colnames(hwc)[summarise_all(hwc, funs(n_distinct)) != 1], with = F]

# table(hwc[,SPEAKER]) %>% View

custom1 <- function(x) {
  tmp <- unname(unlist(strsplit(x, split = ' ')))
  tmp <- tmp[str_length(tmp) == 3 & !tmp %in% c('위원장', '진술인', '참고인')]
  return(tmp)
}

hwc[,custom1(SPEAKER)] %>% View
hwc[,NAME := custom1(SPEAKER)]
setkey(hwc, NAME)
setkey(party, NAME)
hwc[party] %>% View

table(hwc[,NAME]) %>% View
hwc <- party[hwc][,NAME := NULL]
hwc <- select(hwc, CONFER_NUM:SPEAKER, PARTY, everything())
# hwc[,SPEAKER := gsub('위원', '의원', SPEAKER),]

# fwrite(hwc, 'Dropbox/HCLee/data/meetinglog20thHWC(N).csv')