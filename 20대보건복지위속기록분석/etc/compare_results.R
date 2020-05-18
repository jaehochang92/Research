{
  require(dplyr)
  require(data.table)
  require(stringr)
  rem <- c()
  for (c in list.files('./GoogleDrive/')) {
    if (regexpr('gsheet', c) != -1) {
      rem <- c(rem, gsub('.gsheet', '', c))
    }
  }
  ctable = fread('GoogleDrive/Research/HCLee/result/optparset.csv')
  ctable = ctable[file %in% rem, ]
  save <- c()
  for (f in c('보건복지', '총선')) {
    idx <- regexpr(f, ctable$file) != -1
    ti <- ctable[idx, ]
    save <- arrange(ti, desc(coherence)) %>% head(., 5) %>% rbind(., save)
  }
  save %>% View(., 'save')
}
# For reset
ctable = ctable[1,][-1,]
# fwrite(ctable, 'GoogleDrive/Research/HCLee/result/optparset.csv')
prv <- fread('GoogleDrive/Research/HCLee/result/my_optparset.csv')