source('04SourceCodes/Initiator_1.0.0.R')
sapply(lib,require,character.only=T)
options(scipen = 100)

## 2. Univariate Analysis ------------------------------------------------------------------

datalist <- preparedata('', pool = F) # MDR Type invariant
summary(datalist)
inputdata <- datalist$inputdata;inputdata_num <- datalist$inputdata_num;inputdata_fact <- datalist$inputdata_fact
labels <- datalist$labels

# 2.1 Bar charts ---------------------------------------------------------

barcharts <- function(r1=5,c1=7,r2=3,c2=4){
  a <- c()
  for(i in 1:dim(inputdata_fact)[2]){
    a <- append(a,ifelse(dim(unique(inputdata_fact[,..i]))[1]>2,i,next))
  } # : detect the location of column with distinct>3
  cn <- colnames(inputdata_fact)[a] # distinct>3
  
  # Barcharts for factorial variables
  theme_bw()
  bardf <- inputdata_fact[,cn,with=F]
  
  cn <- colnames(inputdata_fact)[!colnames(inputdata_fact)%in%cn] # distinct<=3
  page <- 0:(ncol(inputdata_fact)/(r1*c1))
  for(p in page){
    v <- 1:(r1*c1)+(r1*c1)*p ; k <- 1
    for(j in v){
      if(!is.na(cn[j])){
        nam <- paste("bar", 1:(r1*c1), sep = "")
        dt <- data.table(table(inputdata_fact[,cn[j],with=F])) ; colnames(dt) <- c('Group','value')
        bar <- ggplot(dt, aes(x=Group, y=value)) + geom_bar(stat = "identity") + ylab('Freq') + xlab(cn[j])
        assign(nam[k], bar)
        k <- k+1
      }else{
        next
      }
    }
    # paste(nam,collapse = ',')
    ga <- grid.arrange(bar1,bar2,bar3,bar4,bar5,bar6,bar7,bar8,bar9,bar10,bar11,bar12,bar13,bar14,bar15,
                       bar16,bar17,bar18,bar19,bar20,bar21,bar22,bar23,bar24,bar25,bar26,bar27,bar28,
                       bar29,bar30,bar31,bar32,bar33,bar34,bar35,ncol=c1, nrow=r1)
    ggsave(paste('FactBarChartpg',p,'.png',sep = ''),ga,width = 25.7,height = 16,units = 'cm',dpi = 300,limitsize = F) ; gc()
  }
  # Barcharts for numeric variables
  theme_bw()
  cn <- colnames(inputdata_num)
  page <- 0:(ncol(inputdata_num)/(r2*c2))
  for(p in page){
    v <- 1:(r2*c2)+(r2*c2)*p ; k <- 1
    for(j in v){
      if(!is.na(cn[j])){
        nam <- paste("bar", 1:(r2*c2), sep = "")
        dt <- data.table(table(inputdata_num[,cn[j],with=F])) ; colnames(dt) <- c('Group','value')
        bar <- ggplot(dt, aes(x=Group, y=value)) + geom_bar(stat = "identity") + ylab('Freq') + xlab(cn[j])
        assign(nam[k], bar)
        k <- k+1 
      }else{
        next
      }
  }
  # paste(nam,collapse = ',')
  ga <- grid.arrange(bar1,bar2,bar3,bar4,bar5,bar6,bar7,bar8,bar9,bar10,bar11,bar12,
                     ncol=c2, nrow=r2)
  ggsave(paste('NumBarChartpg',p,'.png',sep = ''),ga,width = 25.7,height = 16,units = 'cm',dpi = 300) ; gc()
  }
  print('Plot saving done')
}
barcharts()

# 2.2 Dates hist. / Vio + Box plot (deprecated) ---------------------------

# theme_set(theme_bw())
# dates <- data.frame(MED_DATE=ymd(inputdata[,MED_DATE]))
# gg <- ggplot(dates) + geom_bar(aes(x=MED_DATE)) + scale_x_date(labels = date_format("%Y-%m"))
# ggsave('ggsave.png',gg,width = 25.7,height = 16,units = 'cm',dpi = 300,limitsize = F) ; gc()
# 
# cn <- colnames(inputdata_num)[2:3]
# nam <- paste("box", 1:2, sep = "")
# k <- 1
# for(j in 1:2){
#   df <- data.frame(Variable=cn[j],value=inputdata_num[,cn[j],with=F]) ; colnames(df)[2] <- 'value'
#   box <- ggplot(df, aes(x=Variable,y=value)) + 
#     geom_violin(fill='lightblue') + 
#     geom_boxplot(fill='lightgreen',width=.05)
#   assign(nam[k], box)
#   k <- k+1
# }
# ga <- grid.arrange(box1,box2,ncol=2)
# ggsave('ggsave.png',ga,width = 25.7,height = 16,units = 'cm',dpi = 300)


# 2.3 Descriptives for nums ---------------------------------------------

inputdata_num_desc <- psych::describe(inputdata_num[,])
stargazer(as.data.frame(inputdata_num_desc),out = 'numdsc.html',summary = F)

# 2.4.1 Chisq + G-test versus MDR type -----------------------------

ChsqGT <- function(mdr, opt){
  
  datalist <- preparedata(mdr, pool = F) # Same for all types
  inputdata <- datalist$inputdata;inputdata_num <- datalist$inputdata_num;inputdata_fact <- datalist$inputdata_fact
  labels <- datalist$labels
  
  j1 <- c() ; j2 <- c()
  for(i in 1:ncol(inputdata_fact)){
    if(dim(unique(inputdata_fact[,..i]))[1]>2){
      j1 <- append(j1, i) # Multinomial variable locations
    }else if(dim(unique(inputdata_fact[,..i]))[1]==1){
      j2 <- append(j2, i) # Single-valued variable locations
    }else{next}
  }
  
  type = paste('MDR_TYPE_',mdr,sep = '')
  
  if(opt=='bin'){
    df <- inputdata_fact[,-c(j1,j2),with=F][,Y:=labels[,type,with=F]]
    cn <- colnames(df)
    bns <- colnames(df)
    bna <- data.frame(1,1,1,1,1,1)
    colnames(bna) <- c('Cols','x2pv','g2pv','OddsR','OR 2.5%','OR 97.5%')
    for(i in 1:(length(bns)-1)){
      bna[i,1] <- bns[i]
      ba_df <- dplyr::select(df,bns[i],bns[length(bns)])
      ba_tb <- table(ba_df) ; ba_x2 <- chisq.test(ba_tb)
      G <- GTest(ba_tb, correct="none")
      prmd <- glm(Y~.,data = ba_df,family = binomial('logit'))
      bna$x2pv[i] <- ba_x2$p.value ; bna$g2pv[i] <- G$p.value ; bna$OddsR[i] <- exp(prmd$coefficients)[2]
      bna$`OR 2.5%`[i] <- exp(confint.default(prmd))[2,1]
      bna$`OR 97.5%`[i] <- exp(confint.default(prmd))[2,2]
    } ; bna[,2:4] <- round(bna[2:4],4) ; setDT(bna)
    bna <- bna[order(-rank(x2pv),-rank(g2pv),-rank(OddsR)),]
    return(bna)
  }else if(opt=='mult'){ # OvR analysis
    df <- as.data.frame(cbind(inputdata_fact[,j1,with=F])) # data generation
    df <- dummy_cols(df,select_columns = colnames(inputdata_fact)[j1]) ; setDT(df)
    df[,eval(parse(text = paste("':='(",paste(paste(colnames(inputdata_fact)[j1],'=',"NULL",sep = ''),collapse = ','),')',sep = '')))][,Y:=labels[,type,with=F]]
    cn <- colnames(df) ; bns <- colnames(df) ; bna <- data.frame(1,1,1,1,1,1)
    colnames(bna) <- c('Cols','x2pv','g2pv','OddsR','OR 2.5%','OR 97.5%')
    
    for(i in 1:(length(bns)-1)){
      bna[i,1] <- bns[i]
      ba_df <- dplyr::select(df,bns[i],bns[length(bns)])
      ba_tb <- table(ba_df) ; ba_x2 <- chisq.test(ba_tb)
      G <- GTest(ba_tb, correct="none")
      prmd <- glm(Y~.,data = ba_df,family = binomial('logit'))
      bna$x2pv[i] <- ba_x2$p.value ; bna$g2pv[i] <- G$p.value ; bna$OddsR[i] <- exp(prmd$coefficients)[2]
      bna$`OR 2.5%`[i] <- exp(confint.default(prmd))[2,1]
      bna$`OR 97.5%`[i] <- exp(confint.default(prmd))[2,2]
    } ; bna[,2:4] <- round(bna[2:4],4) ; setDT(bna)
    bna <- bna[order(-rank(x2pv),-rank(g2pv),-rank(OddsR)),]
    return(bna)
  }
}

ChsqGT('MRSA','bin') %>% fwrite(.,'UnivtestMRSA_bin.csv')
ChsqGT('MRSA','mult') %>% fwrite(.,'UnivtestMRSA_mult.csv')
ChsqGT('VRE','bin') %>% fwrite(.,'UnivtestVRE_bin.csv')
ChsqGT('VRE','mult') %>% fwrite(.,'UnivtestVRE_mult.csv')


RFassc <- function(th){
  
  # 2.4.2 Chisq + G-test : RFs Associations ------------------------------------------------
  
  newdata <- inputdata_fact
  cn <- colnames(newdata) # 1. convert the data as a table
  
  j2 <- c()
  for(i in 1:ncol(newdata)){
    if(dim(unique(newdata[,..i]))[1]==1){
      j2 <- append(j2, i) # Single-valued variable locations
    }else{next}
  } ; newdata <- newdata[,-j2,with=F]
  
  # mx <- data.table(X1=NA,X2=NA,X2pv=NA,G2pv=NA)
  # for(i in 1:(ncol(newdata)-1)){
  #   for(j in (i+1):ncol(newdata)){
  #     tb <- table(newdata[,c(i,j),with=F])
  #     gt <- GTest(tb,correct = 'none')
  #     ct <- chisq.test(tb)
  #     mx <- rbind(mx,t(c(X1=names(dimnames(tb))[1],
  #                        X2=names(dimnames(tb))[2],
  #                        X2pv=ct$p.value[[1]], G2pv=gt$p.value[[1]])))
  #   }
  # }
  # mx <- mx[-1]

  pvmx <- matrix(0, nrow = length(cn), ncol = length(cn))
  colnames(pvmx)=cn ; rownames(pvmx)=cn
  for(i in 1:(ncol(newdata)-1)){
    for(j in (i+1):ncol(newdata)){
      tb <- table(newdata[,c(i,j),with=F])
      gt <- GTest(tb,correct = 'none')
      pvmx[i,j] <- gt$p.value
      pvmx[j,i] <- gt$p.value
    }
  }
  bf <- 0.05/(ncol(inputdata_fact)*(ncol(inputdata_fact)-1)/2) # https://en.wikipedia.org/wiki/Bonferroni_correction
  pvmx <- pvmx<bf ; diag(pvmx) <- NA
  pvmx <- ifelse(pvmx==T,1,0)
  par(mar = c(1,2,5,2))
  gg <- ggcorrplot(pvmx,title = 'Whether two variables are statistically independent (BF corrected)',
                   legend.title = 'indp.=0')
  ggsave('Gtpvplot.png',gg,dpi = 500,height = 50,width = 100, units = 'cm')
  print('G-test p-value plot done')
  
  # 2.4.3 Phi coefficients --------------------------------------------------
  
  # Pearson's Phi coef.
  phimx <- matrix(0, nrow = length(cn), ncol = length(cn))
  colnames(phimx)=cn ; rownames(phimx)=cn
  for(i in 1:(ncol(newdata)-1)){
    for(j in (i+1):ncol(newdata)){
      tb <- table(newdata[,c(i,j),with=F])
      gt <- GTest(tb,correct = 'none')
      ct <- chisq.test(tb)
      phimx[i,j] <- phimx[j,i] <- 
        (prod(diag(tb))-tb[1,2]*tb[2,1])/(sqrt(sum(tb[1,])*sum(tb[2,]))*sqrt(sum(tb[,1])*sum(tb[,2])))
    }
  }
  
  v <- c()
  for(i in 1:nrow(phimx)){
    tmp <- c()
    for(j in 1:ncol(phimx)){
      if(abs(phimx[i,j])>th){
        tmp <- append(tmp, colnames(phimx)[j])
      }else{}
    }
    if(is.null(tmp)){
      next
    }else{
      names(tmp) <- rep(rownames(phimx)[i],length(tmp))
      v <- append(v, tmp)
    }
  } ; v <- na.omit(v)
  
  phimx <- phimx[,(apply(phimx, 2, function(x){sum(x>th)})!=0)]
  phimx <- phimx[1:ncol(phimx),]
  
  par(mar = c(1,2,5,2))
  gg <- ggcorrplot(phimx,title = paste('Pearson Phi plot wtih threshold = ',th,sep=''),
                   legend.title = expression(phi))
  ggsave('Phiplot.png',gg,dpi = 500,height = 50,width = 100, units = 'cm')
  return(phimx)
}
phimx <- RFassc(.3)

# 2.5 MEDDEPT vs MDR (deprecated) -----------------------------------------

# df <- newdata[,.(MEDDEPT)] ; df[,Y:=labels[,MDR_TYPE_MRSA]] # MEDDEPT vs is_MRSA
# dt <- table(df)
# 
# x2 <- chisq.test(dt)
# G <- GTest(dt, correct="none") ; G # "none" "williams" "yates"
# t(abs(x2$residuals)>2) ; t(x2$residuals) #
# sql_temp <- as.data.frame.matrix(x2$residuals) ; sql_temp$rnms = rownames(x2$residuals)
# sql_temp <- sqldf('select * from sql_temp order by TRUE desc, FALSE desc')
# # stargazer(sql_temp,out = 'sql_temp.html',summary = F,title = 'MEDDEPT vs isMRSA')
# 
# corrplot(x2$residuals, is.cor = FALSE, # 2. Graph
#          main = expression(chi^2~~'Residuals of MEDDEPT with is_MRSA'),
#          mar = c(1,2,5,2))
# # Positive residuals are in blue.
# # Positive values in cells specify an attraction (positive association) between
# # the corresponding row and column variables.
# # Negative residuals are in red.
# # This implies a repulsion (negative association) between the corresponding
# # row and column variables.
# 
# pha <- data.frame(1,1,1) ; colnames(pha) <- c('Dept','x2pv','g2pv')
# dept <- as.character(c(unique(df[,1,with=F]))[[1]])
# 
# for(i in 1:length(dept)){ # OvR
#   df <- newdata[,.(MEDDEPT)] ; df[,Y:=labels[,MDR_TYPE_MRSA]] # MEDDEPT vs is_MRSA
#   dept <- as.character(c(unique(df[,1,with=F]))[[1]])
#   pha[i,1] <- dept[i]
#   ph_d <- df
#   ph_d[,MEDDEPT:=ifelse(MEDDEPT==dept[i],dept[i],paste('Not_',dept[i],sep = ''))]
#   ph_dt <- table(ph_d) ; ph_x2 <- chisq.test(ph_dt)
#   G <- GTest(ph_dt, correct="none")
#   pha$x2pv[i] <- ph_x2$p.value ; pha$g2pv[i] <- G$p.value
#   ph_x2$residuals
#   print(ph_dt)
# } ; pha[,2:3] <- round(pha[2:3],5) ; setDT(pha)
# pha <- pha[order(-rank(x2pv),-rank(g2pv)),]
# # stargazer(pha,out='pha.html',summary = F)
# 
# sig_dt <- dt[rownames(dt)[(apply(abs(x2$residuals)>2,MARGIN = 1,FUN = sum)!=0)],]
# sig_x2 <- chisq.test(sig_dt)
# t(abs(sig_x2$residuals)>2) ; t(sig_x2$residuals)
# 
# corrplot(sig_x2$residuals, is.cor = FALSE, # 2. Graph
#          title = expression(chi^2~~'Residuals of significant MEDDEPT with is_MRSA'),
#          mar = c(1,2,5,2))