source('04SourceCodes/Initiator_1.0.0.R')
sapply(lib,require,character.only=T)
options(scipen = 100)

# 1. RF Detection + MDR Prediction --------------------------------------------------

# 3. Multivariate Analysis -----------------------------------------------

# 3.1 Numericals -------------------------------------------------------

# 3.1.1 Correlations ----------------------------------------------------

mdr <- 'MRSA'
mdr <- 'VRE'

datalist <- preparedata(mdr)
summary(datalist)
inputdata <- datalist$inputdata;inputdata_num <- datalist$inputdata_num;inputdata_fact <- datalist$inputdata_fact
labels <- datalist$labels

corr.data <- inputdata_num
p.corm <- cor(corr.data,method = 'pearson')
p.mat <- cor_pmat(corr.data,method='pearson')

ggcor <- ggcorrplot(p.corm, hc.order=T, type = 'lower', lab = T,p.mat = p.mat,
                    title = "Pearson's r")
ggsave('ggcor_prs.png',ggcor,dpi = 500,height = 15,width = 15)

# Logistic Scenario -------------------------------------------------------

mdr <- 'MRSA'
mdr <- 'VRE'

datalist <- preparedata(mdr)
summary(datalist)
inputdata <- datalist$inputdata;inputdata_num <- datalist$inputdata_num;inputdata_fact <- datalist$inputdata_fact
labels <- datalist$labels

sz <- 2000
zr <- .3 ; classprop <- c(zr,1-zr)
emprnk <- logistic.emp.rnk(sz = sz, p = 1, k = 50, mdr = mdr, classprop = classprop)
load('Logist/MRSAemp.RData')

# 
Go <- function(tryzr){
  sz <- 2000
  zr <- tryzr ; classprop <- c(zr,1-zr)
  final <- logistic.final(mdr = mdr, emprnk = emprnk, p = .7,
                          sz = sz, j = 15, k = 40, classprop = classprop, coff = .5)
  ul <- unlist(final$Perf)
  perf <- (data.table(V=names(ul),v=as.numeric(unname(ul))))
  perf <- perf[V%in%c('overall.Accuracy','byClass.Sensitivity','byClass.Specificity',
                      'byClass.Precision','byClass.F1'),]
  print(paste('Median performances of 40 models with Zero prop. = ',tryzr,sep = ''))
  print(perf[,median(v),by=V])
  return(final)
}
final <- Go(.5)
idx <- which.max(unlist(final$Perf)[names(unlist(final$Perf))%in%'byClass.Sensitivity'])[[1]]

# For Documentation
# stargazer(final$model[[idx]],single.row = T,out = 'tmp.html')
# paste('logit(P(Y = ',mdr,')) = ',
#       paste(paste(paste(round(final$fin.model$coefficients,5)),
#                   names(final$fin.model$coefficients),sep = '*'), collapse = ' + '), sep = '')


# 4. ElNet -------------------------------------------------------------

pkgs <- list("glmnet", "doParallel", "foreach", "pROC",'lars')
sapply(pkgs, require, character.only = T)

lass <- list() ; ridg <- list() ; lass.auc <- c() ; ridg.auc <- c()
set.seed(20180905) # Fix seed
for(i in 1:1){
  print(i)
  EN <- ElNet(mdr = 'VRE',p = .7,sz = 1000)
  predicted <- unname(predict(EN$lasso$model[[1]],EN$newX,type = 'response'))
  optCutOff <- optimalCutoff(EN$newY, predicted)[1]
  prd <- as.factor((predicted>optCutOff)*1) ; levels(prd) <- c('0','1')
  lass.cm <- confusionMatrix(prd, EN$newY, positive = '1')
  lass.auc <- c(lass.auc, ModelMetrics::auc(EN$newY,prd))
  
  predicted <- unname(predict(EN$ridge$model[[1]],EN$newX,type = 'response'))
  optCutOff <- optimalCutoff(EN$newY, predicted)[1]
  prd <- as.factor((predicted>optCutOff)*1) ; levels(prd) <- c('0','1')
  ridg.cm <- confusionMatrix(prd, EN$newY, positive = '1')
  ridg.auc <- c(ridg.auc, ModelMetrics::auc(EN$newY,prd))
  
  lass <- append(lass,lass.cm)
  ridg <- append(ridg,ridg.cm)
}
mean(lass.auc) ; mean(ridg.auc)

ul <- unlist(ridg)
perf <- (data.table(V=names(ul),v=as.numeric(unname(ul))))
perf <- perf[V%in%c('overall.Accuracy','byClass.Sensitivity','byClass.Specificity',
                    'byClass.Precision','byClass.F1'),]
perf <- na.omit(perf)
perf[,mean(v),by=V]
ggplot(perf[1:100,]) + 
  geom_violin(aes(x=V,y=v)) + geom_boxplot(aes(x=V,y=v),width=.03)

# For Documentation
# stargazer(cbind(as.matrix(rbind(intercept=EN$lasso$model[[1]]$a0,EN$lasso$model[[1]]$beta)),
#                 as.matrix(rbind(intercept=EN$ridge$model[[1]]$a0,EN$ridge$model[[1]]$beta))),
#           out = 'ElNet.html')


# 5. DT -------------------------------------------------------------------

mdr <- 'MRSA'
# mdr <- 'VRE'

datalist <- preparedata(mdr)
summary(datalist)
inputdata <- datalist$inputdata;inputdata_num <- datalist$inputdata_num;inputdata_fact <- datalist$inputdata_fact
labels <- datalist$labels

sz <- 2000
p <- .7
l <- list()
names <- c()
for(i in 1:30){
  print(i)
  spd <- trtsplt(inputdata_fact,inputdata_num,labels,mdr,1,p,sz,classprop)
  trData <- spd$trData[,index:=NULL]
  ctrl <- ctree_control(teststat = c("quadratic"),
                        splitstat = c("quadratic"),
                        splittest = FALSE,
                        testtype = c("Bonferroni"), alpha = 0.05, mincriterion = 1 - .05,
                        minsplit = 20, minbucket = 100,
                        minprob = 0.01, stump = FALSE, lookahead = FALSE, MIA = FALSE,
                        tol = sqrt(.Machine$double.eps),maxsurrogate = 0L, numsurrogate = FALSE,
                        multiway = FALSE, splittry = 2L, intersplit = FALSE, majority = FALSE,
                        caseweights = TRUE, saveinfo = TRUE,
                        splitflavour = c("ctree"))
  smpctmd <-ctree(Y~., data=trData, control = ctrl)
  plot(smpctmd)
  names <- append(names,names(trData)[str_detect(paste(partykit:::.list.rules.party(smpctmd),
                                                       collapse = ''),names(trData))])
}
sort(table(names[names!='Y'])/1000,decreasing = T)
# fwrite(data.table(EmpiRank=sort(table(names)/1000,decreasing = T)),'dtrank.csv')