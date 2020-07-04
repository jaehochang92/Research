rm(list = ls()) ; gc()

lib <- c('sqldf','ggplot2','dplyr','data.table','psych','stargazer','Hmisc','vcd',
         'corrgram','ggcorrplot','scales','ggrepel','forcats','gridExtra','mice',
         'bit64','corrplot','Rfast','DescTools','RVAideMemoire','lubridate',
         'fastDummies','graphics','questionr','reshape2','InformationValue',
         'stringr','partykit','rpart','rpart.plot','caret',"glmnet","doParallel",
         "foreach", "pROC",'lars','doSNOW','tcltk','parallel')

# Define difference :
# For given vector 'vct', sequentially calculate 
# the difference of neighborhood elements.

difference <- function(vct){ # vct must be vector
  l <- length(vct)
  null <- rep(0,l)
  if(l>1){
    for(i in 2:l){
      null[i] <- as.numeric(vct[i]-vct[i-1])
    }
    return(null)
  }else{
    return(null)
  }
}

# Define preparedata :
# For given MDR_TYPE 'mdr', generates the list of
# MDR labels + Factorial / Numeric inputdata and those union.

preparedata <- function(mdr, pool = T){ # mdr must be sting
  ainputall <- fread('01data/0808_aist10mdrchecked.csv',na.strings = '') ; gc()
  ainputall[,THORA_YN:=NULL]
  inputdata <- ainputall
  inputdata <- inputdata[AGE_YR>0,] # remove neg.
  inputdata <- inputdata[MED_DATE>20071231,]
  rm.idx <- with(inputdata,(!is.na(ADM_PATH))&is.na(CURR_LOS))
  inputdata <- inputdata[!rm.idx,] # Outliers of inpatients
  inputdata <- inputdata[!is.na(ADM_PATH),] ; gc() # Inpatients only
  if(pool == F){
    inputdata[,MED_DATE:=ymd(MED_DATE)]
    inputdata <- inputdata[order(MED_DATE),];inputdata[,MDdiff:=difference(MED_DATE),by=.(PTNT_UUID)]
    temp1 <- inputdata[MDdiff>30,];temp2 <- inputdata[MDdiff<=30,];temp3 <- temp2[,min(MED_DATE),by=.(PTNT_UUID)]
    setkey(temp2,PTNT_UUID,MED_DATE) ; setkey(temp3,PTNT_UUID,V1);temp <- temp2[temp3,nomatch=0]
    inputdata <- rbind(temp1,temp) ; rm(temp1,temp2,temp3,temp) ; inputdata[,MDdiff:=NULL]
    inputdata[,':='(PTNT_UUID=NULL,MED_DATE=NULL)]
    labels <- inputdata[,(ncol(inputdata)-4):ncol(inputdata)]
    labels[] <- lapply(labels, factor)
    inputdata_num <- inputdata[,colnames(labels):=NULL] %>% # numeric data pool
      dplyr::select_if(is.numeric)
    inputdata_num[] <- lapply(inputdata_num, c)
    inputdata_fact <- inputdata %>% # character data pool
      dplyr::select_if(is.character)
    inputdata_fact[] <- lapply(inputdata_fact, factor)
    l <- list(inputdata=inputdata, inputdata_num=inputdata_num, inputdata_fact=inputdata_fact,
              labels=labels)
  }else{
    ch <- fread('01data/RFPool(Include Article)_0914.csv',na.strings = '')
    ch <- ch[,c('var',mdr),with=F] ; ch <- ch[c(is.na(ch[,2])),1] ; ch <- c(ch)[[1]]
    inputdata <- inputdata[,c('PTNT_UUID',ch),with=F]
    inputdata[,MED_DATE:=ymd(MED_DATE)]
    inputdata <- inputdata[order(MED_DATE),];inputdata[,MDdiff:=difference(MED_DATE),by=.(PTNT_UUID)]
    temp1 <- inputdata[MDdiff>30,];temp2 <- inputdata[MDdiff<=30,];temp3 <- temp2[,min(MED_DATE),by=.(PTNT_UUID)]
    setkey(temp2,PTNT_UUID,MED_DATE) ; setkey(temp3,PTNT_UUID,V1);temp <- temp2[temp3,nomatch=0]
    inputdata <- rbind(temp1,temp) ; rm(temp1,temp2,temp3,temp) ; inputdata[,MDdiff:=NULL]
    inputdata[,':='(PTNT_UUID=NULL,MED_DATE=NULL)]
    labels <- inputdata[,(ncol(inputdata)-4):ncol(inputdata)]
    labels[] <- lapply(labels, factor)
    inputdata_num <- inputdata[,colnames(labels):=NULL] %>% # numeric data pool
      dplyr::select_if(is.numeric)
    inputdata_num[] <- lapply(inputdata_num, c)
    inputdata_fact <- inputdata %>% # character data pool
      dplyr::select_if(is.character)
    inputdata_fact[] <- lapply(inputdata_fact, factor)
    l <- list(inputdata=inputdata, inputdata_num=inputdata_num, inputdata_fact=inputdata_fact,
              labels=labels)
  }
  return(l)
}

# Define distk :
# For given data 'data', and scalar 'k',
# return the names of variables
# having 'k' unique values.
# Usually used for the detection of non-variant variable.

distk <- function(data,k){
  useless <- c()
  for(i in 1:dim(data)[2]){                   # distinct=1 variables
    test <- dim(unique(data[,..i]))[1]==k
    if(test==T){ useless <- c(useless,colnames(data)[i])}
    else{next}
  }
  return(useless)
}

# Define trtsplt :
# For given MDR_TYPE 'mdr', and inputdata + labels,
# generate a list of Train / Test data with size 'sz'
# with Train : Test = p : 1-p and
# Ones : Zeros = classprop[1] : classprop[2].

trtsplt <- function(inputdata_fact,inputdata_num,labels,
                    mdr, k = 1, p, sz, classprop = NULL, tsidx = NULL){ # sz : trts size
  type <- paste('MDR_TYPE_',mdr,sep = '')
  lrdata <- cbind(inputdata_fact,inputdata_num,labels[,type,with=F])
  Y <- lrdata[,ncol(lrdata),with=F] ; lrdata[,Y:=Y] ; lrdata <- lrdata[,-ncol(lrdata)+1,with=F]
  lrdata[,index:=1:nrow(lrdata)]
  Ones <- lrdata[Y==1,]
  Zeros <- lrdata[Y==0,]
  if(is.null(classprop)){
    classprop <- c(nrow(Zeros)/nrow(lrdata), nrow(Ones)/nrow(lrdata))
  }else{
    classprop <- classprop}
  if(is.null(tsidx)){
    Ts_tr_rows <- sample(Ones$index, p*sz*classprop[2])  # 1's for training
    Fs_tr_rows <- sample(Zeros$index, p*sz*classprop[1])  # 0's for training
    tr_ones <- Ones[index%in%Ts_tr_rows,] ; tr_zeros <- Zeros[index%in%Fs_tr_rows,]
    Ts_ts_rows <- sample(setdiff(Ones$index,Ts_tr_rows), (1-p)*sz*classprop[2])  # 1's for test
    Fs_ts_rows <- sample(setdiff(Zeros$index,Fs_tr_rows), (1-p)*sz*classprop[1])  # 0's for test
    ts_ones <- Ones[index%in%Ts_ts_rows,] ; ts_zeros <- Zeros[index%in%Fs_ts_rows,]
    trData <- rbind(tr_ones, tr_zeros)  # row bind the 1's and 0's
    tsData <- rbind(ts_ones, ts_zeros)  # row bind the 1's and 0's
    return(list(trData=trData,tsData=tsData))
  }else{
    Ts_tr_rows <- sample(setdiff(Ones$index,tsidx), p*sz*classprop[2])  # 1's for training
    Fs_tr_rows <- sample(setdiff(Zeros$index,tsidx), p*sz*classprop[1])  # 0's for training
    tr_ones <- Ones[index%in%Ts_tr_rows,] ; tr_zeros <- Zeros[index%in%Fs_tr_rows,]
    Ts_ts_rows <- sample(setdiff(Ones$index,Ts_tr_rows), (1-p)*sz*classprop[2])  # 1's for test
    Fs_ts_rows <- sample(setdiff(Zeros$index,Fs_tr_rows), (1-p)*sz*classprop[1])  # 0's for test
    ts_ones <- Ones[index%in%Ts_ts_rows,] ; ts_zeros <- Zeros[index%in%Fs_ts_rows,]
    trData <- rbind(tr_ones, tr_zeros)  # row bind the 1's and 0's
    tsData <- rbind(ts_ones, ts_zeros)  # row bind the 1's and 0's
    return(list(trData=trData,tsData=tsData))
  }
}

# Define logistic.emp.rnk :
# For given MDR_TYPE 'mdr', generate a empirical rank of Risk Factors
# with a random sample data of size 'sz' for k times
# with Train : Test = p : 1-p and
# Ones : Zeros = classprop[1] : classprop[2]
# using Forward selection based on Bayesian Information Criterion(BIC).

logistic.emp.rnk <- function(sz,p,k,mdr,classprop = NULL){
  set.seed(1111)
  l <- list()
  for(i in 1:k){
    spd <- trtsplt(inputdata_fact,inputdata_num,labels,mdr = mdr,p = 1,sz = sz,classprop = classprop)
    spd$trData[,index:=NULL]
    if(is.null(distk(spd$trData,1))==F){
      spd$trData[,(distk(spd$trData,1)):=NULL]
      null <- glm(Y ~ 1, data=spd$trData, family=binomial(link="logit"))
      full <- glm(Y ~ ., data=spd$trData, family=binomial(link="logit"))
      logitMod <- step(null, scope=list(lower=null, upper=full), direction="forward",
                       trace = F, k=log(dim(spd$trData)[1])) # BIC
      l <- append(l,list(logitMod))
    }else{
      null <- glm(Y ~ 1, data=spd$trData, family=binomial(link="logit"))
      full <- glm(Y ~ ., data=spd$trData, family=binomial(link="logit"))
      logitMod <- step(null, scope=list(lower=null, upper=full), direction="forward",
                       trace = F, k=log(dim(spd$trData)[1])) # BIC
      l <- append(l,list(logitMod))
    }
    print(i)
  }
  coef <- c()
  for(i in 1:length(l)){
    coef <- append(coef,l[[i]]$coefficients)
  };dt <- melt(coef) ; setDT(dt) ; dt[,V:=names(coef)] ; dt[,coef:=value]
  return(data.table(EmpiRank=sort(table(dt[,V])/length(l),decreasing = T)))
}

# Define logistic.final :
# For given MDR_TYPE 'mdr' and empirical variable rank 'emprnk', 
# first make Train/Test data using 'trtsplt'
# with a random sample data of size 'sz'
# with Train : Test = p : 1-p and
# Ones : Zeros = classprop[1] : classprop[2]
# using top 'j' variables from emprnk.
# Assess the performance of model with trained model and test data
# for 'k' times of random resampling.
# Here, the iteration of Train / Test data split
# whose test data once used for train will not be used and
# resample until it attains the valid data.

logistic.final <- function(mdr,emprnk,p,sz,j,k,classprop = NULL,coff,pkgs=lib){
  set.seed(2222) # Fix seeds
  fin <- emprnk[order(-EmpiRank.N),EmpiRank.V1][1:(j+1)] # top j features
  fin <- subset(fin,fin!='(Intercept)')
  fin2 <- fin[fin%in%names(inputdata_num)] ; fin1 <- setdiff(fin,fin2)
  fin1 <- substr(fin1,1,nchar(fin1)-1) ; fin <- unique(c(fin1,fin2))
  l <- list(Perf = NULL, models = NULL, tsindex = NULL)
  mypb <- tkProgressBar(title = "R progress bar", label = "",
                        min = 0, max = 1, initial = 0, width = 300)
  spd <- trtsplt(inputdata_fact,inputdata_num,labels,mdr = mdr,p = p,sz = sz,classprop = classprop)
  for(i in 1:k){
    setTkProgressBar(mypb, i/k, title = "Final logistic.", label = NULL)
    if(k>1){
      spd <- trtsplt(inputdata_fact,inputdata_num,labels,mdr = mdr,p = p,sz = sz,classprop = classprop,
                     tsidx = l$tsindex) # avoid re-train
      model <-  glm(Y~.,data = spd$trData[,c(fin,'Y'),with=F],family = binomial)
      predicted <- unname(predict(model,spd$tsData,type = 'response'))
      # optCutOff <- optimalCutoff(spd$tsData$Y, predicted)[1]
      optCutOff <- coff
      prd <- as.factor((predicted>optCutOff)*1) ; levels(prd) <- c('0','1')
      cm <- confusionMatrix(prd, spd$tsData$Y,
                            positive = '1')
      l$Perf <- append(l$Perf,list(cm))
      l$tsindex <- unique(append(l$tsindex,spd$tsData$index))
      l$models <- append(l$models,list(model))
    }else if(k==1){ # First iteration
      if(is.null(distk(spd$trData,1))==F){
        while(is.null(distk(spd$trData,1))==F){
          spd <- trtsplt(inputdata_fact,inputdata_num,labels,mdr,p,sz,classprop)
        }
        model <-  glm(Y~.,data = spd$trData[,c(fin,'Y'),with=F],family = binomial)
        predicted <- unname(predict(model,spd$tsData,type = 'response'))
        # optCutOff <- optimalCutoff(spd$tsData$Y, predicted)[1]
        optCutOff <- coff
        prd <- as.factor((predicted>optCutOff)*1) ; levels(prd) <- c('0','1')
        cm <- confusionMatrix(prd, spd$tsData$Y,
                              positive = '1')
        l$Perf <- append(l$Perf,list(cm))
        l$tsindex <- spd$tsData$index
        l$models <- append(l$models,list(model))
      }else if(is.null(distk(spd$trData,1))==T){
        model <-  glm(Y~.,data = spd$trData[,c(fin,'Y'),with=F],family = binomial)
        predicted <- unname(predict(model,spd$tsData,type = 'response'))
        # optCutOff <- optimalCutoff(spd$tsData$Y, predicted)[1]
        optCutOff <- coff
        prd <- as.factor((predicted>optCutOff)*1) ; levels(prd) <- c('0','1')
        cm <- confusionMatrix(prd, spd$tsData$Y,
                              positive = '1')
        l$Perf <- append(l$Perf,list(cm))
        l$tsindex <- spd$tsData$index
        l$models <- append(l$models,list(model))
      }
    }
  } ; close(mypb)
  return(l)
}

# Define ElNet :
# For given MDR_TYPE 'mdr' and empirical variable rank 'emprnk', 
# first make Train/Test data using 'trtsplt'
# with a random sample data of size 'sz'
# with Train : Test = p : 1-p.
# First of all, Elastic Net is currently not in use due to
# the parameter tuning procedure being far more heavier than LASSO or Ridge.
# We use cv.glmnet to get a 10-fold cross-validated model tuning parameter 
# which minimizes the validation error.

ElNet <- function(mdr,p,sz){
  EN <- list(lasso=list(model=NULL, roc=NULL, ll=NULL),
             ridge=list(model=NULL, roc=NULL, ll=NULL),
             elnet=list(model=NULL, roc=NULL, ll=NULL))
  datalist <- preparedata(mdr)
  inputdata <- datalist$inputdata
  inputdata_num <- datalist$inputdata_num
  inputdata_fact <- datalist$inputdata_fact
  labels <- datalist$labels
  if(sz=='all'){sz <- nrow(inputdata)}
  spd <- trtsplt(inputdata_fact,inputdata_num,labels,mdr,p,sz)
  trData <- spd$trData ; tsData <- spd$tsData
  trX <- as.data.table(model.matrix(~.-1-Y,trData))
  trX <- dplyr::select(trX,-c('SEXF')) ; trX <- as.matrix(trX)
  trY <- as.factor(trData[,Y])
  newX <- as.data.table(model.matrix(~.-1-Y,tsData))
  newX <- dplyr::select(newX,-c('SEXF')) ; newX <- as.matrix(newX)
  newY <- as.factor(tsData[,Y])
  
  cl<-makeCluster(detectCores(),type="SOCK")
  registerDoSNOW(cl)
  # LASSO WITH ALPHA = 1
  cv1 <- cv.glmnet(trX, trY, family = "binomial", nfold = 10,type.measure = "class", parallel = TRUE, alpha = 1)
  EN$lasso$model <- append(EN$lasso$model,
                           list(model.lasso <- glmnet(trX, trY, family = "binomial", lambda = cv1$lambda.min, alpha = 1)))
  EN$lasso$roc <- append(EN$lasso$roc,
                         list(roc(newY, as.numeric(predict(model.lasso, newX, type = "response")))))
  EN$lasso$ll <- append(EN$lasso$ll,
                        LogLoss(as.numeric(newY)-1,predict(model.lasso, newX, type = 'response')))
  print('LASSO done')
  # RIDGE WITH ALPHA = 0
  cv2 <- cv.glmnet(trX, trY, family = "binomial", nfold = 10,type.measure = "class", parallel = TRUE, alpha = 0)
  EN$ridge$model <- append(EN$ridge$model,
                           list(model.ridge <- glmnet(trX, trY, family = "binomial", lambda = cv2$lambda.min, alpha = 0)))
  EN$ridge$roc <- append(EN$ridge$roc,
                         list(roc(newY, as.numeric(predict(model.ridge, newX, type = "response")))))
  EN$ridge$ll <- append(EN$ridge$ll,
                        LogLoss(as.numeric(newY)-1,predict(model.ridge, newX, type = 'response')))
  print('RIDGE done')
  # ELASTIC NET WITH 0 < ALPHA < 1
  # a <- seq(0.1, 0.9, 0.05)
  # search <- foreach(i = a, .combine = rbind) %dopar% {
  #   cv <- cv.glmnet(trX, trY, family = "binomial", nfold = 10,
  #                   type.measure = "class", paralle = TRUE, alpha = i)
  #   data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.min], lambda.min = cv$lambda.min, alpha = i)
  # }
  # cv3 <- search[search$cvm == min(search$cvm), ]
  # EN$elnet$model <- append(EN$elnet$model,
  #                          list(model.elnet <- glmnet(trX, trY, family = "binomial", lambda = cv3$lambda.min, alpha = cv3$alpha)))
  # EN$elnet$roc <- append(EN$elnet$roc,
  #                        list(roc(newY, as.numeric(predict(model.elnet, newX, type = "response")))))
  # EN$elnet$ll <- append(EN$elnet$ll,
  #                       LogLoss(as.numeric(newY)-1,predict(model.elnet, newX, type = 'response')))
  # print('ELNET done')
  EN$newX <- newX ; EN$newY <- newY
  return(EN)
  stopCluster(cl)
}

# Define LogLoss :
# For given True-condition labels 'act' and Predicted-condition labels 'pred'
# compute the logloss. The logloss is designed to measure the loss of
# model predicting probability through the convex function of 'pred'.
# Also, we stablize the numeric calculation with 'eps' for the case of
# returning the Inf values.

LogLoss<-function(act, pred) # Assessment of model : Measures how much close the model predicted a probability for a given class.
{
  eps = 1e-15;
  nr = length(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr) 
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(length(act))
  return(ll);
}