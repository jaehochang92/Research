rm(list = ls())
lib <- c('sqldf','dplyr','data.table','psych','Hmisc','vcd','scales',
         'ggrepel','forcats','bit64','Rfast','DescTools','RVAideMemoire',
         'lubridate','plyr','memisc','fastDummies','stringr','mice')
sapply(lib, require, character.only = TRUE) # Load packages
options(scipen = 100)

# 1. CRF & ST10 data --------------------------------------------------

# 1.1 CRF import & processing --------------------------------------------------------------

# crf <- fread('~/02CRF,STdata/CRF_20170101~20180322.csv',header = T,na.strings = c('',NA))
# crf <- crf[,-1] ; crf <- rename(crf, c("가상UUID"="PUUID", "질환상위분류"="DPC", "질환세부분류"="DSC", "상병코드"="DCODE", "상병명"="DNM", "성별"="GENDER", 
#                                        "나이(개월수)"="AGE_M", "진료일"="TD", "진료구분"="TC", "신장"="TALL", "체중"="WEIGHT", "임부여부"="PREG_YN_CRF", 
#                                        "Pathogen 검사명"="PATHEXNM", "검사장비코드"="EXEQIPCD", "Pathogen 검사일"="PATHEXDATE", "검체코드"="SPC_CD_CRF", 
#                                        "검체명"="SPC_NM_CRF", "원인균코드"="BACT_CD_CRF", "원인균명"="BACT_NM_CRF", "감수성 항생제성분코드"="STANTINGRECD", 
#                                        "감수성 항생제성분명"="STANTINGRENM", "감수성 TYPE"="ST_TYPE", "결과 SIGN"="R_SIGN", "원인균 량"="BACT_QUANT",
#                                        "치료항생제 처방일"="TANTIPRESDATE", "치료항생제 성분명"="TANTINGRENM", "치료항생제 용량(일회량)"="TANTIGREQUNAT", 
#                                        "치료항생제 단위"="TANTIGREUNIT", "치료항생제 용법(코드)"="TANTIGREUSECD", "치료항생제 기간"="TANTITERM")) #변수명 kor->eng shift
# 
# colnames(crf)[colnames(crf)=="Creatinine(BF/UR)"] <- "Creatinine_BF_UR"
# colnames(crf)[colnames(crf)=="PUUID"] <- "PTNT_UUID"
# awk_ind <- which(crf=='<3'|crf=='<2.0'|crf=='<0.2'|crf=='<1.0',arr.ind = T) # 1022건
# # AST, ALT, BUN, Creatinine, Creatinine(BF/UR), Total Urine Vol. -> exits character
# crf[99336,13]
# crf[awk_ind] <- NA # How can we treat these values?
# crf[which(crf=='없음',arr.ind=T)] <- NA # to NA
# crf[99336,13] 
# 
# # AGE_M,TALL,WEIGHT,AST,ALT,ALP,BUN,Creatinine,Creatinine_BF_UR,TANTIGREQUNAT : numerical Variables
# nums <- strsplit('PTNT_UUID,AGE_M,TALL,WEIGHT,AST,ALT,ALP,BUN,Creatinine,Creatinine_BF_UR,TANTIGREQUNAT',split = ',')[[1]]
# facts <- colnames(crf)[!colnames(crf)%in%nums]
# crf <- as.data.frame(crf)
# 
# crf[,nums] <- apply(crf[,nums], MARGIN = 2, FUN = as.numeric) # Make numeric
# crf[,facts] <- apply(crf[,facts],MARGIN = 2,FUN = factor) # Make factor
# setDT(crf)
# crf <- crf[TC=='병동',] ; crf <- crf[,-"TC"]


# 1.2 Import ST data -----------------------------------------------------------

st1 <- fread('01data/180712_st_micr_rslt_info_20080101_20111231.csv',na.strings = '')
st2 <- fread('01data/180712_st_micr_rslt_info_20120101_20141231.csv',na.strings = '')
st3 <- fread('01data/180712_st_micr_rslt_info_20150101_20161231.csv',na.strings = '')
st4 <- fread('01data/180712_st_micr_rslt_info_20170101_20181231.csv',na.strings = '')
st10 <- rbind.fill(st1,st2,st3,st4)

setDT(st10)
st10 <- st10[MED_YMD > "2007-12-31",] # Since 2008
st10 <- unique(st10) # remove duplication
rm(st1,st2,st3,st4);gc() # remove temporary data

# 2. AI Filtering --------------------------------------------------

# 2.1 Import AI data --------------------------------------------------------------

ainputall10 <- fread('01data/ai_input_sktest_all.csv',na.strings = '') # 10 years
ainputall10 <- unique(ainputall10) # Remove duplications

# 2.2 AI filtering on st10 UUID, MED_DATE ------------------------------------------------------
# Here, we only take cases from AI data whose PTNT_UUID and MED_DATE match with ST10 data.

# st10$MED_YMD <- as.numeric(gsub('-','',st10$MED_YMD)) # yyyymmdd
ainputall10[,MED_DATE:=as.numeric(gsub('-','',gsub(' 00:00:00','',ainputall10$MED_DATE)))] # yyyymmdd
st10[,MED_YMD:=as.numeric(gsub('-','',MED_YMD))]
st10 <- rename(st10, c('MED_YMD'='MED_DATE')) # Unify names of Keys

setkey(ainputall10,PTNT_UUID,MED_DATE) ; setkey(st10,PTNT_UUID,MED_DATE)
aist10 <- ainputall10[unique(st10[,.(PTNT_UUID,MED_DATE)]),nomatch=0]
rm(ainputall10)

# 2.3 Multiple MDR status issue : Join with aiMDRcheck ------------------------------------------------------------

aimdrcheck <- fread('01data/aimdrcheck.csv',na.strings = '')
aimdrcheck <- dummy_cols(aimdrcheck,select_columns = 'MDR_TYPE') ; aimdrcheck[,MDR_TYPE_VRSA:=NULL] # Make dummy table
aimdrcheck[,MDR_TYPE:=NULL]
aimdrcheck[,MED_DATE:=as.numeric(gsub('-','',gsub(' 00:00:00','',aimdrcheck$MED_DATE)))] # yyyymmdd
head(aist10$MED_DATE) ; head(aimdrcheck$MED_DATE) # Eye check
aist10[MDR_TYPE=='VRSA',] # Remove VRSA cases
aimdrcheck <- sqldf('select PTNT_UUID, MED_DATE,
                    sum(MDR_TYPE_MRSA) as MDR_TYPE_MRSA,
                    sum(MDR_TYPE_MRAB) as MDR_TYPE_MRAB,
                    sum(MDR_TYPE_MRPA) as MDR_TYPE_MRPA,
                    sum(MDR_TYPE_VRE) as MDR_TYPE_VRE,
                    sum(MDR_TYPE_CRE) as MDR_TYPE_CRE 
                    from aimdrcheck group by PTNT_UUID,MED_DATE');setDT(aimdrcheck)
temp1 <- aist10[MDR_TYPE=='0',] ; temp1[,MDR_TYPE:=NULL] # Gather negative cases
temp2 <- aist10[MDR_TYPE!='0',] ; temp2[,MDR_TYPE:=NULL] # Gather positive cases
temp2 <- sqldf('select a.*, b.MDR_TYPE_MRSA,b.MDR_TYPE_MRAB,b.MDR_TYPE_MRPA,b.MDR_TYPE_VRE,b.MDR_TYPE_CRE
               from temp2 as a, aimdrcheck as b
               where a.PTNT_UUID=b.PTNT_UUID and a.MED_DATE=b.MED_DATE') ; setDT(temp2) # Re-labelling
temp1[,':='(MDR_TYPE_MRSA=0,MDR_TYPE_MRAB=0,MDR_TYPE_MRPA=0,MDR_TYPE_VRE=0,MDR_TYPE_CRE=0)] # Negative cases
aist10mdrchecked <- rbind(temp1,temp2);rm(temp1,temp2) # Final data
rm(aist10, aimdrcheck)

# 2.4 AI additional processing --------------------------------------------

# Basis Disease RFs reduction
check <- fread('01data/0914_diagRFcheck.csv',na.strings = '')
for(i in 1:nrow(check)){
  ts <- unname(unlist(check[i,1])) ; cond <- unname(na.omit(as.vector(unlist(check[i,-1]))))
  # print(aist10mdrchecked[eval(parse(text = 
  #                              substr(paste(cond,'=="Y"|',collapse = ''),1,str_length(paste(cond,'=="Y"|',collapse = ''))-1))),][332,c(ts,cond),with=F])
  aist10mdrchecked[eval(parse(text = 
                         substr(paste(cond,'=="Y"|',collapse = ''),1,str_length(paste(cond,'=="Y"|',collapse = ''))-1)))
            ,eval(parse(text = paste(ts,':=',"'Y'",sep = ' ')))]
  # print(aist10mdrchecked[eval(parse(text = 
  #                              substr(paste(cond,'=="Y"|',collapse = ''),1,str_length(paste(cond,'=="Y"|',collapse = ''))-1))),][332,c(ts,cond),with=F])
}

fwrite(aist10mdrchecked, file = '01data/aist10mdrchecked.csv')

# 3 Time series data : Antibiotic agent grouping, so called AAG -------------------------------------------

# 3.1 Resistence Trend : st10 grouping with aag,icu -----------------------------------------------------
# Requires rcpt, st10, aag data

# Here, we inner join ST10 data and rcptinfo data on keys to get ICU_WAD_YN column.
rcptinfo <- fread('01data/180710_st_rcpt_info_I_all.csv')
rcptinfo <- rename(rcptinfo,c('MED_YMD'='MED_DATE'))
rcptinfo[,MED_DATE:=as.numeric(gsub('-','',MED_DATE))]
rcptinfo[,':='(PNCL_ADEFCT_YN=NULL,CPM_ADEFCT_YN=NULL,FC_RC_DVSN_CD=NULL)] # Drop useless columns

aag <- fread('01data/aag.csv',na.strings = '')

###
rcptkey <- unique(rcptinfo[,.(MED_DATE,PTNT_UUID,ICU_WAD_YN)]) # Get keys

setkey(st10,PTNT_UUID,MED_DATE) ; setkey(rcptkey,PTNT_UUID,MED_DATE)
temp <- st10[rcptkey,nomatch=0] # Filtering on rcpk keys
temp <- join(temp, aag[,.(AAG,ANTB_IGRD_NM)], by='ANTB_IGRD_NM') # vlookup aag on antb
temp <- temp[!is.na(AAG),]
# unique(temp[,ANTB_IGRD_NM])%>%View('st10') ; unique(aag[,AAG])%>%View('aag')

temp[,unique(ANTB_IGRD_NM),by=AAG]%>%View()

st10rcptaag <- temp ; rm(temp)

# fwrite(st10rcptaag,'01data/st10rcptaag.csv')


# 3.2 AU(Antibiotic Use) trend : ordr data -------------------------------------------------------
# Need ord, rcpt, aag

# Here, we get AAG, INTG_ANTB_CD through the inner join of ORDR data and rcptinfo data.
load('01data/ordr.RData') ; ord <- ordr ; rm(ordr)

colnames(ord)[c(1,4)] <- c('INTG_ANTB_CD','MED_DATE') # Change column name
ord[,MED_DATE:=as.numeric(gsub('-','',gsub(' 00:00:00','',ord$MED_DATE)))] 

setkey(ord,PTNT_UUID,MED_DATE) ; setkey(rcptinfo,PTNT_UUID,MED_DATE)
temp <- ord[rcptinfo[,':='(DR_ID=NULL, CRT_DTTI=NULL)],nomatch=0]# Filtering on rcpk keys
temp <- join(temp, aag[,.(AAG,INTG_ANTB_CD)], by='INTG_ANTB_CD') # vlookup aag on antb
temp <- temp[!is.na(AAG),]

# fwrite(temp,'0817_ordrrcptaag.csv') ; rm(temp)

# Miscellaneous -----------------------------------------------------------

# merge(dt1,dt2,by.x=c("a","b"),by.y=c("c","b"),all=T) # Outer Join
# setkey(dt1,a,b)
# setkey(dt2,c,b)
# dt2[dt1,nomatch=0] # Inner Join
# dt2[dt1] #Left Join (if dt1 is the "left" table)
# dt1[dt2] #Right Join (if dt1 is the "left" table)