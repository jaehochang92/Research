lib <- c('ggplot2','dplyr','data.table','psych','MASS','reshape2',"doParallel", "foreach",
         'scales','ggrepel','gridExtra','Rfast','lubridate','graphics','stringr','tidyverse')
sapply(lib, require, character.only = TRUE)
AUd <- fread('../../3A/01data/0817_ordrrcptaag.csv',na.strings = '');AUd[,ORDR_YMD:=ymd(gsub(' 00:00:00','',ORDR_YMD))];AUd[,END_YMD:=ymd(ORDR_YMD)+ORDR_NDY-1];options(scipen = 100)
st10rcptaag <- fread('../../3A/01data/st10rcptaag.csv',na.strings = '')

rm(list = ls()[!ls()%in%c('AUd','st10rcptaag')]) ; gc()

# 1.1 Data Cleansing -----------------------------------------------

Cm.diff <- function(v,lag,diff){
  n <- rep(NA,length(v))
  for(i in lag:length(v)){
    n[i] <- sum(v[(i+1-lag):i]) # Not Cumul.
  }
  r <- rep(NA,length(v))
  for(i in (lag+1):length(v)){
    r[i] <- n[i]-n[i-1] # Diff.
  }
  if(diff==T){return(r)}else{return(n)}
}

HospCh <- function(r.aag, d.aag, icu, stt, end, cm, R.mM)
  {
  # 1.2 plot --------------------------------------------------------------------
  vars <- c('ICU_WAD_YN','AAG','EXMN_YMD','REACT_TYPE_CD')
  gd <- unique(st10rcptaag[AAG%in%r.aag&ICU_WAD_YN==icu,]) ; gc()
  # gd[,MED_DATE:=as.numeric(gsub('-','',MED_DATE))]
  gd[,EXMN_YMD:=as.numeric(substr(gsub(' ','',gsub('-','',gsub(':','',EXMN_YMD))),1,6))]
  gd[,EXMN_YMD:=paste(as.character(EXMN_YMD),'01',sep='')][,EXMN_YMD:=ymd(EXMN_YMD)]
  gd <- gd[!is.na(REACT_TYPE_CD)&MED_DATE>20091231,vars,with=F] # Use 2010 ~
  gd[,Rscore:=case_when(
    REACT_TYPE_CD == 'R' ~ 1,
    REACT_TYPE_CD == '+' ~ 1,
    REACT_TYPE_CD == 'I' ~ 1,
    REACT_TYPE_CD == 'S' ~ 0,
    REACT_TYPE_CD == '-' ~ 0,
    TRUE ~ 0)] ; gd[,REACT_TYPE_CD:=NULL]
  gd[,':='(Rscore=sum(Rscore),EXAM_CNT=.N),
     by=.(ICU_WAD_YN,AAG,EXMN_YMD)] %>% unique() -> RSd
  RSd[,Rprop:=Rscore/EXAM_CNT*100] ; RSd <- RSd[order(EXMN_YMD),]
  # rsdf1 <- unique(RSd[AAG==aag&ICU_WAD_YN==icu,])
  rsdf1 <- RSd
  dseq <- seq(stt,end,1)
  rsdf1 <- rsdf1[EXMN_YMD %in% dseq,][,':='(Rscore=NULL,EXAM_CNT=NULL)] # 기간 필터링
  rsdf <- rsdf1 ; rm(rsdf1)

  AUdf <- AUd[ORDR_YMD%in%dseq,.(PTNT_UUID,MED_DATE,ORDR_YMD,END_YMD,ORDR_NDY,ICU_WAD_YN,AAG,
                 ANTB_IGRD_NM)]
  # Specify Groups by AAG, ICU YN
  AUdf <- AUdf[AAG%in%d.aag&ICU_WAD_YN==icu,]
  AUdf[,ORDR_NDY:=max(ORDR_NDY),by=.(PTNT_UUID,ORDR_YMD,ANTB_IGRD_NM)]
  du <- c()
  
  registerDoParallel(cores = detectCores())
  du <- foreach(i = 1:nrow(AUdf), .packages = c('data.table','bit64'), .combine = c) %dopar% {
    ap <- AUdf[i,seq(ORDR_YMD,END_YMD,1)]
    names(ap) <- rep(paste(AUdf[i,AAG],AUdf[i,PTNT_UUID],sep = ''),length(ap))
    ap
  };stopImplicitCluster()
  du <- data.table(ID=str_sub(names(du),-15,-1),AAG=str_sub(names(du),1,str_length(names(du))-15),date=du)
  du <- du[date%in%dseq,] # Filter Period
  
  # DOT using Hot coding
  dot <- du[,.N,by=.(ID,AAG,date)] ; dot[,ym:=substr(date,1,7)] ; dot <- dot[,sum(N),by=.(ym,AAG)]
  lot <- du[,length(unique(ID)),by=.(date,AAG)] ; lot[,ym:=substr(date,1,7)] ; lot <- lot[,sum(V1),by=.(ym,AAG)]
  setkey(dot,ym,AAG) ; setkey(lot,ym,AAG) ;
  dlr <- dot[lot,nomatch=0] ; dlr[,dlr:=V1/i.V1*100] ; colnames(dlr) <- c('ORDR_YMD','AAG','DOT','LOT','DLRatio')
  dlr[,ORDR_YMD:=ymd(paste(ORDR_YMD,'-01',sep = ''))]
  dlr[,Cm.DOT:=Cm.diff(DOT,cm,FALSE),by=.(AAG)]
  rsdf <- melt(rsdf,measure.vars = 'Rprop')
  dlr <- melt(dlr,id.vars = 1:2)
  
  # labels and breaks for X axis text
  # brks <- unique(dlr$ORDR_YMD)
  brks <- unique(rsdf$EXMN_YMD)
  lbls <- substr(brks,1,7)
  # plot
  sel <- c('Cm.DOT','Rprop')
  theme_set(theme_bw())
  if(R.mM==T){
    idx1 <- rsdf[variable%in%sel[2],which.max(value),by=AAG] ; idx1 <- rename(idx1,c('V1'='idx.max'))
    idx2 <- rsdf[variable%in%sel[2],which.min(value),by=AAG] ; idx2 <- rename(idx2,c('V1'='idx.min'))
    setkey(idx1,AAG) ; setkey(idx2,AAG)
    idx <- idx1[idx2]
  }
  sel <- c('Cm.DOT','Rprop')
  g2 <- ggplot() +
    geom_line(data=rsdf[variable%in%sel[2],],
              aes(x=EXMN_YMD,y=value,col=AAG), lwd=.5) +
    geom_point(data=rsdf[variable%in%sel[2],], aes(x=EXMN_YMD,y=value,col=AAG)) +
    labs(title="Resistence Prop. VS Cumulative DOT",
        subtitle=paste('Cumulation = ', cm, ', ','Top = R.Prop. & Bottom = Cumul.DOT',
                       sep = ''),
        x=NULL, y=NULL, color=NULL) +  # title and caption
    scale_x_date(labels = NULL, breaks = brks) +  # change to monthly ticks and labels
    theme(panel.grid.minor = element_blank(),  # turn off minor grid
          legend.position = 'top', plot.margin = margin(0,1,0,1,'cm'),
          axis.text.y=element_blank())
    if(R.mM==T){
      for(i in 1:nrow(idx)){
        g2 <- g2 + 
          annotate("text",
                   x = rsdf[variable%in%sel[2]&AAG==idx$AAG[i],][idx$idx.max[i],EXMN_YMD],
                   y = rsdf[variable%in%sel[2]&AAG==idx$AAG[i],][idx$idx.max[i],value]+1,
                   label = paste(idx$AAG[i]," Max.",'(',round(rsdf[variable%in%sel[2]&AAG==idx$AAG[i],][idx$idx.max[i],value],1),'%)',
                                 sep = '')) +
          annotate("text",
                   x = rsdf[variable%in%sel[2]&AAG==idx$AAG[i],][idx$idx.min[i],EXMN_YMD],
                   y = rsdf[variable%in%sel[2]&AAG==idx$AAG[i],][idx$idx.min[i],value]-1,
                   label = paste(idx$AAG[i]," min.",'(',round(rsdf[variable%in%sel[2]&AAG==idx$AAG[i],][idx$idx.min[i],value],1),'%)',
                                 sep = ''))
      }
    }
    
  g3 <- ggplot() +
    geom_line(data=dlr[variable%in%sel[1],], aes(x=ORDR_YMD, y=value, col=AAG), lwd=.5) +
    geom_point(data=dlr[variable%in%sel[1],], aes(x=ORDR_YMD, y=value, col=AAG)) +
    labs(y=NULL, color=NULL) +  # title and caption
    scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
    theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 8),  # rotate x axis text
          panel.grid.minor = element_blank(),  # turn off minor grid
          legend.position = 'bottom', plot.margin = margin(0,1,0,1,'cm'),
          axis.text.y=element_blank())
  gg <- grid.arrange(g2,g3,ncol=1)
  # gg <- g1
  ggsave('RSplt.png',gg,units = 'mm', width = 400, height = 250)
  return(list(rsdf=rsdf,dlr=dlr))
}

allaag <- intersect(unique(AUd[,AAG]),unique(st10rcptaag[,AAG]))

RvsAUdf <- HospCh(r.aag=allaag[5], d.aag = allaag[3:7],
                  icu='Y',
                  stt=ymd('20080101'),
                  end=ymd('20180701'),
                  cm=12, R.mM=T)

dcast(RvsAUdf,ORDR_YMD+AAG+ICU_WAD_YN~variable,value.var = 'value') %>% View

# 1.3 Granger C ---------------------------------------------------------------

# gcdf <- dcast(RvsAUdf,ORDR_YMD+AAG+ICU_WAD_YN~variable) ; setDT(gcdf)
# ndiffs(gcdf$DOT, alpha = .05, test = 'kpss')
# ndiffs(gcdf$Cm.DOT.diff, alpha = .05, test = 'kpss')
# ndiffs(gcdf$Rprop, alpha = .05, test = 'kpss')
# # Make stationary data
# # AUTOMATICALLY SEARCH FOR THE MOST SIGNIFICANT RESULT
# for (i in 1:30){
#   G <- causality(VAR(na.omit(gcdf[,.(Cm.DOT.diff,Rprop)]), p = i, type = "const"),
#                  cause = "Cm.DOT.diff")$Granger
#   pv <- G$p.value[1,1]
#   if(pv<0.05){
#     cat("DOT cause LAG =", i) ; print(pv)
#   }
#   G <- causality(VAR(na.omit(gcdf[,.(Cm.DOT.diff,Rprop)]), p = i, type = "const"),
#                  cause = "Rprop")$Granger
#   pv <- G$p.value[1,1]
#   if(pv<0.05){
#     cat("R cause LAG =", i) ; print(pv)
#   }else{next}
# }
# var <- VAR(na.omit(gcdf[,.(Cm.DOT.diff,Rprop)]), p = 4, type = "const")
# causality(var, cause = "Cm.DOT.diff")$Granger
# causality(var, cause = "Rprop")$Granger