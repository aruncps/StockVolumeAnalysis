anchor<-126
DateArray = sort(Data_TrendforValidation[,unique(DATE)])
DateArrayLength = length(sort(Data_TrendforValidation[,unique(DATE)]))

date1<-DateArray[DateArrayLength-1-anchor]
date2<-DateArray[DateArrayLength-anchor]
length(Data_TrendforValidation[DATE==date1 & CLOSE>EMA_200 & EMA_150>EMA_200,unique(SYMBOL)]) / 
length(Data_TrendforValidation[DATE==date1,unique(SYMBOL)])


#Data_02_all_files_dataTable[SYMBOL=='TIMKEN' & DATE>'2020-01-01']
#EMA_RS_dt[SYMBOL=='TIMKEN' & DATE>'2020-01-01']
EMA_RS_dt<-Data_TrendforValidation[,.(SYMBOL, Month_Yr, DATE, CLOSE, Uptrend_CL_GT150GT200
                                          ,RSI_5
                                          , EMA_8, EMA_50, EMA_150, EMA_200
                                          , EMA_RS=((CLOSE-EMA_200)/EMA_200)*.5 + ((EMA_150-EMA_200)/EMA_200)*.5, EMA_Up=(CLOSE>EMA_200 & EMA_150>EMA_200)
                                          ,EMA_RS_10,EMA_RS_40,EMA_RS_80
                                          , Nifty_RS=((EMA_RS_40-EMA_RS_80)/EMA_RS_80), Nifty_RS_Up=(EMA_RS_40>EMA_RS_80)
                                          ,EMA_VOL_10,EMA_VOL_40,EMA_VOL_80,EMA_VOL_250,Vol_Trend
                                          ,Tick= (1) * (IsO2C_PositiveUpTick == TRUE ) + (-1) * (IsO2C_PositiveUpTick == FALSE)
                                          , Vol_RS=((EMA_VOL_40-EMA_VOL_80)/EMA_VOL_80), Vol_RS_Up=(EMA_VOL_40>EMA_VOL_80)
                                          ,NIFTY500_CLOSE ,NIFTY500_CLOSE_10 ,NIFTY500_CLOSE_40, NIFTY500_UpTrend
                                          ,Rank_63, Rank_all
                                          ,ltst_Rank_63, ltst_Rank_126, ltst_Rank_189, ltst_Rank_252, ltst_Rank_all
                                          ,OneMonthUp
                                          )]
#EMA_RS_dt1[SYMBOL=='TIMKEN' & DATE>'2020-01-01']
EMA_RS_dt1<-EMA_RS_dt[,.(SYMBOL, Month_Yr, DATE, CLOSE, Uptrend_CL_GT150GT200
                         ,RSI_5
                         , EMA_8, EMA_50, EMA_150, EMA_200, EMA_RS,EMA_Up
                         ,EMA_RS_rank=floor((rank(EMA_RS)/length(EMA_RS))*100)
                         ,EMA_RS_10,EMA_RS_40,EMA_RS_80,Nifty_RS,Nifty_RS_Up
                         ,Nifty_RS_rank=floor((rank(Nifty_RS)/length(Nifty_RS))*100)
                         ,EMA_VOL_10,EMA_VOL_40,EMA_VOL_80,Vol_RS,EMA_VOL_250,Vol_Trend,Tick
                         ,Vol_RS_Up
                         ,Vol_RS_rank=floor((rank(Vol_RS)/length(Vol_RS))*100)
                         ,OneMonthUp
                         ,NIFTY500_CLOSE ,NIFTY500_CLOSE_10 ,NIFTY500_CLOSE_40, NIFTY500_UpTrend
                         ,Rank_63, Rank_all
                         ,ltst_Rank_63, ltst_Rank_126, ltst_Rank_189, ltst_Rank_252, ltst_Rank_all
                         ), by=.(DATE)]
#EMA_RS_rnk_trnd[SYMBOL=='TIMKEN' & DATE>'2020-01-01']
EMA_RS_rnk_trnd<-EMA_RS_dt1[DATE>=date1 & (CLOSE>EMA_150 & CLOSE>EMA_200 & EMA_150>EMA_200),.(SYMBOL, Month_Yr, DATE
                                          ,CLOSE, Uptrend_CL_GT150GT200
                                          ,RSI_5
                                          , EMA_8, EMA_50, EMA_150, EMA_200, EMA_RS,EMA_Up,EMA_RS_rank
                                          ,EMA_RS_10,EMA_RS_40,EMA_RS_80,Nifty_RS,Nifty_RS_Up,Nifty_RS_rank
                                          ,EMA_VOL_10,EMA_VOL_40,EMA_VOL_80,EMA_VOL_250,Vol_Trend,Tick
                                          ,Vol_RS,Vol_RS_Up,Vol_RS_rank,OneMonthUp
                                          ,NIFTY500_CLOSE ,NIFTY500_CLOSE_10 ,NIFTY500_CLOSE_40, NIFTY500_UpTrend
                                          ,Rank_63, Rank_all
                                          ,ltst_Rank_63, ltst_Rank_126, ltst_Rank_189, ltst_Rank_252, ltst_Rank_all
                                          )]

# -------------------------------------
Code33 <- read_excel("Code33.xlsx", 
                     col_types = c(
                       "text", "text", "logical", "logical", "logical"
                       , "logical", "logical", "logical", "logical", "logical"
                       , "logical", "logical", "logical", "logical", "numeric"
                       , "numeric", "date", "date", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "logical", "logical", "logical"
                       , "logical", "logical", "logical", "logical", "logical"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "logical"
                       , "logical", "logical", "logical", "logical", "logical"
                       , "logical", "logical", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "logical", "logical", "logical", "logical"
                       , "logical", "logical", "logical", "logical", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "logical", "logical", "logical"
                       , "logical", "logical", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "logical", "logical", "logical", "logical", "logical"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "numeric", "numeric"
                       , "numeric", "numeric", "numeric", "logical", "logical"
                       , "logical", "logical", "logical", "logical"
                     ), skip = 1)

dt_Code33<-data.table(Code33)
HQ_Code33<- dt_Code33[`Latest QTR Results` > -90 & Code33_GT_YoY==TRUE & Ltst_QoQ_GT==TRUE,.(NSE,`Next QTR Result Date`)]

# -------------------------------------
OneMonth_up<-EMA_RS_rnk_trnd[OneMonthUp==TRUE,unique(SYMBOL)]
EMA_RS_rnk_trnd_OneMonth_up<-EMA_RS_rnk_trnd[SYMBOL %in% OneMonth_UP[DATE>=DateArray[DateArrayLength] ,unique(SYMBOL)]]
Enter <- EMA_RS_rnk_trnd_OneMonth_up[SYMBOL %in% HQ_Code33[is.na(`Next QTR Result Date`)==TRUE,unique(NSE)] & CLOSE>EMA_50,unique(SYMBOL)]
Wait <- EMA_RS_rnk_trnd_OneMonth_up[SYMBOL %in% HQ_Code33[is.na(`Next QTR Result Date`)==FALSE,unique(NSE)] & CLOSE>EMA_50,unique(SYMBOL)]
Exit <- EMA_RS_rnk_trnd_OneMonth_up[DATE>=DateArray[DateArrayLength-1] & CLOSE<=EMA_50 & SYMBOL %in% HQ_Code33[,unique(NSE)],unique(SYMBOL)]
data.table(Enter[order(Enter)])
data.table(Wait[order(Wait)])
data.table(Exit[order(Exit)])
# -------------------------------------


# -------------------------------------
#Write Data_02_all_files_dataTable to .CSV
# -------------------------------------
File_EMA_RS_rnk_trnd = paste("File_EMA_RS_rnk_trnd_",toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
if (file.exists(File_EMA_RS_rnk_trnd))
{
  write.table(EMA_RS_rnk_trnd,file=File_EMA_RS_rnk_trnd,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
}else
{
  write.table(EMA_RS_rnk_trnd,file=File_EMA_RS_rnk_trnd,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
}


OneMonth_up<-EMA_RS_rnk_trnd[OneMonthUp==TRUE,unique(SYMBOL)]
File_OneMonth_up = paste("File_OneMonth_up_",toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
if (file.exists(File_OneMonth_up))
{
  write.table(OneMonth_up,file=File_OneMonth_up,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
}else
{
  write.table(OneMonth_up,file=File_OneMonth_up,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
}

# -------------------------------------
--Data_TrendforValidation[SYMBOL=='HONAUT' & DATE>'2020-02-01']
a=5
m=a
while(m>=0)
{
  anchor<-m
  DateArray = sort(Data_TrendforValidation[,unique(DATE)])
  DateArrayLength = length(sort(Data_TrendforValidation[,unique(DATE)]))
  
  date1<-DateArray[DateArrayLength-1-anchor]
  date2<-DateArray[DateArrayLength-anchor]
  date2_plus1<-DateArray[DateArrayLength-anchor+1]
  tdy<-DateArray[DateArrayLength-0]
  
  dt1<-data.table(Data_TrendforValidation[DATE==date1 & Uptrend_CL_GT150GT200==TRUE & (Vol_Trend*((1) * (IsO2C_PositiveUpTick == TRUE ) + (-1) * (IsO2C_PositiveUpTick == FALSE))<1) & EMA_VOL_10<EMA_VOL_80 ,unique(SYMBOL)])
  dt2<-data.table(Data_TrendforValidation[DATE==date2 & Uptrend_CL_GT150GT200==TRUE & (Vol_Trend*((1) * (IsO2C_PositiveUpTick == TRUE ) + (-1) * (IsO2C_PositiveUpTick == FALSE))>=2) & Vol_UpTrend_S==TRUE & Vol_UpTrend_L==TRUE & RS_UpTrend_L==TRUE & RS_UpTrend==TRUE & CLOSE>EMA_8,unique(SYMBOL)])
  
  setkey(dt1,V1)
  
  setkey(dt2,V1)
  dt3<-dt2[dt1, nomatch=0]
  dt3<-dt3[order(V1)]
  dt<-c(date1,date2)
  Enter<-dt3
  setnames(Enter, c("Enter") )
  print(dt)
  print(Enter)
  
  m=m-1
}

m=a
while(m>=0)
{
  anchor<-m
  DateArray = sort(Data_TrendforValidation[,unique(DATE)])
  DateArrayLength = length(sort(Data_TrendforValidation[,unique(DATE)]))
  
  date1<-DateArray[DateArrayLength-1-anchor]
  date2<-DateArray[DateArrayLength-anchor]
  date2_plus1<-DateArray[DateArrayLength-anchor+1]
  tdy<-DateArray[DateArrayLength-0]
  
  dt1<-data.table(Data_TrendforValidation[DATE==date1 & Uptrend_CL_GT150GT200==TRUE & Vol_UpTrend_L==TRUE & RS_UpTrend_L==TRUE,unique(SYMBOL)])
  dt2<-data.table(Data_TrendforValidation[DATE==date2 & Uptrend_CL_GT150GT200==TRUE & (CLOSE<=EMA_50) ,unique(SYMBOL)])

  setkey(dt1,V1)

  setkey(dt2,V1)
  dt3<-dt2[dt1, nomatch=0]
  dt3<-dt3[order(V1)]
  dt<-c(date1,date2)
  Exit<-dt3
  setnames(Exit, c("Exit") )
  print(dt)
  print(Exit)
  m=m-1
}


# EMA_RS_rnk_trnd[DATE==date2 & OneMonthUp==TRUE &Uptrend_CL_GT150GT200==TRUE & EMA_VOL_10>EMA_VOL_40 & EMA_VOL_40>EMA_VOL_80 &  EMA_RS_10>EMA_RS_40 & EMA_RS_40>EMA_RS_80 & CLOSE>(EMA_8),unique(SYMBOL)]
# EMA_RS_rnk_trnd[DATE==date2 & OneMonthUp==TRUE &Uptrend_CL_GT150GT200==TRUE & EMA_VOL_40<EMA_VOL_80 & EMA_RS_40>EMA_RS_80 & CLOSE>(EMA_8),unique(SYMBOL)]
# 
# EMA_RS_rnk_trnd[DATE %in% unique(unlist(list(dt))) & SYMBOL=='CAREERP']
# Data_TrendforValidation[DATE %in% unique(unlist(list(dt))) & SYMBOL %in% unique(unlist(list(dt3)))]
