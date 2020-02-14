library(TTR)
library(data.table)
library(lubridate)
#install.packages('Jmisc')
library(Jmisc)
library(readxl)
# -------------------------------------
#Define Working Directory, where files would be saved
# -------------------------------------
setwd('C:\\Users\\PRIYA\\Documents\\Stock Analysis\\Testing')
getwd()
# -------------------------------------
#Define start and end dates, and convert them into date format
# -------------------------------------
endDate = Sys.Date()-0
startDate = endDate-600

# -------------------------------------
#work with date, month, year for which data has to be extracted
# -------------------------------------
myDate = startDate
zippedFile <- tempfile()
#Remove the files in the folder
#do.call(file.remove, list(list.files()))
do.call(file.remove, list(all_files))
# Create a dataframe, to append all the csv's
all_files_dataTable = data.frame(DATE=as.Date(character()),SYMBOL=character(),OPEN=double(),HIGH=double(),LOW=double(),CLOSE=double(),LAST=double(),VOLUME=double(),stringsAsFactors=FALSE)
all_files_dataTable <- all_files_dataTable[0,]
# Create a dataframe, to process the csv's and load into another dataframe
temp = data.frame(DATE=as.Date(character()),SYMBOL=character(),OPEN=double(),HIGH=double(),LOW=double(),CLOSE=double(),LAST=double(),VOLUME=double(),stringsAsFactors=FALSE)
temp <- temp[0,]
# -------------------------------------
# Loop start for bhav file processing
# -------------------------------------
while (myDate <= endDate){
  filenameDate = paste(as.character(myDate, "%y%m%d"), ".csv", sep = "")
  monthfilename=paste(as.character(myDate, "%y%m"),".csv", sep = "")
  all_files = paste(toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
  all_files_final_unfiltered = paste("final_unfiltered_",toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
  all_files_final_filtered = paste("Signals_to_Analyze_",toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
  Signal_ema_8_cross = paste("Signal_ema_8_cross_",toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
  downloadfilename=paste("cm", toupper(as.character(myDate, "%d%b%Y")), "bhav.csv", sep = "")
  temp <- temp[0,]
  # -------------------------------------
  #Generate URL
  # -------------------------------------
  myURL = paste("https://www1.nseindia.com/content/historical/EQUITIES/", as.character(myDate, "%Y"), "/", toupper(as.character(myDate, "%b")), "/", downloadfilename, ".zip", sep = "")
  
  # ------------------------------------- 
  #retrieve Zipped file
  # -------------------------------------
  tryCatch({
    #Check if downloaded file already exists
    if (file.exists(downloadfilename))
      {
        # if exists, read csv file into temp variable
        temp <- read.csv(downloadfilename, sep = ",")
      }
      else 
        {
          # if not exist, 
          # 1. download the zip file and
          download.file(myURL,zippedFile, quiet=TRUE, mode="wb")
          # 2. Unzip and read csv file into temp variable
          #Unzip file and save it in temp
          temp <- read.csv(unzip(zippedFile), sep = ",") 
        }
    #Rename Columns Volume and Date
    colnames(temp)[9] <- "VOLUME"
    colnames(temp)[11] <- "DATE"
    #Define Date format
    temp$DATE <- as.Date(temp$DATE, format="%d-%b-%Y")
    # Keep only SERIES type EQ
    temp<- temp[temp$SERIES %in% c("EQ"), ]
    #Reorder Columns and Select relevant columns
    temp<-subset(temp,select=c("DATE","SYMBOL","OPEN","HIGH","LOW","CLOSE","LAST","VOLUME"))
    #As a backup, write the csv in all_files file
    if (file.exists(all_files))
      {
        write.table(temp,file=all_files,sep=",", eol="\n", row.names = FALSE, col.names = FALSE, append=TRUE)
      }
      else
        {
          write.table(temp,file=all_files,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
        }
    # Write to a data table
    all_files_dataTable <- rbind(all_files_dataTable,temp)
    #Print Progress
    #print(paste (myDate, "-Done!", endDate-myDate, "left"))
  }, error=function(err){
    #print(paste(myDate, "-No Record"))
    }
  )
  myDate <- myDate+1
  #print(paste(myDate, "Next Record"))
}

# -------------------------------------
#Delete temp file - Bhavcopy
# -------------------------------------
# junk <- dir(pattern="cm")
# file.remove(junk)

# -------------------------------------
# Finding the unique symbols
# -------------------------------------
symbol <- unique(all_files_dataTable[c("SYMBOL")])
#h <- head(symbol,3)
h <- symbol
#h <- c('ASIANPAINT','ABBOTINDIA','AMRUTANJAN')
l<-as.list(as.data.frame(t(h)))
#l
# -------------------------------------
# Finding Relative Strength to NIFTY_500
# -------------------------------------
datafile = data.frame(DATE=as.Date(character()),OPEN=double(),HIGH=double(),LOW=double(),CLOSE=double(),VOLUME=double(),TURNOVER=double(),stringsAsFactors=FALSE)
datafile <- datafile[0,]

x1<-read.csv('data_20160101_20161130.csv', sep = ",")
x2<-read.csv('data_20161201_20171130.csv', sep = ",")
x3<-read.csv('data_20171201_20181130.csv', sep = ",")
x4<-read.csv('data_20181201_20191130.csv', sep = ",")
x5<-read.csv('data_20191201_20200110.csv', sep = ",")

datafile<-rbind(x1,x2,x3,x4,x5)
dim(datafile)

datafile$Date <- as.Date(datafile$Date, format="%d-%b-%Y")

colnames(datafile)[1] <- "DATE"
colnames(datafile)[2] <- "OPEN"
colnames(datafile)[3] <- "HIGH"
colnames(datafile)[4] <- "LOW"
colnames(datafile)[5] <- "NIFTY500_CLOSE"
colnames(datafile)[6] <- "VOLUME"
colnames(datafile)[7] <- "TURNOVER_INR"

datafile<-subset(datafile,select=c("DATE","NIFTY500_CLOSE"))
datafile<-data.frame(datafile
           ,NIFTY500_CLOSE_10=EMA(datafile[c('NIFTY500_CLOSE')],n=10)
           ,NIFTY500_CLOSE_40=EMA(datafile[c('NIFTY500_CLOSE')],n=40)
           )
datafile<-data.frame(datafile
                     ,NIFTY500_UpTrend = datafile$NIFTY500_CLOSE_10>datafile$NIFTY500_CLOSE_40
                     )
datafile<-data.table(datafile)
setkey(datafile,DATE)

all_files_dataTable<-data.table(all_files_dataTable)
setkey(all_files_dataTable,DATE)

all_files_dataTable<-datafile[all_files_dataTable]
all_files_dataTable$RS<-all_files_dataTable[,.(CLOSE/NIFTY500_CLOSE)]
all_files_dataTable <- all_files_dataTable[, RS:=as.double(RS)]
all_files_dataTable<-data.frame(all_files_dataTable)
all_files_dataTable<-subset(all_files_dataTable,select=c("DATE","SYMBOL","OPEN","CLOSE","LOW","HIGH","LAST","VOLUME","NIFTY500_CLOSE", "NIFTY500_CLOSE_10", "NIFTY500_CLOSE_40","NIFTY500_UpTrend", "RS" ))


# **********************************************************************************************#
# ********** Finding the stocks with Relative Strength RANK ABOVE 90 ***************************#
# **********************************************************************************************#
# **********                         START                           ***************************#
# **********************************************************************************************#
Rank_all_fulllist<-data.frame(SYMBOL=character(),DATE=as.Date(character()),Rank_63=double(),Rank_126=double(),Rank_189=double(),Rank_252=double(),Rank_all=double())
Rank_all_fulllist<-data.table(Rank_all_fulllist)
Rank_all_fulllist<-Rank_all_fulllist[0,]

Rank_all_fulllist_detail<-data.frame(SYMBOL=character(), DATE=as.Date(character()), Day_01=as.Date(character()), Day_01_CLOSE=double(), Day_01_N500CLOSE=double()
                           , Day_63=as.Date(character()), Day_63_CLOSE=double(), Day_63_N500CLOSE=double(), C_63=double(), C_63_N500=double(), RS_63=double(), Rank_63=double()
                           , Day_126=as.Date(character()), Day_126_CLOSE=double(), Day_126_N500CLOSE=double(), C_126=double(), C_126_N500=double(), RS_126=double(), Rank_126=double()
                           , Day_189=as.Date(character()), Day_189_CLOSE=double(), Day_189_N500CLOSE=double(), C_189=double(), C_189_N500=double(), RS_189=double(), Rank_189=double()
                           , Day_252=as.Date(character()), Day_252_CLOSE=double(), Day_252_N500CLOSE=double(), C_252=double(), C_252_N500=double(), RS_252=double(), Rank_252=double()
                           , RS_all=double(), Rank_all=double())
Rank_all_fulllist_detail<-data.table(Rank_all_fulllist_detail)
Rank_all_fulllist_detail<-Rank_all_fulllist_detail[0,]


Rank_all_fulllist <- Rank_all_fulllist[0,]
Rank_all_fulllist_detail <- Rank_all_fulllist_detail[0,]

all_files_dataTable<-data.table(all_files_dataTable)
# all_files_dataTable<-all_files_dataTable[SYMBOL=='TIMKEN']
DateArray = sort(all_files_dataTable[,unique(DATE)],decreasing=TRUE)

m=0
loopcounter <- length(all_files_dataTable[,unique(DATE)])

while(m <= loopcounter)
{
  tryCatch(
    {
      Day_01<-all_files_dataTable[DATE==DateArray[m+1],.(Day_01=DATE,SYMBOL,Day_01_CLOSE=CLOSE,Day_01_N500CLOSE=NIFTY500_CLOSE)]
      setkey(Day_01,SYMBOL)
      
      Day_63<-all_files_dataTable[DATE==DateArray[m+(63*1)],.(Day_63=DATE,SYMBOL,Day_63_CLOSE=CLOSE,Day_63_N500CLOSE=NIFTY500_CLOSE)]
      setkey(Day_63,SYMBOL)
      RS_63<-Day_01[Day_63,nomatch=0]
      RS_63<-RS_63[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_63, Day_63_CLOSE, Day_63_N500CLOSE, C_63=(Day_01_CLOSE-Day_63_CLOSE)/Day_63_CLOSE, C_63_N500=(Day_01_N500CLOSE-Day_63_N500CLOSE)/Day_63_N500CLOSE)]
      RS_63<-RS_63[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_63, Day_63_CLOSE, Day_63_N500CLOSE, C_63, C_63_N500, RS_63=C_63/C_63_N500)]
      RS_63<-RS_63[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_63, Day_63_CLOSE, Day_63_N500CLOSE, C_63, C_63_N500, RS_63,Rank_63=100*(rank(RS_63)/length(RS_63)))]
      RS_63<-RS_63[order(-Rank_63)]
      setkey(RS_63,SYMBOL)
      
      
      Day_126<-all_files_dataTable[DATE==DateArray[m+(63*2)],.(Day_126=DATE,SYMBOL,Day_126_CLOSE=CLOSE,Day_126_N500CLOSE=NIFTY500_CLOSE)]
      setkey(Day_126,SYMBOL)
      RS_126<-Day_01[Day_126,nomatch=0]
      RS_126<-RS_126[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_126, Day_126_CLOSE, Day_126_N500CLOSE, C_126=(Day_01_CLOSE-Day_126_CLOSE)/Day_126_CLOSE, C_126_N500=(Day_01_N500CLOSE-Day_126_N500CLOSE)/Day_126_N500CLOSE)]
      RS_126<-RS_126[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_126, Day_126_CLOSE, Day_126_N500CLOSE, C_126, C_126_N500, RS_126=C_126/C_126_N500)]
      RS_126<-RS_126[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_126, Day_126_CLOSE, Day_126_N500CLOSE, C_126, C_126_N500, RS_126,Rank_126=100*(rank(RS_126)/length(RS_126)))]
      RS_126<-RS_126[order(-Rank_126)]
      setkey(RS_126,SYMBOL)
      
      Day_189<-all_files_dataTable[DATE==DateArray[m+(63*3)],.(Day_189=DATE,SYMBOL,Day_189_CLOSE=CLOSE,Day_189_N500CLOSE=NIFTY500_CLOSE)]
      setkey(Day_189,SYMBOL)
      RS_189<-Day_01[Day_189,nomatch=0]
      RS_189<-RS_189[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_189, Day_189_CLOSE, Day_189_N500CLOSE, C_189=(Day_01_CLOSE-Day_189_CLOSE)/Day_189_CLOSE, C_189_N500=(Day_01_N500CLOSE-Day_189_N500CLOSE)/Day_189_N500CLOSE)]
      RS_189<-RS_189[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_189, Day_189_CLOSE, Day_189_N500CLOSE, C_189, C_189_N500, RS_189=C_189/C_189_N500)]
      RS_189<-RS_189[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_189, Day_189_CLOSE, Day_189_N500CLOSE, C_189, C_189_N500, RS_189,Rank_189=100*(rank(RS_189)/length(RS_189)))]
      RS_189<-RS_189[order(-Rank_189)]
      setkey(RS_189,SYMBOL)
      
      Day_252<-all_files_dataTable[DATE==DateArray[m+(63*4)],.(Day_252=DATE,SYMBOL,Day_252_CLOSE=CLOSE,Day_252_N500CLOSE=NIFTY500_CLOSE)]
      setkey(Day_252,SYMBOL)
      RS_252<-Day_01[Day_252,nomatch=0]
      RS_252<-RS_252[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_252, Day_252_CLOSE, Day_252_N500CLOSE, C_252=(Day_01_CLOSE-Day_252_CLOSE)/Day_252_CLOSE, C_252_N500=(Day_01_N500CLOSE-Day_252_N500CLOSE)/Day_252_N500CLOSE)]
      RS_252<-RS_252[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_252, Day_252_CLOSE, Day_252_N500CLOSE, C_252, C_252_N500, RS_252=C_252/C_252_N500)]
      RS_252<-RS_252[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE, Day_252, Day_252_CLOSE, Day_252_N500CLOSE, C_252, C_252_N500, RS_252,Rank_252=100*(rank(RS_252)/length(RS_252)))]
      RS_252<-RS_252[order(-Rank_252)]
      setkey(RS_252,SYMBOL)
      
      RS_all<-RS_63[RS_126[,.(SYMBOL, Day_126, Day_126_CLOSE, Day_126_N500CLOSE, C_126, C_126_N500, RS_126, Rank_126)],nomatch=0]
      RS_all<-RS_all[RS_189[,.(SYMBOL, Day_189, Day_189_CLOSE, Day_189_N500CLOSE, C_189, C_189_N500, RS_189,Rank_189)],nomatch=0]
      RS_all<-RS_all[RS_252[,.(SYMBOL, Day_252, Day_252_CLOSE, Day_252_N500CLOSE, C_252, C_252_N500, RS_252,Rank_252)],nomatch=0]
      RS_all<-RS_all[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE
                        , Day_63, Day_63_CLOSE, Day_63_N500CLOSE, C_63, C_63_N500, RS_63, Rank_63
                        , Day_126, Day_126_CLOSE, Day_126_N500CLOSE, C_126, C_126_N500, RS_126, Rank_126
                        , Day_189, Day_189_CLOSE, Day_189_N500CLOSE, C_189, C_189_N500, RS_189, Rank_189
                        , Day_252, Day_252_CLOSE, Day_252_N500CLOSE, C_252, C_252_N500, RS_252, Rank_252
                        , RS_all=RS_63*.4 + RS_126*.3 + RS_189*.2 + RS_252*.1)]
      RS_all<-RS_all[,.(SYMBOL, Day_01, Day_01_CLOSE, Day_01_N500CLOSE
                        , Day_63, Day_63_CLOSE, Day_63_N500CLOSE, C_63, C_63_N500, RS_63, Rank_63
                        , Day_126, Day_126_CLOSE, Day_126_N500CLOSE, C_126, C_126_N500, RS_126, Rank_126
                        , Day_189, Day_189_CLOSE, Day_189_N500CLOSE, C_189, C_189_N500, RS_189, Rank_189
                        , Day_252, Day_252_CLOSE, Day_252_N500CLOSE, C_252, C_252_N500, RS_252, Rank_252
                        , RS_all, Rank_all=100*(rank(RS_all)/length(RS_all)))]

      RS_all<-RS_all[order(-Rank_all)]
      Rank_all_summary<-RS_all[,.(SYMBOL,DATE=Day_01,Rank_63,Rank_126,Rank_189,Rank_252,Rank_all)]
      Rank_all_fulllist<-rbind(Rank_all_fulllist,Rank_all_summary)
      #print(Rank_all_fulllist)
      
      
      Rank_all_detail<-RS_all[,.(SYMBOL, DATE=Day_01, Day_01, Day_01_CLOSE, Day_01_N500CLOSE
                                  , Day_63, Day_63_CLOSE, Day_63_N500CLOSE, C_63, C_63_N500, RS_63, Rank_63
                                  , Day_126, Day_126_CLOSE, Day_126_N500CLOSE, C_126, C_126_N500, RS_126, Rank_126
                                  , Day_189, Day_189_CLOSE, Day_189_N500CLOSE, C_189, C_189_N500, RS_189, Rank_189
                                  , Day_252, Day_252_CLOSE, Day_252_N500CLOSE, C_252, C_252_N500, RS_252, Rank_252
                                  , RS_all, Rank_all)]
      Rank_all_fulllist_detail<-rbind(Rank_all_fulllist_detail,Rank_all_detail)
      #print(Rank_all_fulllist_detail)
      
    }, error=function(e){cat("ERROR : @",j,conditionMessage(e), "\n")})
  m=m+1}
#Rank_all_fulllist[SYMBOL=='TIMKEN' & DATE>='2019-08-05' & DATE<'2019-08-07']

#Rank_all_fulllist_detail[DATE>='2019-08-05' & DATE<'2019-08-07']
# tofile<-Rank_all_fulllist_detail[DATE>='2019-08-05' & DATE<'2019-08-07']
# 
# File_tofile = paste("File_tofile_",toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
# if (file.exists(File_tofile))
# {
#   write.table(tofile,file=File_tofile,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
# }else
# {
#   write.table(tofile,file=File_tofile,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
# }


#Rank_all_fulllist[SYMBOL %in% c("DEEPAKNTR","GSPL","HDFC","INFY","MCX","MUTHOOTFIN","NESCO","PVR","VOLTAMP","AAVAS","GMMPFAUDLR","LINDEINDIA","MIDHANI","TIMKEN")]
#HOLDING<-as.factor(c("DEEPAKNTR","GSPL","HDFC","INFY","MCX","MUTHOOTFIN","NESCO","PVR","VOLTAMP","AAVAS","GMMPFAUDLR","LINDEINDIA","MIDHANI","TIMKEN"))
#Rank_all_fulllist[order(-Rank_63) & Rank_63>=95 & Rank_all>=90]

# **********************************************************************************************#
# ********** Finding the stocks with Relative Strength RANK ABOVE 90 ***************************#
# **********************************************************************************************#
# **********                          END                            ***************************#
# **********************************************************************************************#


# -------------------------------------
# Setting up empty dataframes to append rows when we loop and process the data
# -------------------------------------
Data_00_all_files_dataTable = data.frame(DATE=as.Date(character()),SYMBOL=character()
                                         ,OPEN=double(),HIGH=double(),LOW=double()
                                         ,CLOSE=double(),LAST=double(),VOLUME=double()
                                         ,NIFTY500_CLOSE=double(), NIFTY500_CLOSE_10=double(), NIFTY500_CLOSE_40=double(), NIFTY500_UpTrend=logical()
                                         ,RS=double(),stringsAsFactors=FALSE)
Data_00_all_files_dataTable <- Data_00_all_files_dataTable[0,]
Data_01_all_files_dataTable  = data.frame(DATE=as.Date(character()),SYMBOL=character()
                                          ,OPEN=double(),HIGH=double(),LOW=double()
                                          ,CLOSE=double(),LAST=double()
                                          ,VOLUME=double()
                                          ,EMA_200 = double(),EMA_150 = double(), EMA_50 = double(),EMA_8 = double()
                                          ,EMA_VOL_10 = double(),EMA_VOL_40 = double(),EMA_VOL_80 = double()
                                          ,EMA_VOL_250 = double()
                                          ,RSI_5 = double()
                                          ,EMA_RS_10 = double(),EMA_RS_80 = double()
                                          ,EMA_RS_40 = double()
                                          ,NIFTY500_CLOSE_10 = double()
                                          ,NIFTY500_CLOSE_40 = double()
                                          ,NIFTY500_UpTrend = logical()
                                          ,RS_UpTrend = logical()
                                          ,Vol_Trend = double(), Vol_UpTrend_S = logical(), Vol_UpTrend_L = logical()
                                          ,Uptrend = logical()
                                          ,IsO2C_PositiveUpTick = logical()
                                          ,stringsAsFactors=FALSE)
Data_01_all_files_dataTable <- Data_01_all_files_dataTable[0,]
Data_02_all_files_dataTable  = data.frame(DATE=as.Date(character()),SYMBOL=character()
                                          ,OPEN=double(),HIGH=double(),LOW=double()
                                          ,CLOSE=double(),LAST=double()
                                          ,VOLUME=double()
                                          ,EMA_200 = double(),EMA_150 = double(), EMA_50 = double(),EMA_8 = double()
                                          ,EMA_VOL_10 = double(),EMA_VOL_40 = double(),EMA_VOL_80 = double()
                                          ,EMA_VOL_250 = double()
                                          ,RSI_5 = double()
                                          ,EMA_RS_10 = double(),EMA_RS_80 = double()
                                          ,EMA_RS_40 = double()
                                          ,NIFTY500_CLOSE_10 = double()
                                          ,NIFTY500_CLOSE_40 = double()
                                          ,NIFTY500_UpTrend = logical()
                                          ,RS_UpTrend = logical()
                                          ,RS_UpTrend_L = logical()
                                          ,Vol_Trend = double(), Vol_UpTrend_S = logical(), Vol_UpTrend_L = logical()
                                          ,Uptrend = logical()
                                          ,Uptrend_CL_GT150GT200 = logical()
                                          ,IsO2C_PositiveUpTick = logical()
                                          ,stringsAsFactors=FALSE
                                          )
Data_02_all_files_dataTable <- Data_02_all_files_dataTable[0,]
print(all_files_dataTable[,max(DATE)])
# -------------------------------------
#Looping to add EMA200, EMA150 and EMA50 AND VOLUME EMA 20
# -------------------------------------
all_files_dataTable<-data.frame(all_files_dataTable)

for(i in l)
{
  for(j in i)
  {
    tryCatch(
      {
        Data_00_all_files_dataTable<-subset(all_files_dataTable,SYMBOL==j)
        
        Data_01_all_files_dataTable<-data.frame(Data_00_all_files_dataTable
                                                ,EMA_200=EMA(Data_00_all_files_dataTable[c('CLOSE')],n=200)
                                                ,EMA_150=EMA(Data_00_all_files_dataTable[c('CLOSE')],n=180)
                                                ,EMA_50=EMA(Data_00_all_files_dataTable[c('CLOSE')],n=50)
                                                ,EMA_8=EMA(Data_00_all_files_dataTable[c('CLOSE')],n=20)
                                                ,EMA_VOL_10=EMA(Data_00_all_files_dataTable[c('VOLUME')],n=10)
                                                ,EMA_VOL_40=EMA(Data_00_all_files_dataTable[c('VOLUME')],n=40)
                                                ,EMA_VOL_80=EMA(Data_00_all_files_dataTable[c('VOLUME')],n=80)
                                                ,EMA_VOL_250=EMA(Data_00_all_files_dataTable[c('VOLUME')],n=250)
                                                ,RSI_5 = RSI(Data_00_all_files_dataTable[c('CLOSE')],n=5)
                                                ,EMA_RS_10=EMA(Data_00_all_files_dataTable[c('RS')],n=10)
                                                ,EMA_RS_80=EMA(Data_00_all_files_dataTable[c('RS')],n=80)
                                                ,EMA_RS_40=EMA(Data_00_all_files_dataTable[c('RS')],n=40)
        )
        Data_01_all_files_dataTable<-data.frame(Data_01_all_files_dataTable
                                                ,RS_UpTrend = Data_01_all_files_dataTable$EMA_RS_10>Data_01_all_files_dataTable$EMA_RS_40
                                                ,RS_UpTrend_L = Data_01_all_files_dataTable$EMA_RS_40>Data_01_all_files_dataTable$EMA_RS_80
                                                ,Vol_Trend = Data_01_all_files_dataTable$VOLUME/Data_01_all_files_dataTable$EMA_VOL_250
                                                ,Vol_UpTrend_S = Data_01_all_files_dataTable$EMA_VOL_10>Data_01_all_files_dataTable$EMA_VOL_40
                                                ,Vol_UpTrend_L = Data_01_all_files_dataTable$EMA_VOL_40>Data_01_all_files_dataTable$EMA_VOL_80
                                                ,Uptrend = (
                                                  (Data_01_all_files_dataTable$CLOSE>Data_01_all_files_dataTable$EMA_50)
                                                  & (Data_01_all_files_dataTable$LOW>Data_01_all_files_dataTable$EMA_150)
                                                  & (Data_01_all_files_dataTable$LOW>Data_01_all_files_dataTable$EMA_200)
                                                  & (Data_01_all_files_dataTable$EMA_50>Data_01_all_files_dataTable$EMA_150)
                                                  & (Data_01_all_files_dataTable$EMA_50>Data_01_all_files_dataTable$EMA_200)
                                                  & (Data_01_all_files_dataTable$EMA_150>Data_01_all_files_dataTable$EMA_200)
                                                )
                                                ,Uptrend_CL_GT150GT200 = (
                                                  (Data_01_all_files_dataTable$LOW>Data_01_all_files_dataTable$EMA_150)
                                                  & (Data_01_all_files_dataTable$LOW>Data_01_all_files_dataTable$EMA_200)
                                                  & (Data_01_all_files_dataTable$EMA_150>Data_01_all_files_dataTable$EMA_200)
                                                )
                                                ,IsO2C_PositiveUpTick = (Data_01_all_files_dataTable$CLOSE>Data_01_all_files_dataTable$OPEN)
        )
        Data_02_all_files_dataTable <- rbind(Data_02_all_files_dataTable,Data_01_all_files_dataTable)
      }, error=function(e){cat("ERROR : @",j,conditionMessage(e), "\n")})
    }
}
Data_02_all_files_dataTable <- data.table(Data_02_all_files_dataTable)
Data_02_all_files_dataTable[, Month_Yr:= format(as.Date(Data_02_all_files_dataTable$DATE), "%Y-%m")]

Data_TrendforValidation <- Data_TrendforValidation[0,]
Data_TrendforValidation <- Data_02_all_files_dataTable
#Data_TrendforValidation[SYMBOL=='ADSL' & DATE>'2019-12-01']
print(Data_TrendforValidation[,max(DATE)])

# -------------------------------------
# Finding Uptrend for atleast One Quater
# 1. Low greater than EMA 150 
# 2. EMA 150 > EMA 200
# -------------------------------------
MonthRangeStart<-0
MonthRangeEnd<-MonthRangeStart+1
#HOLDING<-as.factor(c("DEEPAKNTR","GSPL","HDFC","INFY","MCX","MUTHOOTFIN","NESCO","PVR","VOLTAMP","AAVAS","GMMPFAUDLR","LINDEINDIA","MIDHANI","TIMKEN"))

# Finding SYMBOLs with Price in UpTrend for One Month & write to "Data_TrendforValidation"
  Data_03_all_files_dataTable <- Data_03_all_files_dataTable[0,]
  as.Date(Data_TrendforValidation[,max(DATE)]) %m-% months(MonthRangeEnd)
  Data_03_all_files_dataTable<-Data_TrendforValidation[DATE>=as.Date(Data_TrendforValidation[,max(DATE)]) %m-% months(MonthRangeEnd)]
  as.Date(Data_03_all_files_dataTable[,max(DATE)]) %m-% months(MonthRangeStart)
  Data_03_all_files_dataTable<-Data_03_all_files_dataTable[DATE<as.Date(Data_03_all_files_dataTable[,max(DATE)]) %m-% months(MonthRangeStart)]
  Data_03_all_files_dataTable <- data.table(Data_03_all_files_dataTable)
  A <- Data_03_all_files_dataTable[, .(`Number of rows` = .N), by = list(SYMBOL)]
  B <- data.table(Data_03_all_files_dataTable[, .(`Number of Trues` = .N), by = list(SYMBOL,Uptrend_CL_GT150GT200)],key='SYMBOL')
  Data_03_all_files_dataTable <- data.table(B[A, on="SYMBOL"])
  Data_03_all_files_dataTable <- Data_03_all_files_dataTable[Uptrend_CL_GT150GT200==TRUE]
  Data_03_all_files_dataTable <- data.frame(Data_03_all_files_dataTable,
                                          OneQTR_UpTrend = Data_03_all_files_dataTable$`Number of rows`==Data_03_all_files_dataTable$`Number of Trues`
                                          )
  Data_03_all_files_dataTable <- data.table(Data_03_all_files_dataTable)
  Data_03_all_files_dataTable <- Data_03_all_files_dataTable[OneQTR_UpTrend==TRUE]
  Data_03_all_files_dataTable <- data.frame(Data_03_all_files_dataTable)
  One_QTR_UpTrend<-unique(Data_03_all_files_dataTable[c("SYMBOL")])
  One_QTR_UpTrend <- One_QTR_UpTrend$SYMBOL
  #One_QTR_UpTrend <- unique(unlist(list(One_QTR_UpTrend,HOLDING)))
  Data_TrendforValidation <- subset(Data_TrendforValidation,Data_TrendforValidation$SYMBOL %in% One_QTR_UpTrend)
  OneMonth_UP<-Data_TrendforValidation
  #OneMonth_UP[,unique(SYMBOL)]

print(Data_TrendforValidation[,max(DATE)])

# # Finding SYMBOLs with VOLUME EMA_40 > EMA_80 & write to "Data_TrendforValidation"
#   as.Date(Data_TrendforValidation[,max(DATE)]) %m-% months(MonthRangeEnd)
#   p<-Data_TrendforValidation[DATE>=as.Date(Data_TrendforValidation[,max(DATE)]) %m-% months(MonthRangeEnd)]
#   as.Date(p[,max(DATE)]) %m-% months(MonthRangeStart)
#   q<-p[DATE<as.Date(p[,max(DATE)]) %m-% months(MonthRangeStart)]
#   SYMBOL_Vol_UpTrend <- q[Vol_UpTrend_L==TRUE,unique(SYMBOL)]
#   Data_TrendforValidation <- subset(Data_TrendforValidation,Data_TrendforValidation$SYMBOL %in% SYMBOL_Vol_UpTrend)
#   #Data_TrendforValidation[,unique(SYMBOL)]
#   #Data_TrendforValidation[SYMBOL=='TIMKEN' & DATE>='2020-01-01']
# 
#   # Finding SYMBOLs with RELATICE STRENGTH EMA_40 > EMA_80  & write to "Data_TrendforValidation"
#   as.Date(Data_TrendforValidation[,max(DATE)]) %m-% months(MonthRangeEnd)
#   r<-Data_TrendforValidation[DATE>=as.Date(Data_TrendforValidation[,max(DATE)]) %m-% months(MonthRangeEnd)]
#   as.Date(r[,max(DATE)]) %m-% months(MonthRangeStart)
#   s<-r[DATE<as.Date(r[,max(DATE)]) %m-% months(MonthRangeStart)]
#   SYMBOL_RS_UpTrend <- q[RS_UpTrend_L==TRUE,unique(SYMBOL)]
#   #SYMBOL_RS_UpTrend <- unique(unlist(list(SYMBOL_RS_UpTrend,HOLDING)))
#   Data_TrendforValidation <- subset(Data_TrendforValidation,Data_TrendforValidation$SYMBOL %in% SYMBOL_RS_UpTrend)
#   Data_TrendforValidation[,unique(SYMBOL)]

  setkey(Data_02_all_files_dataTable,SYMBOL,DATE)
  setkey(Rank_all_fulllist,SYMBOL,DATE)
  
  Data_TrendforValidation<-Rank_all_fulllist[Data_02_all_files_dataTable,nomatch=0]
  Data_TrendforValidation<-Data_TrendforValidation[order(-Rank_63,-Rank_all),.(SYMBOL,DATE,OPEN,CLOSE,LOW,HIGH,LAST,VOLUME
                                                      ,NIFTY500_CLOSE,NIFTY500_CLOSE_10,NIFTY500_CLOSE_40,NIFTY500_UpTrend
                                                      ,RS,EMA_200,EMA_150,EMA_50,EMA_8,EMA_VOL_10,EMA_VOL_40,EMA_VOL_80
                                                      ,EMA_VOL_250,RSI_5,EMA_RS_10,EMA_RS_80,EMA_RS_40,RS_UpTrend,RS_UpTrend_L,Vol_Trend,Vol_UpTrend_S
                                                      ,Vol_UpTrend_L,Uptrend,Uptrend_CL_GT150GT200,IsO2C_PositiveUpTick,Month_Yr
                                                      ,Rank_63,Rank_126,Rank_189,Rank_252,Rank_all
                                                      ,OneMonthUp=SYMBOL %in% OneMonth_UP[,unique(SYMBOL)])]

  ltst_Rank<-Rank_all_fulllist[DATE==DateArray[1],.(SYMBOL, ltst_Rank_63=Rank_63, ltst_Rank_126=Rank_126, ltst_Rank_189=Rank_189, ltst_Rank_252=Rank_252, ltst_Rank_all=Rank_all)]
  
  setkey(Data_TrendforValidation,SYMBOL)
  setkey(ltst_Rank,SYMBOL)  
  
  Data_TrendforValidation<-ltst_Rank[Data_TrendforValidation,nomatch=0]
  
  Data_TrendforValidation[,unique(SYMBOL)]


  setkey(Data_02_all_files_dataTable,SYMBOL,DATE)
  Data_02_all_files_dataTable<-Rank_all_fulllist[Data_02_all_files_dataTable,nomatch=0]

# -------------------------------------
#Write Data_02_all_files_dataTable to .CSV
# -------------------------------------
File_Data_TrendforValidation = paste("File_Data_TrendforValidation_",toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
if (file.exists(File_Data_TrendforValidation))
{
  write.table(Data_TrendforValidation[DATE>'2019-07-01'],file=File_Data_TrendforValidation,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
}else
{
  write.table(Data_TrendforValidation[DATE>'2019-07-01'],file=File_Data_TrendforValidation,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
}

  
# ***********************************************************************
# ***********************************************************************
# *************                STOP                    ******************  
# ***********************************************************************
# ***********************************************************************
  
# -------------------------------------
# # FInding 52 week high - NEED TO CREATE A LOOP *******
# -------------------------------------
# Data_02_all_files_dataTable[,min(DATE)]
# Data_02_all_files_dataTable[,max(DATE)]
# z <- as.list(max(as.Date(unlist(DateArray),origin = "1970-01-01")))
result_52wH  = data.frame(SYMBOL=character()
                          ,DATE_52wH=as.Date(character())
                          ,HIGH_52wH=double()
                          ,rangeStart = as.Date(character())
                          ,DATE = as.Date(character())
                          ,stringsAsFactors=FALSE)

result_52wH <- result_52wH[0,]
loopcounter <- length(Data_02_all_files_dataTable[,unique(DATE)])-250
DateArray = sort(Data_02_all_files_dataTable[,unique(DATE)])
symbol_52wH <- unique(data.frame(Data_02_all_files_dataTable$SYMBOL))
NoOfSymbols <- length(unlist(symbol_52wH))
DateArray[1]
# h_52wH <- head(symbol_52wH,3)
h_52wH <- symbol_52wH
l_52wH<-as.list(as.data.frame(t(h_52wH)))

#l_52wH
x<-0
#l_52wH='SBILIFE'
# unlist(c(l_52wH[c(1)], l_52wH[c(length(l_52wH))]))
for(i in l_52wH){
  # print(i)
  for(j in i){
    x<-x+1
    print((paste(j,x,"/",as.character(NoOfSymbols))))
    m=1
    while(m <= loopcounter)
    {
      tryCatch(
        {
          # print(m)[]
          # print(DateArray[c(0+m)])
          # print(DateArray[c(250+m)])
          # print(DateArray[c(0+m:250+m)])
          TestHigh    <- Data_02_all_files_dataTable[(DATE >= DateArray[c(0+m)] & DATE <= DateArray[c(250+m)]) & SYMBOL == j]
          result      <- TestHigh[,.SD[which.max(HIGH)]]
          result      <- cbind(result[,.(SYMBOL,DATE,HIGH)],DateArray[c(0+m)],DateArray[c(250+m)])
          setnames(result, old=c("DATE","HIGH","V2","V3"), new=c("DATE_52wH","HIGH_52wH","rangeStart","DATE"))
          result_52wH <- rbind(result_52wH,result)
          #m=m+1
          # print(result)
        }, error=function(e){cat("ERROR : @",j,conditionMessage(e), "\n")})
      m=m+1}
  }
}
result_52wH

Data_52High_06_52WeekHighWithLimitedColumns<-merge(Data_02_all_files_dataTable,result_52wH,by=c("SYMBOL","DATE"),all.x = TRUE)
# -------------------------------------
# -------------------------------------
# Finding GAP 
# -------------------------------------
x<-data.table(sort(Data_52High_06_52WeekHighWithLimitedColumns[,unique(DATE)]))
x$uid1<-1:nrow(x)
x$uid2<-x[,uid1-1]
DateAray__x<-x[,.(V1,uid1)]
DateAray__y<-x[,.(V1,uid2)]
setkey(DateAray__x,uid1)
setkey(DateAray__y,uid2)
DateAray__z<-DateAray__y[DateAray__x, nomatch=0]
setkey(DateAray__z,V1)
setkey(DateAray__z,i.V1)
setkey(Data_52High_06_52WeekHighWithLimitedColumns,DATE,SYMBOL)
H52<-Data_52High_06_52WeekHighWithLimitedColumns[DateAray__z]
H52_Cl<-H52[,.(SYMBOL,DATE,CLOSE)]
setkey(H52_Cl,SYMBOL,DATE)
setkey(H52,SYMBOL,V1)
H52_final<-H52[H52_Cl,nomatch=0]
H52_final<-H52_final[,.(SYMBOL,DATE,OPEN,HIGH,LOW,CLOSE,LAST,VOLUME,EMA_200,EMA_150,EMA_50,EMA_8,EMA_VOL_250,RSI_5,Vol_Trend,Uptrend,Uptrend_CL_GT150GT200,IsO2C_PositiveUpTick,DATE_52wH,HIGH_52wH,rangeStart,Prev_DATE=V1,Prev_CLOSE=i.CLOSE, GAP=OPEN-i.CLOSE, GAP_perc=((OPEN-i.CLOSE)*100)/i.CLOSE)]
# -------------------------------------
# Volume Analysis
# -------------------------------------

#SubsetData01<-Data_52High_06_52WeekHighWithLimitedColumns[Vol_Trend>=1]
SubsetData01<-H52_final
SubsetData01<-SubsetData01[,.(SYMBOL, DATE, Uptrend, VOLUME, OPEN, CLOSE, LOW, HIGH, EMA_8, RSI_5=round(RSI_5,1), Vol_Trend=round(Vol_Trend,1), IsO2C_PositiveUpTick, DATE_52wH,HIGH_52wH, Prev_DATE, Prev_CLOSE, GAP, GAP_perc, LOWp_52wH= round(100*(1-((HIGH_52wH-LOW)/HIGH_52wH)),1), LOW_GT75p_of52H=LOW>=(HIGH_52wH*.75))]
SubsetData01<-SubsetData01[, Tick := (1) * (IsO2C_PositiveUpTick == TRUE ) + (-1) * (IsO2C_PositiveUpTick == FALSE)]
SubsetData01<-SubsetData01[,Vol_Trend:=Vol_Trend*Tick]
SubsetData01<-SubsetData01[,Vol:=VOLUME*Tick]

SubsetData01[, UpMove := (1) * (IsO2C_PositiveUpTick == TRUE ) + (0) * (IsO2C_PositiveUpTick == FALSE)]
SubsetData01[, DownMove := (0) * (IsO2C_PositiveUpTick == TRUE ) + (1) * (IsO2C_PositiveUpTick == FALSE)]
SubsetData01[, AllMove := (1) * (IsO2C_PositiveUpTick == TRUE ) + (1) * (IsO2C_PositiveUpTick == FALSE)]
SubsetData01[, Month_Yr:= format(as.Date(SubsetData01$DATE), "%Y-%m")]
SubsetData01<-SubsetData01[,.(SYMBOL, DATE, Month_Yr, Uptrend, OPEN, CLOSE, LOW, HIGH, EMA_8, RSI_5=round(RSI_5,1), Vol_Trend=round(Vol_Trend,1), AllMove, UpMove, DownMove, DATE_52wH,HIGH_52wH, LOWp_52wH, LOW_GT75p_of52H, VOLUME, Vol, Prev_DATE, Prev_CLOSE, GAP, GAP_perc)]

Summary01<-SubsetData01[, .(Sum_AllMove=sum(AllMove), Sum_UpMove=sum(UpMove),Sum_DownMove=sum(DownMove)), by = .(SYMBOL, Month_Yr)]

setkey(SubsetData01,SYMBOL,Month_Yr)
setkey(Summary01,SYMBOL,Month_Yr)

SubsetData02<-Summary01[SubsetData01]
SubsetData02<-SubsetData02[,.(SYMBOL, DATE, Month_Yr, Uptrend, OPEN, CLOSE, LOW, HIGH, EMA_8, RSI_5, Vol_Trend, AllMove=Sum_AllMove, UpMove=Sum_UpMove, DownMove=Sum_DownMove,  DATE_52wH,HIGH_52wH, LOWp_52wH, LOW_GT75p_of52H, VOLUME, Vol, Prev_DATE, Prev_CLOSE, GAP, GAP_perc)]

  # -------------------------------------
  #SubsetData02 to .CSV
  # -------------------------------------
  
  File_SubsetData02 = paste("File_SubsetData02",".csv",sep="")
  if (file.exists(File_SubsetData02))
  {
    write.table(SubsetData02,file=File_SubsetData02,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
  }else
  {
    write.table(SubsetData02,file=File_SubsetData02,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
  }
# -------------------------------------

# -------------------------------------
# Write Data_06_52WeekHighWithLimitedColumns to .CSV
# -------------------------------------
File_Data_06_52WeekHighWithLimitedColumns = paste("File_Data_06_52WeekHighWithLimitedColumns_",toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
if (file.exists(File_Data_06_52WeekHighWithLimitedColumns))
{
  write.table(Data_52High_06_52WeekHighWithLimitedColumns,file=File_Data_06_52WeekHighWithLimitedColumns,sep=",", eol="\n", row.names = FALSE, col.names = FALSE, append=TRUE)
}else
{
  write.table(Data_52High_06_52WeekHighWithLimitedColumns,file=File_Data_06_52WeekHighWithLimitedColumns,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
}

# -------------------------------------
# Write Data.CSV
# -------------------------------------
File_Data_TrendforValidation = paste("File_Data_TrendforValidation_",toupper(as.character(startDate, "%d%b%Y")),"_",toupper(as.character(endDate, "%d%b%Y")),".csv",sep="")
if (file.exists(File_Data_TrendforValidation))
{
  write.table(Data_TrendforValidation[SYMBOL=='VOLTAS'],file=File_Data_TrendforValidation,sep=",", eol="\n", row.names = FALSE, col.names = FALSE, append=TRUE)
}else
{
  write.table(Data_TrendforValidation[SYMBOL=='VOLTAS'],file=File_Data_TrendforValidation,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
}

# -------------------------------------
#Copying all files to a different path
# -------------------------------------
FileCopyPatternCSV <- list.files(pattern='File_Data_')
file.copy(FileCopyPatternCSV,
          to = "C:\\Users\\PRIYA\\Documents\\Stock Analysis\\Files", recursive = TRUE,
          overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)

# -------------------------------------
# Removing all csv files from Soruce path
# -------------------------------------
file.remove(FileCopyPatternCSV)

# -------------------------------------
GAP_HighVol_52wH<-SubsetData02[DATE==DATE_52wH & Vol_Trend>=2 & GAP_perc>1 & DATE>'2020-01-01',.(SYMBOL, DATE, Month_Yr,  GAP_perc, Vol_Trend, GAP, RSI_5, Uptrend, OPEN, CLOSE, LOW, HIGH, EMA_8, AllMove, UpMove, DownMove,  DATE_52wH,HIGH_52wH, LOWp_52wH, LOW_GT75p_of52H, VOLUME, Vol, Prev_DATE, Prev_CLOSE)]
GAP_HighVol_52wH[order(DATE)]