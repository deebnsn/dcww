library(tidyr)
library(zoo)
library(data.table)
library(reshape)

#----------------------------------WTW Samples-----------------------------------------------------------------------------------------------

load("QDB_wtw_Clean.RData")

wtw_complete[,c("Lab_Ref","SPT_No","Purpose_Code","NGR")] <- NULL
colnames(wtw_complete)[5] <- "WTW"

wtw_complete$Date <- substr(wtw_complete$Date_Time,1,10)
wtw_complete$Date_Time <- NULL
wtw_complete$Date <- as.Date(wtw_complete$Date,format='%d/%m/%Y')

det <- read.csv('Det_Lookup.csv')
wtw_complete <- merge(wtw_complete,det,by="Det_Code")
wtw_complete$Det_Code <- NULL

wtw_complete$Result <- as.numeric(wtw_complete$Result)

wtw_complete$week <- strftime(as.POSIXct(wtw_complete$Date),format="%W")
wtw_complete$year <- strftime(as.POSIXct(wtw_complete$Date),format="%Y")

wtw_complete <- drop_na(wtw_complete,WQZ)
wtw_complete <- drop_na(wtw_complete,Result)

wtwMean <- aggregate(wtw_complete$Result, by=list(WTW=wtw_complete$WQZ,Det_Name=wtw_complete$Det_Name,Year=wtw_complete$year,Week=wtw_complete$week),data=wtw_complete,FUN=mean, na.action=na.pass, na.rm=TRUE)
colnames(wtwMean)[5] <- "Mean"
wtwMean <- reshape(wtwMean, idvar = c("WTW","Year","Week"), timevar = "Det_Name", direction = "wide")
wtwMean$`Mean.Iron (Unflushed)` <- NULL

wtwMean <- wtwMean[order(wtwMean$WTW,wtwMean$Year,wtwMean$Week),]

table(wtwMean$WTW)
wtwMean <- wtwMean[wtwMean$WTW!="G10",]
wtwMean <- wtwMean[wtwMean$WTW!="H70",]
wtwMean <- wtwMean[wtwMean$WTW!="B14",]

weekD <-wtwMean[,c("Year","Week")]
weekD$Week <- as.numeric(weekD$Week)
weekD <- unique(weekD)

temp <- list()
wtw <- unique(wtwMean$WTW)
for (i in 1:length(wtw)){
  temp[[i]] <- wtwMean[wtwMean$WTW==wtw[i],]
  temp[[i]] <- merge(weekD,temp[[i]],by=c("Year","Week"),all.x=T)
  temp[[i]][3] <- wtw[i]
}
wtwFull <- data.frame(do.call(rbind,temp))

temp <- list()
wtwTemp <- list()
joiner <- list()
for (i in 1:length(wtw)){
  temp[[i]] <- wtwFull[wtwFull$WTW==wtw[i],]
  wtwTemp[[i]] <- temp[[i]][3]
  temp[[i]] <- na.approx(temp[[i]][-3],rule=2)
  joiner[[i]] <- cbind(wtwTemp[[i]],temp[[i]]) 
}
wtwSamples <- data.frame(do.call(rbind,joiner))
colnames(wtwSamples)[1] <- "WQZ" 

rm(wtwMean,wtw_complete,det, wtwFull)


#------------------------RAIN--------------------------------------------------------------------------------
load("2010_To_2016_Rain_Intensity.RData")
DbIdent_Se <- paste(Rain$Server,Rain$tidIdentifier, sep='')
newRain <- cbind(Rain,DbIdent_Se)

rainfallGrid <- fread('Rainfall_Grid_To_WQZ.txt')

rainData <- merge(newRain,rainfallGrid, by="DbIdent_Se")

rainData$WQZ_REF[rainData$WQZ_REF==" "] <- NA
rainData$WQZ_DESC[rainData$WQZ_DESC==" "] <- NA


rainDrops <- drop_na(rainData,WQZ_REF)
rainDrops <- drop_na(rainData,WQZ_DESC)

rainDrops <- rainDrops[,c("DailyTotal","sumDate","WQZ_REF")]
colnames(rainDrops) <- c("DailyTotal","Date","WQZ")

rainDrops$week <- strftime(as.POSIXct(rainDrops$Date),format="%W")
rainDrops$year <- strftime(as.POSIXct(rainDrops$Date),format="%Y")

rainWeekly <- aggregate(list(RainDailyTotal=rainDrops$DailyTotal),by=list(WQZ=rainDrops$WQZ,Year=rainDrops$year,Week=rainDrops$week),data=rainDrops,FUN=mean)

rm(DbIdent_Se,newRain,rainfallGrid,rainData,rainDrops,Rain)

##------------------Average?max flow SDMA to WQZ------------------------------------------------------------------------------#

#WARNING! - large files (don't load unless necessary)
load("FlowComplete.RData")
load("Full_Missing_Flow.RData")
meters <- read.csv('DMA_TDMA_Flow_Meters.csv')
idarea <- meters[,c("area_id","Tid.Identifier")]
colnames(idarea) <- c("SDMA","Ident")
FlowT <- Flow
FlowT[,1:3] <- NULL
FlowT$Sum_Date <- substr(FlowT$Sum_Date,1,10)
FlowT$Sum_Date <- as.Date(FlowT$Sum_Date,format='%d/%m/%Y')
FlowT <- merge(FlowT,idarea,by="Ident") 
FlowT <- merge(FlowT,lookup,by="SDMA")
FlowT1 <- FlowT[3:8]

Flow_Missing <- Flow_Missing[,2:7]
colnames(Flow_Missing) <- c("Ident","Sum_Date","Daily_Avg","Daily_Max","Daily_Min","Stand_Dev")
FlowMissingT <- merge(Flow_Missing,idarea,by="Ident") 
FlowMissingT <- FlowMissingT[c("Ident","Daily_Avg","Daily_Max","Daily_Min","Stand_Dev","Sum_Date","SDMA")]
FlowMissingT <- merge(FlowMissingT,lookup,by="SDMA")
FlowMissingT1 <- FlowMissingT[,3:8]

flows <- rbind(FlowT1,FlowMissingT1)
flows$Week <- strftime(as.POSIXct(flows$Sum_Date),format="%W")
flows$Year <- strftime(as.POSIXct(flows$Sum_Date),format="%Y")
flows$Stand_Dev <- as.numeric(flows$Stand_Dev)

flowsA <- aggregate(list(Daily_Avg = flows$Daily_Avg,Daily_Max = flows$Daily_Max,Daily_Min=flows$Daily_Min,Stand_Dev=flows$Stand_Dev),by=list(Year=flows$Year,Week=flows$Week,WQZ=flows$WQZ),data=flows,FUN=mean)

flowsA$WQZ[flowsA$WQZ==" "] <- NA
flowsA <- drop_na(flowsA,WQZ)

flowsA$X <- NULL
colnames(flowsA) <- c("Year","Week","WQZ","FlowDailyAvg","FlowDailyMax","FlowDailyMin","FlowStandDev")

rm(FlowT,FlowMissingT,FlowT1,FlowMissingT1,flows,idarea,meters,Flow,Flow_Missing)

##---------------------For population -------------------------------------------------------------------------------#

#WARNING! - large file (don't load unless necessary)
acorn <- fread('Households_2015.txt', select = c("V9"))
population <- data.frame(table(acorn))

rm(acorn)

#---------------------------------CONTACTS-------------------------------------------------------------------------------
contacts <- read.csv('Taste_and_Odour_Contacts_(Updated).csv')
contacts <- contacts[,c("DMA.from.Super.DMA.F","Postal.Code","Create.Date","Task.Total.Count","Task.Code")]

colnames(contacts) <- c("SDMA","Postal.Code","Date","Count","Task.Code") 
contacts <- merge(contacts,lookup,by="SDMA", all.x = TRUE)
contacts <- merge(contacts,postal,by="Postal.Code", all.x=TRUE)
contacts$WQZ[contacts$WQZ==" "] <- NA
contacts$WQZ_2[contacts$WQZ_2==" "] <- NA
contacts$WQZ[!is.na(contacts$WQZ_2)] <- contacts$WQZ_2[!is.na(contacts$WQZ_2)]
contacts$WQZ_2 <- NULL
contacts <- drop_na(contacts,WQZ)

contacts$Date <- as.Date(contacts$Date,format='%d.%m.%Y')
contacts$Week <- strftime(as.POSIXct(contacts$Date),format="%W")
contacts$Year <- strftime(as.POSIXct(contacts$Date),format="%Y")

##Contacts for Time Series Model-----------------------------------------------------------------------

contacts$Count <- as.numeric(as.character(contacts$Count))
timeContactsTask <- aggregate(Count ~ Task.Code + WQZ + Year + Week, data=contacts,FUN=sum)

weekLookup <- wtwSamples[,2:3]
weekLookup$Week <- as.character(weekLookup$Week)
weekLookup$Week <- sapply(as.integer(weekLookup$Week), function(n) sprintf("%02d", n))

WQZs <- unique(lookup$WQZ)
for (i in 1:length(WQZs))
{
  weekLookup[,i+2] <- WQZs[i]
  colnames(weekLookup)[i+2] <- as.character(WQZs[i])
}

weekLookup <- melt(weekLookup,id=c("Year","Week"))
weekLookup <- unique(weekLookup)
weekLookup$value <- NULL
colnames(weekLookup)[3] <- "WQZ"

weekLookup$QTC <- "QTC"
weekLookup$QTT <- "QTT"
weekLookup$QTM <- "QTM"
weekLookup$QCP <- "QCP"
weekLookup$QTP <- "QTP"

weekLookup <- melt(weekLookup,id=c("WQZ","Year","Week"))
weekLookup <- weekLookup[,1:4]
colnames(weekLookup)[4] <- "Task.Code"
weekLookup <- unique(weekLookup)


timeContactsTask <- merge(weekLookup,timeContactsTask, by=c("WQZ","Year","Week","Task.Code"),all.x=TRUE)
timeContactsTask$Count[is.na(timeContactsTask$Count)] <- 0

reshapeCounts <- timeContactsTask
none <- reshapeCounts[which(is.na(reshapeCounts$Task.Code)),]
none$Task.Code <- "Contact"

reshapeCounts <- drop_na(reshapeCounts,Task.Code)
reshapeCounts <- rbind(reshapeCounts,none)
reshapeCounts <- reshape(reshapeCounts, timevar="Task.Code", idvar=c("WQZ","Year","Week"), direction="wide")
reshapeCounts$Count.Contact <- NULL
reshapeCounts[is.na(reshapeCounts)] <- 0
reshapeCounts$TotalCounts <- reshapeCounts$Count.QTC+reshapeCounts$Count.QTT+reshapeCounts$Count.QCP+reshapeCounts$Count.QTM+reshapeCounts$Count.QTP
reshapeCounts$binaryCount <- reshapeCounts$TotalCounts
reshapeCounts$binaryCount[reshapeCounts$binaryCount>0] <- 1
reshapeCounts$binaryCount <- factor(ifelse(reshapeCounts$binaryCount==0, "0", "1"))

timeContactsTask <- reshapeCounts
factors <- c("Year","Week")
timeContactsTask[,factors] <- apply(timeContactsTask[,factors], 2, function(x) as.numeric(x))
rainWeekly[,factors] <- apply(rainWeekly[,factors], 2, function(x) as.numeric(x))
timeContactsTask <- merge(timeContactsTask,wtwSamples, by=c("WQZ","Year","Week"),all.x=TRUE)
timeContactsTask <- merge(timeContactsTask,rainWeekly, by=c("WQZ","Year","Week"),all.x=TRUE)
timeContactsTask <- merge(timeContactsTask, flowsA,  by=c("WQZ","Year","Week"), all.x = TRUE)
timeContactsTask <- merge(timeContactsTask, population, by=c("WQZ"), all.x=TRUE)
timeContactsTask$totalWeek <- NULL

timeContactsTask$ConCount[is.na(timeContactsTask$ConCount)] <- 0
characters <- c("Mean.Chlorine.Free","Mean.Chlorine.Total","Mean.Temperature","Mean.Aluminium","Mean.Colour"
                ,"Mean.Iron","Mean.Manganese","Mean.PH","Mean.Turbidity","Mean.TOC","Mean.DOC","Mean.Hardness")
timeContactsTask[,characters] <- apply(timeContactsTask[,characters], 2, function(x) as.numeric(x))


timeContactsTask$WQZ[timeContactsTask$WQZ==" "] <- NA
timeContactsTask <- drop_na(timeContactsTask,WQZ)


##Correlation and lag creation

library(corrplot)

########all Time Modelling####------------------------------

##Correlation
cory <- timeContactTask[,11:28]
corName <- c("Chlorine.Free","Chlorine.Total","Temperature","Aluminium","Colour",
             "Iron","Manganese","pH","Turbidity","TOC","DOC","Hardness",
             "RainDailyTotal","FlowDailyAvg","FlowDailyMax","FlowDailyMin","FlowStandDev","Population")
colnames(cory) <- corName

corT <- cor(cory, use="complete.obs")
corrplot(corT, method="color", type="upper")

timeContactTask$Mean.Chlorine.Total <- NULL
timeContactTask$Mean.TOC <- NULL
timeContactTask$FlowDailyAvg <- NULL
timeContactTask$FlowDailyMin <- NULL
timeContactTask$FlowStandDev <- NULL

corT <- cor(cbind(timeContactTask[,11:23]), use="complete.obs")


#lag creation 
timeContactTask <- timeContactTask[order(timeContactTask$WQZ,timeContactTask$Year,timeContactTask$Week),]
timeContactTask$binaryLag <- timeContactTask$binaryCount
for (i in 1:31247){
  if(timeContactTask$WQZ[i+1]==timeContactTask$WQZ[i])
  {
    timeContactTask$binaryLag[i] <- timeContactTask$binaryCount[i+1]}
}

write.csv(timeContactTask,'main.csv',row.names=FALSE)


