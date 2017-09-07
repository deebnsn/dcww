library(zoo)
library(data.table)
library(tidyr)
library(corrplot)

##-----------------WTW SAMPLES------------------------------------------------------------------------------------------------------------

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
wtw_complete$month <- strftime(as.POSIXct(wtw_complete$Date),format="%m")

wtwMean <- aggregate(wtw_complete$Result, by=list(Det_Name=wtw_complete$Det_Name,Year=wtw_complete$year,Month=wtw_complete$month),data=wtw_complete,FUN=mean, na.action=na.pass, na.rm=TRUE)
colnames(wtwMean)[4] <- "Mean"
wtwMean <- reshape(wtwMean, idvar = c("Year","Month"), timevar = "Det_Name", direction = "wide")
wtwMean$`Mean.Iron (Unflushed)` <- NULL


wtwMean <- wtwMean[order(wtwMean$Year,wtwMean$Month),]
wtwMean$Mean.DOC <- na.approx(wtwMean$Mean.DOC,rule=2)

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

rainDrops$year <- strftime(as.POSIXct(rainDrops$Date),format="%Y")
rainDrops$month <- strftime(as.POSIXct(rainDrops$Date),format="%m")

rainMonthly <- aggregate(list(RainDailyTotal=rainDrops$DailyTotal),by=list(Year=rainDrops$year,Month=rainDrops$month),data=rainDrops,FUN=mean)
rainMonthly <- rainMonthly[order(rainMonthly$Year,rainMonthly$Month),]

rm(DbIdent_Se,newRain,rainfallGrid,rainData,rainDrops,Rain)

#---------------------------------CONTACTS-------------------------------------------------------------------------------
contacts <- read.csv('Taste_and_Odour_Contacts_(Updated).csv')
contacts <- contacts[,c("Create.Date","Task.Total.Count","Task.Code")]

colnames(contacts) <- c("Date","Count","Task.Code") 

contacts$Date <- as.Date(contacts$Date,format='%d.%m.%Y')
contacts$Month <- strftime(as.POSIXct(contacts$Date),format="%m")
contacts$Year <- strftime(as.POSIXct(contacts$Date),format="%Y")

contacts$Count <- as.numeric(as.character(contacts$Count))
timeContactsTask <- aggregate(Count ~ Task.Code + Year + Month, data=contacts,FUN=sum)

timeContacts <- reshape(timeContactsTask, idvar = c("Year","Month"), timevar = "Task.Code", direction = "wide")
timeContacts <- timeContacts[order(timeContacts$Year,timeContacts$Month),]

for (i in 1:5){
  timeContacts[,i+2][is.na(timeContacts[i+2])] <- 0
}
timeContacts$Count.ALL <- timeContacts$Count.QCP+timeContacts$Count.QTC+timeContacts$Count.QTM+timeContacts$Count.QTM+timeContacts$Count.QTP+timeContacts$Count.QTT


##----Monthly Dataset

monthly <- merge(timeContacts,wtwMean, by=c("Year","Month"))
monthly <- merge(monthly, rainMonthly, by=c("Year","Month"))

##Analysis
colnames(monthly)[9] <- "DailyAvFlow"

View(cor(monthly))

allCor <- data.frame(cor(monthly)[,"Count.ALL"])
chlorCor <- data.frame(cor(monthly)[,"Count.QTC"])


corrplot(cor(monthly))

View(cor(monthly[1:36,]))
View(cor(monthly[49:84,]))


