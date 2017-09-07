library(corrgram)
library(tidyr)
library(arules)
library(effects)
library(penalized)
library(car)
library(MASS)
library(caret)
library(partykit)


########QTC Time Modelling####------------------------------
timeContactTask <- read.csv('main.csv')

timeContactTask$QTCbinary <- timeContactTask$Count.QTC
timeContactTask$QTCbinary[timeContactTask$QTCbinary>0] <- 1
timeContactTask$QTCbinary <- factor(ifelse(timeContactTask$QTCbinary==0, "0", "1"))

#lag creation 
timeContactTask <- timeContactTask[order(timeContactTask$WQZ,timeContactTask$Year,timeContactTask$Week),]
timeContactTask$binaryLag <- timeContactTask$QTCbinary
for (i in 1:31247){
  if(timeContactTask$WQZ[i+1]==timeContactTask$WQZ[i])
  {
    timeContactTask$binaryLag[i] <- timeContactTask$QTCbinary[i+1]}
}

qtc <- timeContactTask[,-5:-8]
qtc <- qtc[,-5:-6]

#missing values

qtc <- drop_na(qtc,Mean.Chlorine.Free)
qtc <- drop_na(qtc,RainDailyTotal)
qtc <- drop_na(qtc,FlowDailyMax)


#Categories


ar <- qtc[,5:17]

options(scipen=999)

pie <- list()
for (i in 1:length(ar)) {
  pie[[i]] <- discretize(ar[[i]],"frequency",categories=6)
}
pie <- data.frame(pie)
colnames(pie) <- colnames(ar)

aContactTasks <- cbind(qtc[,1:4],pie,qtc[,18:19])
rm(pie,ar)


##BINARY MODELS  -LOGIT/TREE

aContactTasks[,5:17] <- apply(aContactTasks[,5:17], 2, function(x) as.character(x))
aContactTasks[is.na(aContactTasks)] <- "Missing" 

week <- discretize(aContactTasks$Week,"frequency",categories=4)
aContactTasks<- cbind(aContactTasks,week)

 for (i in 1:10){
   c <- table(aContactTasks[i+4])
   print(c)
   }


#Lag Model
effects <- aContactTasks
for (i in 1:13){
  effects[,i+4] <- as.factor(effects[,4+i])
}


ll <- glm(binaryLag ~ . -Year -Week -week -WQZ -Count.QTC -binaryLag -QTCbinary,data=effects, family=binomial(logit))

step <- stepAIC(ll, direction="both",maxit = 100)
summary(step)


list( step$coefficient, 
      round( 1 - ( step$deviance / step$null.deviance ), 2 ) )


step <- glm(formula = binaryLag ~ Mean.Chlorine.Free + 
                                     Mean.Temperature + Mean.Aluminium + Mean.Colour + Mean.Iron + 
                                     Mean.Manganese + Mean.PH + Mean.Turbidity + Mean.TOC + Mean.Hardness + 
                                     RainDailyTotal + FlowDailyMax + Population, family = binomial(logit), data = effects)
#AIC: 10529
# 0.14


#LASSO MODEL

bestfit <- optL1(binaryLag ~ . -Year -Week -week -WQZ -Count.QTC -binaryLag -QTCbinary, minlambda1=2, maxlambda1=500, fold=10, standardize=FALSE, data=effects, model="logistic")
bestfit$fullfit
#lambda1 = 4.035148
lasso1 <- penalized(binaryLag ~ . -Year -Week -week -WQZ -Count.QTC -binaryLag -QTCbinary, lambda1= 4.035148, standardize=FALSE, data=effects, model="logistic")
coefficients(lasso1)
#keep all

lasso <- glm(formula = binaryLag ~ . -Year -Week -week -WQZ -Count.QTC -binaryLag -QTCbinary, family = binomial(logit), 
    data = effects)
list( lasso$coefficient, 
      round( 1 - ( lasso$deviance / lasso$null.deviance ), 2 ) )
#AIC: 10529
#0.14

lasso1 <- penalized(binaryLag ~ . -Year -Week -week -WQZ -Count.QTC -binaryLag -QTCbinary, lambda1=2, steps=100, trace=FALSE, standardize=FALSE, data=effects, model="logistic")
plotpath(lasso1, log="x",standardize=FALSE)

nonInter <- step

#interactions


summary(inter<- glm(binaryLag ~ 
                              Mean.Temperature + Mean.Chlorine.Free + Mean.Hardness + Mean.Aluminium + Mean.Colour + 
                              Mean.Manganese + Mean.PH + Mean.Turbidity + RainDailyTotal + Population + Mean.Chlorine.Free:Mean.Hardness, family = binomial(logit), data = effects))

list(inter$coefficient,round( 1 - ( inter$deviance / inter$null.deviance ), 2 ) )

#10561
#0.13


vif(inter)


#INTERPRETATION AND PLOTS
plot(effect("Population",inter),main="Population Effect Plot", ylab="Chlorine Contact Probability (next week)", xlab="Av. Population in Week")
plot(effect("Mean.Chlorine.Free",inter),main="Free Chlorine Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. Free Chlorine in Week")
plot(effect("Mean.Manganese",inter),main="Manganese Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. Manganese in Week")
plot(effect("Mean.Hardness",inter),main="Hardness Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. Hardness in Week")
plot(effect("Mean.Temperature",inter),main="Temperature Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. Temperature in Week")
plot(effect("Mean.Aluminium",inter),main="Aluminium Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. Aluminium in Week")
plot(effect("Mean.Colour",inter),main="Colour Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. Colour in Week")
plot(effect("Mean.Iron",inter),main="Iron Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. Iron in Week")
plot(effect("Mean.PH",inter),main="PH Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. PH in Week")
plot(effect("Mean.Turbidity",inter),main="Turbidity Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. Turbidity in Week")


plot(effect("Mean.Chlorine.Free:Mean.Hardness",inter),main="Hardness*Free Chlorine Effect Plot", ylab="Chlorine Contact Probability (next week)",xlab="Av. Free Chlorine in Week")

plot(effect("week",inter),main="Week Effect Plot", ylab="Chlorine Contact Probability (next week)")

plot(allEffects(step))


##------------------------------------------------------------------
#Trees and Interaction Search


#continuous------------
table(qtc$binaryLag)
# 0     1 
# 28340  2908 

#80% training split
set.seed(4321)
test <- createDataPartition( qtc$QTCbinary, p = 2908/28340, list = FALSE )
data_train <- qtc[ -test, ]
data_test  <- qtc[ test, ]


table(data_train$binaryLag)
# 0     1 
#24341  2500

zeros <- data_train[which(data_train$binaryLag==0),]
notzeros <-data_train[which(data_train$binaryLag!=0),]

mysample <- zeros[sample(1:nrow(zeros), 2908,
                         replace=FALSE),] 

model <- rbind(mysample,notzeros)

tree <- ctree(binaryLag~. -Year -Week -binaryLag -QTCbinary -WQZ -Count.QTC, data=model, maxdepth=4)
plot(tree)
p <- predict(tree,data_test, type="response")
t <- table(data_test$binaryLag,p)
#   p
#    0    1
#0 3000 1036
#1  160  211

(t[1]+t[4])/(t[1]+t[2]+t[3]+t[4])
#accuracy 0.7286136
t[4]/(t[4]+t[3])
#precision 0.1692061


#Stepwise Interactions
summary(test<- glm(binaryLag ~ (Mean.Temperature + 
                     Mean.Aluminium + Mean.Colour + Mean.Iron + Mean.Manganese + 
                     Mean.PH + Mean.Hardness + RainDailyTotal + FlowDailyMax)*Mean.Chlorine.Free + 
                     Population - Mean.Temperature - 
                     Mean.Aluminium - Mean.Colour - Mean.Iron - Mean.Manganese - 
                     Mean.PH - Mean.Hardness - RainDailyTotal - FlowDailyMax - Mean.Chlorine.Free, family = binomial(logit), data = effects))

list(test$coefficient,round( 1 - ( test$deviance / test$null.deviance ), 2 ) )

#AIC: 10890
#0.16


step <- stepAIC(test, direction="both",maxit = 100)
summary(step)
list( step$coefficient, 
      round( 1 - ( step$deviance / step$null.deviance ), 2 ) )


summary(test <- glm(formula = binaryLag ~ Population + Mean.Chlorine.Free:Mean.Iron + 
      Mean.Chlorine.Free:Mean.Manganese + Mean.Chlorine.Free:Mean.Hardness + 
      Mean.Chlorine.Free:FlowDailyMax, family = binomial(logit), 
    data = effects))
#0.14
#10883

vif(inter)







