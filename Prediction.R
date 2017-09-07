library(MASS)
library(caret)
source('unbalanced_functions.R') #Requires r code from:
#https://github.com/ethen8181/machine-learning/tree/master/unbalanced
library(arules)
library(Metrics)
library(randomForest)
library(caret)
library(kernlab)
library(tidyr)


########all Time Modelling####------------------------------
timeContactTask <- read.csv('main.csv')
timeContactTask$X <- NULL
timeContactTask$ConCount <- NULL

#lag creation 
timeContactTask <- timeContactTask[order(timeContactTask$WQZ,timeContactTask$Year,timeContactTask$Week),]
timeContactTask$binaryLag <- timeContactTask$binaryCount
for (i in 1:31247){
  if(timeContactTask$WQZ[i+1]==timeContactTask$WQZ[i])
  {
    timeContactTask$binaryLag[i] <- timeContactTask$binaryCount[i+1]}
}
all <- timeContactTask
all <- all[-4:-8]


##Variable Creation

#MovingTotalCount
all$movingCount <- 0
for (i in 1:31243){
  if(all$WQZ[i+4]==all$WQZ[i])
  {
    all$movingCount[i+4] <- (all$TotalCounts[i]+all$TotalCounts[i+1]+all$TotalCounts[i+2]+all$TotalCounts[i+3])/4
    }
}

#Variance
var <- c("varMean.Chlorine.Free", "varMean.ChlorineTotal", "varMean.Temperature","varMean.Aluminium", "varMean.Colour" , "varMean.Iron" , "varMean.Manganese" , "varMean.PH",
         "varMean.Turbidity", "varMean.TOC", "varMean.DOC", "varMean.Hardness" , "varRainDailyTotal" , "varFlowDailyAvg","varFlowDailyMax",
         "varFlowDailyMin","varFlowStandDev")
for (j in 1:length(var)){
  for (i in 1:31236){
    if(all$WQZ[i+12]==all$WQZ[i])
    {
    all[i+12,j+25] <- var(all[i:(i+11),j+5])
    }
  }
}

colnames(all)[26:42] <- var

#missing values
all <- drop_na(all,Mean.Chlorine.Free)
all <- drop_na(all,RainDailyTotal)
all <- drop_na(all,FlowDailyMax)
all <- drop_na(all,varMean.Hardness)


##-------------------------------------------------------------------------------------------------

#Logistic Regression

#Categories

ar <- all[,6:42]
ar$binaryLag <- NULL

options(scipen=999)

pie <- list()
for (i in 1:length(ar)) {
  pie[[i]] <- discretize(ar[[i]],"frequency",categories=6)
}
pie <- data.frame(pie)
colnames(pie) <- colnames(ar)

aContactTasks <- cbind(all[,1:5],pie,all[,23:24])
rm(pie,ar)
colnames(aContactTasks)[42] <- "pop"

##BINARY MODELS  -LOGIT/TREE

aContactTasks[,5:22] <- apply(aContactTasks[,5:22], 2, function(x) as.character(x))
aContactTasks[,25:41] <- apply(aContactTasks[,25:41], 2, function(x) as.character(x))
aContactTasks[is.na(aContactTasks)] <- "Missing" 

week <- discretize(aContactTasks$Week,"frequency",categories=4)
aContactTasks<- cbind(aContactTasks,week)

for (i in 1:10){
  c <- table(aContactTasks[i+4])
  print(c)
}


#Lag Model

effects <- aContactTasks
for (i in 1:37){
  effects[,i+4] <- as.factor(effects[,4+i])
}
effects$binaryLag <- as.factor(effects$binaryLag)
# 
#0     1 
#23709  7539  

#80% training split


#-------------------------Prediction---------------------

smp_size <- floor(0.8 *nrow(effects))
set.seed(123)
train_ind <- sample(seq_len(nrow(effects)), size = smp_size)
train <- effects[train_ind, ]
test <- effects[-train_ind, ]

summary(m<- glm(formula = binaryLag ~ . -Population, 
                    family = binomial(logit), data = train))

list( m$coefficient, 
      round( 1 - ( m$deviance / m$null.deviance ), 2 ) )


test$predict <- predict(m,test,type="response")

cm_info <- ConfusionMatrixInfo( data = test, predict = "predict", 
                                actual = "binaryLag", cutoff = 0.5 )
cm_info$plot


test$predictClass <- ifelse(test$predict>0.5,"1","0")
confusionMatrix(test$predictClass,test$binaryLag,positive="1")


cost_fp <- 100
cost_fn <- 300
roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
         actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
grid.draw(roc_info$plot)
#$auc
#[1] 0.8111125258


cm_info <- ConfusionMatrixInfo( data = test, predict = "predict", 
                                actual = "binaryLag", cutoff = 0.26 )
cm_info$plot


test$predictClass <- ifelse(test$predict>0.26,"1","0")
confusionMatrix(test$predictClass,test$binaryLag,positive="1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2350  244
# 1  708  583
# 
# Accuracy : 0.755              
# 95% CI : (0.7411, 0.7684)   
# No Information Rate : 0.7871             
# P-Value [Acc > NIR] : 1                  
# 
# Kappa : 0.393              
# Mcnemar's Test P-Value : <0.0000000000000002
# 
# Sensitivity : 0.7050             
# Specificity : 0.7685             
# Pos Pred Value : 0.4516             
# Neg Pred Value : 0.9059             
# Prevalence : 0.2129             
# Detection Rate : 0.1501             
# Detection Prevalence : 0.3323             
# Balanced Accuracy : 0.7367             
# 
# 'Positive' Class : 1                  
# 


rmse(as.numeric(as.character(test$binaryLag)),test$predict)
#0.360981




#------------------------Random Forest-----------------------------------
##Trees 

all1 <- scale(all[,6:23])
all2 <- scale(all[,25:42])
allS <- data.frame(all[,1:5],all1,all2, all[24])
allS$binaryLag <- as.factor(allS$binaryLag)

#Not sampled  - Better One

smp_size <- floor(0.8 *nrow(allS))
set.seed(123)
train_ind <- sample(seq_len(nrow(allS)), size = smp_size)
train <- allS[train_ind, ]
test <-allS[-train_ind, ]

m <- randomForest(binaryLag ~Mean.Chlorine.Free + Mean.Chlorine.Total + Mean.Temperature + 
                    Mean.Aluminium + Mean.Colour + Mean.Iron + Mean.Manganese + 
                    Mean.PH + Mean.Turbidity  + Mean.Hardness + RainDailyTotal + 
                    FlowDailyAvg + FlowDailyMax + FlowDailyMin + Population +
                    movingCount +Week + varMean.Chlorine.Free + varMean.ChlorineTotal + varMean.Temperature + 
                    varMean.Aluminium + varMean.Colour + varMean.Iron + varMean.Manganese + 
                    varMean.PH + varMean.Turbidity + varMean.Hardness + varRainDailyTotal + binaryCount + Year +movingCount, data=train)


test$predict <- predict(m,test,type="prob")[,2]


cm_info <- ConfusionMatrixInfo( data = test, predict = "predict", 
                                actual = "binaryLag", cutoff = .5 )
cm_info$plot

cost_fp <- 100
cost_fn <- 300
roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                     actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
grid.draw(roc_info$plot)

cm_info <- ConfusionMatrixInfo( data = test, predict = "predict", 
                                actual = "binaryLag", cutoff = .264 )
cm_info$plot

test$predictClass <- ifelse(test$predict>0.264,"1","0")
confusionMatrix(test$predictClass,test$binaryLag,positive="1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2299  251
# 1  759  576
# 
# Accuracy : 0.74               
# 95% CI : (0.7259, 0.7538)   
# No Information Rate : 0.7871             
# P-Value [Acc > NIR] : 1                  
# 
# Kappa : 0.3662             
# Mcnemar's Test P-Value : <0.0000000000000002
# 
# Sensitivity : 0.6965             
# Specificity : 0.7518             
# Pos Pred Value : 0.4315             
# Neg Pred Value : 0.9016             
# Prevalence : 0.2129             
# Detection Rate : 0.1483             
# Detection Prevalence : 0.3436             
# Balanced Accuracy : 0.7241             
# 
# 'Positive' Class : 1                  

#0.7893546 auc


##TUNE ABOVE MODEL WITH CARET
ctrl <- trainControl(method="repeatedcv", number=10, repeats=10)
grid_rf <- expand.grid(.mtry=c(2,4,8,16))
set.seed(300)


#mtry=2 Kappa : 0.3485,Sensitivity : 0.4214,Accuracy : 0.7333


#tuning took place by changing mtry, but the best remained the default sqrt(p)
rmse(as.numeric(as.character(test$binaryLag)),test$predict)
#.3689549


##---------Support Vector Machines-----------------------------------------------

all1 <- scale(all[,6:23])
all2 <- scale(all[,25:42])
allS <- data.frame(all[,1:5],all1,all2, all[24])
allS$binaryLag <- as.factor(allS$binaryLag)


smp_size <- floor(0.8 *nrow(allS))
set.seed(123)
train_ind <- sample(seq_len(nrow(allS)), size = smp_size)
train <- allS[train_ind, ]
test <-allS[-train_ind, ]

m <- ksvm(binaryLag ~Mean.Chlorine.Free + Mean.Chlorine.Total + Mean.Temperature + 
            Mean.Aluminium + Mean.Colour + Mean.Iron + Mean.Manganese + 
            Mean.PH + Mean.Turbidity  + Mean.Hardness + RainDailyTotal + 
            FlowDailyAvg + FlowDailyMax + FlowDailyMin + Population +
            movingCount +Week + varMean.Chlorine.Free + varMean.ChlorineTotal + varMean.Temperature + 
            varMean.Aluminium + varMean.Colour + varMean.Iron + varMean.Manganese + 
            varMean.PH + varMean.Turbidity + varMean.Hardness + varRainDailyTotal + binaryCount + Year +movingCount, data=train, kernel="rbfdot", prob.model=TRUE)


test$predict <- predict(m,test,type="prob")[,2]

cm_info <- ConfusionMatrixInfo( data = test, predict = "predict", 
                                actual = "binaryLag", cutoff = .5 )
cm_info$plot

cost_fp <- 100
cost_fn <- 300
ROCInfo( data = cm_info$data, predict = "predict", 
                     actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
#$auc
#[1] 0.7266492


cm_info <- ConfusionMatrixInfo( data = test, predict = "predict", 
                                actual = "binaryLag", cutoff = 0.176728 )
cm_info$plot

test$predictClass <- ifelse(test$predict> 0.176728,"1","0")
confusionMatrix(test$predictClass,test$binaryLag,positive="1")

#Confusion Matrix and Statistics

# Reference
# Prediction    0    1
# 0 2462  320
# 1  596  507
# 
# Accuracy : 0.7642             
# 95% CI : (0.7505, 0.7775)   
# No Information Rate : 0.7871             
# P-Value [Acc > NIR] : 0.9997             
# 
# Kappa : 0.3728             
# Mcnemar's Test P-Value : <0.0000000000000002
# 
# Sensitivity : 0.6131             
# Specificity : 0.8051             
# Pos Pred Value : 0.4597             
# Neg Pred Value : 0.8850             
# Prevalence : 0.2129             
# Detection Rate : 0.1305             
# Detection Prevalence : 0.2839             
# Balanced Accuracy : 0.7091             
# 
# 'Positive' Class : 1                  





