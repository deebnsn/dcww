library(corrplot)
library(tidyr)
library(arules)
library(MASS)
library(penalized)
library(car)
library(MASS)
library(caret)
library(partykit)
library(effects)

########all Time Modelling####------------------------------
all <- read.csv('main.csv')
all <- all[-4:-8]

#missing values
all <- drop_na(all,Mean.Chlorine.Free)
all <- drop_na(all,RainDailyTotal)
all <- drop_na(all,FlowDailyMax)


#Categories

ar <- all[,6:18]

options(scipen=999)

pie <- list()
for (i in 1:length(ar)) {
  pie[[i]] <- discretize(ar[[i]],"frequency",categories=6)
}
pie <- data.frame(pie)
colnames(pie) <- colnames(ar)

aContactTasks <- cbind(all[,1:5],pie,all[19])
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


ll <- glm(binaryLag ~ . -Year -Week -week -WQZ -TotalCounts -binaryLag -binaryCount,data=effects, family=binomial(logit))

step <- stepAIC(ll, direction="both",maxit = 100)
summary(step)


list( step$coefficient, 
      round( 1 - ( step$deviance / step$null.deviance ), 2 ) )

step <- glm(formula = binaryLag ~ Mean.Chlorine.Free + Mean.Aluminium + 
           Mean.Colour + Mean.Manganese + Mean.PH + Mean.Turbidity + 
           Mean.DOC + Mean.Hardness + RainDailyTotal + FlowDailyMax + 
           Population, family = binomial(logit), data = effects)

#17199
#0.19


#LASSO MODEL
bestfit <- optL1(binaryLag ~ . -Year -Week -week -WQZ -binaryLag -binaryCount, minlambda1=2, maxlambda1=500, fold=10, standardize=FALSE, data=effects, model="logistic")
bestfit$fullfit
#lambda1 = 4.035148
lasso1 <- penalized(binaryLag ~ . -Year -Week -week -WQZ -binaryLag -binaryCount, lambda1= 5.133036, standardize=FALSE, data=effects, model="logistic")
coefficients(lasso1)
#keep all

lasso <- glm(formula = binaryLag ~ Mean.Chlorine.Free + Mean.Aluminium + 
              Mean.Colour + Mean.Iron + Mean.Manganese + Mean.PH + Mean.Turbidity + 
              Mean.TOC + Mean.Hardness + RainDailyTotal + FlowDailyMax + 
              Population, family = binomial(logit), data = effects)
# 17201
#0.19

lasso1 <- penalized(binaryLag ~ Mean.Chlorine.Free + Mean.Aluminium + 
                      Mean.Colour + Mean.Manganese + Mean.PH + Mean.Turbidity + 
                      Mean.TOC + Mean.Hardness + RainDailyTotal + FlowDailyMax + 
                      Population, lambda1=2, steps=100, trace=FALSE, standardize=FALSE, data=effects, model="logistic")
plotpath(lasso1, log="x",standardize=FALSE)


#interactions (search below)

effects$contPop <- scale(all$Population)
summary(inter<- glm(binaryLag ~ Mean.Chlorine.Free + 
                      Mean.Colour + Mean.Manganese + Mean.PH + Population + Mean.Hardness + RainDailyTotal +Mean.Chlorine.Free:Mean.PH, family = binomial(logit), data = effects))

list(inter$coefficient,round( 1 - ( inter$deviance / inter$null.deviance ), 2 ) )
plot(allEffects(inter))

#17196
#0.19

vif(inter)

effect("Mean.Turbidity:Population",inter)


#INTERPRETATION AND PLOTS
plot(effect("Population",inter),main="Population Effect Plot", ylab="Contact Probability (next week)", xlab="Av. Population in Week")
plot(effect("Mean.Manganese",inter),main="Manganese Effect Plot", ylab="Contact Probability (next week)",xlab="Av. Manganese in Week")
plot(effect("Mean.Hardness",inter),main="Hardness Effect Plot", ylab="Contact Probability (next week)",xlab="Av. Hardness in Week")
plot(effect("RainDailyTotal",inter),main="Rainfall Effect Plot", ylab="Contact Probability (next week)",xlab="Av. Rainfall (daily total) in Week")

plot(effect("Mean.Colour",inter),main="Colour Effect Plot", ylab="Contact Probability (next week)",xlab="Av. Colour in Week")


c <- plot(effect("Mean.Chlorine.Free:Mean.PH",inter),main="Chlorine Effect Plot (when PH is low)", ylab="Contact Probability (next week)",xlab="Av. Free Chlorine in Week")

plot(effect("Mean.Chlorine.Free",inter),main="Free Chlorine Effect Plot", ylab="Contact Probability (next week)",xlab="Av. Free Chlorine in Week")
plot(effect("Mean.Turbidity",inter),main="Turbidity Effect Plot", ylab="Contact Probability (next week)",xlab="Av. Turbidity in Week")




##------------------------------------------------------------------
#Trees and Interaction Search


#continuous------------
table(all$binaryLag)
#0     1 
#15708  4361 

#80% training split

set.seed(4321)
test <- createDataPartition( all$binaryCount, p = 4361/15708, list = FALSE )
data_train <- all[ -test, ]
data_test  <- all[ test, ]


table(data_train$binaryLag)
#0     1 
#11301  3196

zeros <- data_train[which(data_train$binaryLag==0),]
notzeros <-data_train[which(data_train$binaryLag!=0),]

mysample <- zeros[sample(1:nrow(zeros), 3196,
                         replace=FALSE),] 

model <- rbind(mysample,notzeros)

tree <- ctree(binaryLag~. -Population -Year -Week -binaryLag -binaryCount -WQZ -TotalCounts, data=model, maxdepth=4)
plot(tree)
p <- predict(tree,data_test, type="response")
t <- table(data_test$binaryLag,p)

(t[1]+t[4])/(t[1]+t[2]+t[3]+t[4])
#accuracy 
t[4]/(t[4]+t[3])
#precision 


#Stepwise Interactions
summary(test<- glm(binaryLag ~ (Mean.Temperature + 
                                  Mean.Aluminium + Mean.Colour + Mean.Iron + Mean.Manganese + 
                                  Mean.PH + Mean.Hardness)*(Mean.Temperature + 
         Mean.Aluminium + Mean.Colour + Mean.Iron + Mean.Manganese + Mean.PH + Mean.Hardness) + 
                     Population - Mean.Temperature - 
                     Mean.Aluminium - Mean.Colour - Mean.Iron - Mean.Manganese - 
                     Mean.PH - Mean.Hardness - RainDailyTotal - FlowDailyMax - Mean.Chlorine.Free, family = binomial(logit), data = effects))

list(test$coefficient,round( 1 - ( test$deviance / test$null.deviance ), 2 ) )

#AIC: 17291
#0.2

step <- stepAIC(test, direction="both",maxit = 100)
summary(step)
list( step$coefficient, 
      round( 1 - ( step$deviance / step$null.deviance ), 2 ) )

summary(test <- glm(formula = binaryLag ~ Population + Mean.Chlorine.Free:Mean.Iron + 
                      Mean.Chlorine.Free:Mean.Manganese + Mean.Chlorine.Free:Mean.PH + 
                      Mean.Chlorine.Free:Mean.Hardness + Mean.Chlorine.Free:FlowDailyMax, 
                    family = binomial(logit), data = effects)
#0.19
#17291


vif(inter)



summary(test <- glm(formula = binaryLag ~ Population + Mean.Chlorine.Free:Mean.Iron + 
                      Mean.Chlorine.Free:Mean.Manganese + Mean.Chlorine.Free:Mean.PH + 
                      Mean.Chlorine.Free:Mean.Hardness + Mean.Chlorine.Free:FlowDailyMax +Mean.Temperature:Mean.Hardness
                    + Mean.Temperature:Mean.PH, family = binomial(logit), data = effects))
        #0.19
        #17239
        
        library(car)
        
[7.38,7.51)

summary(inter<- glm(binaryLag ~ Mean.Chlorine.Free + Mean.Aluminium + 
                              Mean.Colour + Mean.Iron + Mean.Manganese + Mean.PH + Mean.Turbidity + 
                              Mean.TOC + Mean.Hardness + RainDailyTotal +  
                              Population   + Mean.Chlorine.Free:Mean.PH, family = binomial(logit), data = effects))
        
list(inter$coefficient,round( 1 - ( inter$deviance / inter$null.deviance ), 2 ) )
vif(inter)      
        #17196
        #0.19
        
c <- plot(effect("Mean.Chlorine.Free:Mean.PH",inter))



#------------------------------------------------------------------------------------------------------------------------------------

#Prediction Model


table(effects$binaryLag)
# 
#0     1 
#15708  4361 


source('unbalanced_functions.R')  #Requires r code from:
#https://github.com/ethen8181/machine-learning/tree/master/unbalanced


smp_size <- floor(0.8 *nrow(effects))
set.seed(123)
train_ind <- sample(seq_len(nrow(effects)), size = smp_size)

train <- effects[train_ind, ]
test <- effects[-train_ind, ]


summary(m<- glm(formula = binaryLag ~ Mean.Chlorine.Free + Mean.Temperature + 
                      Mean.Aluminium + Mean.Colour + Mean.Iron + Mean.Manganese + 
                      Mean.PH + Mean.Turbidity + Mean.Hardness + RainDailyTotal + 
                      FlowDailyMax + Population + week + Year + binaryCount + Mean.Chlorine.Free:Mean.PH, 
                    family = binomial(logit), data = train))

#AIC: 13633

list( m$coefficient, 
      round( 1 - ( m$deviance / m$null.deviance ), 2 ) )
# 0.2


test$predict <- predict(m,test,type="response")

cm_info <- ConfusionMatrixInfo( data = test, predict = "predict", 
                                actual = "binaryLag", cutoff = 0.2974001 )
cm_info$plot


cost_fp <- 100
cost_fn <- 300
r <- ROCInfo( data = cm_info$data, predict = "predict", 
         actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
# $cutoff
# [1] 0.2974001
# 
# $auc
# [1] 0.7864351


test$predictClass <- ifelse(test$predict>0.26,"1","0")
confusionMatrix(test$predictClass,test$binaryLag,positive="1")
#  
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2413  298
# 1  739  564
# 
# Accuracy : 0.7417             
# 95% CI : (0.7278, 0.7551)   
# No Information Rate : 0.7853             
# P-Value [Acc > NIR] : 1                  
# 
# Kappa : 0.354              
# Mcnemar's Test P-Value : <0.0000000000000002
# 
# Sensitivity : 0.6543             
# Specificity : 0.7655             
# Pos Pred Value : 0.4328             
# Neg Pred Value : 0.8901             
# Prevalence : 0.2147             
# Detection Rate : 0.1405             
# Detection Prevalence : 0.3246             
# Balanced Accuracy : 0.7099             
# 
# 'Positive' Class : 1   
# 
# 


#0.26 Cutoff

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2253  250
# 1  899  612
# 
# Accuracy : 0.7138             
# 95% CI : (0.6995, 0.7277)   
# No Information Rate : 0.7853             
# P-Value [Acc > NIR] : 1                  
# 
# Kappa : 0.3335             
# Mcnemar's Test P-Value : <0.0000000000000002
# 
# Sensitivity : 0.7100             
# Specificity : 0.7148             
# Pos Pred Value : 0.4050             
# Neg Pred Value : 0.9001             
# Prevalence : 0.2147             
# Detection Rate : 0.1525             
# Detection Prevalence : 0.3764             
# Balanced Accuracy : 0.7124             
# 
# 'Positive' Class : 1                  
















