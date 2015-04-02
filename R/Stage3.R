##### File 3: Perform SOme Modeling #####

##Setup some functions and to do some single variable analysis##

#Function to make predictions on Cat Values
mkPredC <- function(outCol,varCol,appCol) {
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <-table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}

#Here i believe we start to loop through catVariables and build our prediction code to pass into the Prediction Function for catVars
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  train[,pi] <- mkPredC(train[,outcome],train[,v],train[,v])
  cal[,pi] <- mkPredC(train[,outcome],train[,v],cal[,v])
  test[,pi] <- mkPredC(train[,outcome],train[,v],test[,v])
}

#Building the function to calculate AUC
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}

#Code to loop through catVars and push prediction statements per row into the calcAUC function.  This returns a percent, that if higher then
#0.8 we print out the info to screen:  Variable Name, TrainAUC, and calibrationAUC.  Right before asigning the calibration numbers it throws
#number to the same calcAUC function based on the calibration set and thats whats printed with calibrationAUC.  
#It seems to be better to use variables with a higher number for calibrationAUC as it better adjsuts for variables with many levels.
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(train[,pi],train[,outcome])
  if(aucTrain>=0.6) {
    aucCal <- calcAUC(cal[,pi],cal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                  pi,aucTrain,aucCal))
  }
}

#This function is is designed to be fed with variables/info from numericaVars.  Its going to cut them up into categories as pass to the
#makePredC function
mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(varCol,probs=seq(0, 1, 0.1),na.rm=T)))
  varC <- cut(varCol,cuts)
  appC <- cut(appCol,cuts)
  mkPredC(outCol,varC,appC)
}

#Much like the for loop above for catVars, this loops through the numerica vars preparing the data and passing it into the mkPredN function above
for(v in numericVars) {
  pi <- paste('pred',v,sep='')
  train[,pi] <- mkPredN(train[,outcome],train[,v],train[,v])
  test[,pi] <- mkPredN(train[,outcome],train[,v],test[,v])
  cal[,pi] <- mkPredN(train[,outcome],train[,v],cal[,v])
  aucTrain <- calcAUC(train[,pi],train[,outcome])
  if(aucTrain>=.55) {
    aucCal <- calcAUC(cal[,pi],cal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                  pi,aucTrain,aucCal))
  }
}



## This section is a custom variable selection loop where each variable is scroed according to an AIC / inspired score.  Variables are scored
#with a bonus proportional to the scaled log likelihood of the traning data minus a pentaly proportial to the complexity of the variable
#(which is 2^entropy)   Notice they are using performance on the calibration set to pick variables.
logLikelyhood <- function(outCol,predCol) {
  sum(ifelse(outCol==pos,log(predCol),log(1-predCol)))
}

entropy <- function(x) { 
  xpos <- x[x>0 & !is.na(x)]
  scaled <- xpos/sum(xpos)
  sum(-scaled*log(scaled,2))
}


nPos <- sum(train[,outcome]==pos)
pPos <- nPos/length(train[,outcome])
eps <- 1.0e-5
for(v in c(catVars,numericVars)) {
  pi <- paste('pred',v,sep='')
  li <- paste('lift',v,sep='')
  train[,li] <- log((train[,pi]+eps)/(pPos+eps))
  test[,li] <- log((test[,pi]+eps)/(pPos+eps))
  cal[,li] <- log((cal[,pi]+eps)/(pPos+eps))
}

selVars <- c()
minStep <- 5

baseRateTrain <- logLikelyhood(train[,outcome],sum(train[,outcome]==pos)/length(train[,outcome]))
baseRateCheck <- logLikelyhood(cal[,outcome],sum(cal[,outcome]==pos)/length(cal[,outcome]))

for(v in catVars) {
  pi <- paste('pred',v,sep='')
  li <- paste('lift',v,sep='')
  liTrain <- 2*(logLikelyhood(train[,outcome],train[,pi])- baseRateTrain - 2^entropy(table(train[,pi],useNA='ifany')))
  liCheck <- 2*(logLikelyhood(train[,outcome],train[,pi]) - baseRateCheck - 2^entropy(table(train[,pi],useNA='ifany')))
  if((liTrain>=minStep)&(liCheck>minStep)) {
    print(sprintf("%s, trainAIC: %g calibrationAIC: %g",
                  pi,liTrain,liCheck))
    selVars <- c(selVars,li)
  }
}

print(selVars)


for(v in numericVars) {
  pi <- paste('pred',v,sep='')
  li <- paste('lift',v,sep='')
  liTrain <- 2*(logLikelyhood(train[,outcome],train[,pi])-baseRateTrain-1)
  liCheck <- 2*(logLikelyhood(train[,outcome],train[,pi])-baseRateCheck-1)
  if((liTrain>=minStep) && (liCheck>=minStep)) {
    print(sprintf("%s, trainAIC: %g calibrationAIC: %g",
                  pi,liTrain,liCheck))
    selVars <- c(selVars,li)
  }
}

print(selVars)


f <- paste(outcome,' ~ ',paste(selVars,collapse=' + '),sep='')
model <- glm(as.formula(f),data=train,family=binomial(link='logit'))
print(summary(model))
train$pred <- predict(model,type='response',newdata=train)
cal$pred <- predict(model,type='response',newdata=cal)
test$pred <- predict(model,type='response',newdata=test)

perfTrain <- performance(prediction(train$pred,train[,outcome]>0),'auc')
perfCal <- performance(prediction(cal$pred,cal[,outcome]>0),'auc')
perfTest <- performance(prediction(test$pred,test[,outcome]>0),'auc')
print(paste('train',as.numeric(perfTrain@y.values)))
print(paste('cal',as.numeric(perfCal@y.values)))
print(paste('test',as.numeric(perfTest@y.values)))




##### Ricks Code #####
#Decision tree
dTree <- rpart(WonLost ~ EstRateGroups + OppDurationGroup + EstHoursGroups + ITRGroups, data = train, method = "class")


# Run Random Forrest model:
set.seed(300)
rfTree <- randomForest(WonLost ~ EstRateGroups + OppDurationGroup + EstHoursGroups + ITRGroups + ServiceOffering,data = train, importance=TRUE, ntree=2000)

# The 'importance()' function returns a matrix of importance measures from the RF tree. Larger values = more importance
varImp <- importance(rfTree)
varImp
# Show mean decrease accuracy(what did the RF tree find as most important), and mean decrese Gini
varImpPlot(rfTree)

# Apply a Prediction column to Test and Train for this RF Tree
rfPredict <- predict(rfTree, test)
rfPredict2 <- predict(rfTree, train)
opptest$RFPrediction <- rfPredict
opptrain$RFPrediction <- rfPredict2

# Check confusion matrix, and build precision/recall vector 
RFConfusion <- table(test$WonLost, test$RFPrediction)
RFConfusion
#Calculate Precision
RFprecision <- RFConfusion[2,2]/sum(RFConfusion[2,])
RFprecision
#Calculate Recall
RFrecall <- RFConfusion[2,2]/sum(RFConfusion[,2])
RFrecall
#Calculate Enrichment
RFenrich <- RFprecision/mean(as.numeric(test$WonLost))
RFenrich

## The RF decision tree did a good job of predicting Win/Loss in the test set. ##

##### Executing a Logistic Regression to the Train data set #####

lrModel <- glm(WonLost ~ EstRateGroups + OppDurationGroup + EstHoursGroups + ITRGroups + ServiceOffering + AE + OppoPctFulltime + CSM_Name + Forecast,data = train, family=binomial(link="logit"))

# Applying the logistic regression model. Create prediction column in both Train and Test
train$LogRePred <- predict(lrModel, newdata=train, type="response")
test$LogRePred <- predict(lrModel, newdata=test, type="response")



# Create confusions matrix with LogRe predictions (On TEST data set):
LRConfusion <- (with(test, table(y = WonLost, LogRePred >= 0.5)))
LRConfusion
#Calculate Precision
LRprecision <- LRConfusion[2,2]/sum(LRConfusion[2,])
LRprecision
#Calcualte Recall
LRrecall <- LRConfusion[2,2]/sum(LRConfusion[,2])
LRrecall
#Calculate Enrichment
LRenrich <- LRprecision/mean(as.numeric(test$WonLost))
LRenrich

# Analyze Coefficients for this model:
coefficients(lrModel)

# the AIC criterion. The log likelihood adjusted for the number of coefficients
loglikelihood <- function(y, py) {
  sum(y * log(py) + (1-y) * log(1-py))
}

LRaic <- 2*(length(lrModel$coefficients) - loglikelihood(as.numeric(train$WonLost), train$LogRePred))
LRaic
## !! The AIC is usually used to decide which, and how many, input variables to use in the model.
# If you train many different models with different sets of variables, you can consider the model
# with the lowest AIC to be the best fit. !!

LogReComparo <- data.frame(WonLost = test$WonLost, LogRegPredProb = test$LogRePred, RFPred = test$RFPrediction,
                           LogRegPred = 0) 

LogReComparo$LogRegPred[LogReComparo$LogRegPredProb >= 0.80] <- 1
#LogReComparo <- LogReComparo[, ] %>%
# arrange(opptest.WonLost)

table(LogReComparo$test.WonLost)
prop.table(table(LogReComparo$test.WonLost))

RFConfusion <- (with(LogReComparo, table(y = WonLost, RFPred)))
RFConfusion
#Calculate Accuracy
RFAccuracy <- (RFConfusion[1,1] + RFConfusion[2,2]) / sum(RFConfusion[,])
RFAccuracy
#Calculate Precision
RFPrecision <- RFConfusion[2,2]/sum(RFConfusion[2,])
RFPrecision


LogConfusion <- (with(LogReComparo, table(y = WonLost, LogRegPred)))
LogConfusion
#Calculate Accuracy
LogAccuracy <- (LogConfusion[1,1] + LogConfusion[2,2]) / sum(LogConfusion[,])
LogAccuracy
#Calcualte Precision
Logprecision <- LogConfusion[2,2]/sum(LogConfusion[2,])
Logprecision

########### END Basic Analysis : The models above predicted VERY well for Won/Lost response variable. The Logistic
###########                       Regression model above is 87% accurate in predicion.          #####################

## write.csv(opptrain, file = "oppTrainData.csv")
