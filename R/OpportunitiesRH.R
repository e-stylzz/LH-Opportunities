
##### Load and Setup Data Frame(s) #####
oppdata <- read.csv("opportunitydata.csv", na.strings=c("","NULL"))  # Don't forget to add this argument for the NA's
str(oppdata)

library('rpart')
library('rattle')
library('rpart.plot')
library('RColorBrewer')
library('randomForest')
library('party')
library('ggplot2')
library('lubridate')
library('ROCR')
library('dplyr')

# Remove 'In Progress' opportunities from primary data frame:
oppdata <- data.frame(oppdata[,]) %>%
  filter( oppdata$Status != "In Progress")

# Set dates to 'Date' type(called "class" in R)
oppdata$AvailabilityDate <- as.Date(oppdata$AvailabilityDate, format = "%m/%d/%Y")
oppdata$OppoEndDate <- as.Date(oppdata$OppoEndDate, format = "%m/%d/%Y")
oppdata$OppoStartDate <- as.Date(oppdata$OppoStartDate, format = "%m/%d/%Y")
oppdata$TargetCloseDate <- as.Date(oppdata$TargetCloseDate, format = "%m/%d/%Y")
oppdata$CreatedDate <- as.Date(oppdata$CreatedDate, format = "%m/%d/%Y")

# Create groups for the oppo Duration:
oppdata$OppDurationGroup <- '0-30'
oppdata$OppDurationGroup[oppdata$OppoDuration >= 30 & oppdata$OppoDuration < 90] <- '30-90'
oppdata$OppDurationGroup[oppdata$OppoDuration >= 90 & oppdata$OppoDuration < 180] <- '90-180'
oppdata$OppDurationGroup[oppdata$OppoDuration >= 180] <- 'Over 180'

oppdata$OppDurationGroup <- as.factor(oppdata$OppDurationGroup)

# Create 'Estimated Hours' group:
oppdata$EstHoursGroups <- '0-40'
oppdata$EstHoursGroups[oppdata$EstHours >= 40 & oppdata$EstHours < 80] <- '40-80'
oppdata$EstHoursGroups[oppdata$EstHours >= 80 & oppdata$EstHours < 120] <- '80-120'
oppdata$EstHoursGroups[oppdata$EstHours >= 120 & oppdata$EstHours < 200] <- '120-200'
oppdata$EstHoursGroups[oppdata$EstHours >= 200 & oppdata$EstHours < 400] <- '200-400'
oppdata$EstHoursGroups[oppdata$EstHours >= 500 & oppdata$EstHours < 1000] <- '500 - 1000'
oppdata$EstHoursGroups[oppdata$EstHours >= 1000] <- 'over 1000'

oppdata$EstHoursGroups <- as.factor(oppdata$EstHoursGroups)

# Create Estimated Rate Groups:
oppdata$EstRateGroups <- '0-100'
oppdata$EstRateGroups[oppdata$EstRate >= 100 & oppdata$EstRate < 150] <- '100-150'
oppdata$EstRateGroups[oppdata$EstRate >= 150 & oppdata$EstRate < 175] <- '150-175'
oppdata$EstRateGroups[oppdata$EstRate >= 175] <- 'over 175'

oppdata$EstRateGroups <- as.factor(oppdata$EstRateGroups)

# Create ITR Groups:
oppdata$ITRGroups <- '0-100'
oppdata$ITRGroups[oppdata$ITR >= 100 & oppdata$ITR < 125] <- '100-125'
oppdata$ITRGroups[oppdata$ITR >= 125 & oppdata$ITR < 150] <- '125-150'
oppdata$ITRGroups[oppdata$ITR >= 150 & oppdata$ITR < 175] <- '150-175'
oppdata$ITRGroups[oppdata$ITR >= 175] <- 'over 175'

oppdata$ITRGroups <- as.factor(oppdata$ITRGroups)

# Create a 'Won/Lost' Binary variable. Options are Status = 'Lost' vs everything else:
oppdata$WonLost <- '1'
oppdata$WonLost[oppdata$Status == 'Lost'] <- '0'

oppdata$WonLost <- as.factor(oppdata$WonLost)


##### END Data prep #####

##### Analyze initial main data frame - Basic ##### 
table(oppdata$WonLost)
prop.table(table(oppdata$WonLost))


table(oppdata$WonLost, oppdata$OppDurationGroup)  # Very Interesting **
prop.table(table(oppdata$WonLost, oppdata$OppDurationGroup))

table(oppdata$WonLost, oppdata$EstHoursGroups)  # Very Interesting **
prop.table(table(oppdata$WonLost, oppdata$EstHoursGroups))

table(oppdata$WonLost, oppdata$EstRateGroups)  # Somewhat Interesting **
prop.table(table(oppdata$WonLost, oppdata$EstRateGroups),1)

# A quick plot of those groups:
ggplot(oppdata, aes(x= OppDurationGroup, fill= WonLost)) +
  facet_wrap(~ServiceOffering + EstRateGroups) +
  geom_histogram(binwidth = 5) +
  xlab("Duration Group") +
  ylab("Total Count")


##### Create Train and Test Data frames:
oppdata$RandGroup <- runif(dim(oppdata) [1])
opptest <- subset(oppdata, oppdata$RandGroup <= 0.3)
opptrain <- subset(oppdata, oppdata$RandGroup > 0.3)




###################### Quick Models - Taking initial guesses at the dependent variables ##############################

#####   Decision tree #####
dTree <- rpart(WonLost ~ EstRateGroups + OppDurationGroup + EstHoursGroups + ITRGroups, data = opptrain, method = "class")
fancyRpartPlot(dTree)

# Run Random Forrest model:
set.seed(300)
rfTree <- randomForest(WonLost ~ EstRateGroups + OppDurationGroup + EstHoursGroups + ITRGroups + ServiceOffering, 
                       data = opptrain, importance=TRUE, ntree=2000)

# The 'importance()' function returns a matrix of importance measures from the RF tree. Larger values = more importance
varImp <- importance(rfTree)
varImp
# Show mean decrease accuracy(what did the RF tree find as most important), and mean decrese Gini
varImpPlot(rfTree)

# Apply a Prediction column to Test and Train for this RF Tree
rfPredict <- predict(rfTree, opptest)
rfPredict <- predict(rfTree, opptrain)
opptest$RFPrediction <- rfPredict
opptrain$RFPrediction <- rfPredict

# Check confusion matrix, and build precision/recall vector 
RFConfusion <- table(opptest$WonLost, opptest$RFPrediction)
RFConfusion
RFprecision <- RFConfusion[2,2]/sum(RFConfusion[2,])
RFprecision

RFrecall <- RFConfusion[2,2]/sum(RFConfusion[,2])
RFrecall

RFenrich <- RFprecision/mean(as.numeric(opptest$WonLost))
RFenrich

## The RF decision tree did a good job of predicting Win/Loss in the test set. ##

##### Executing a Logistic Regression to the Train data set #####

lrModel <- glm(WonLost ~ EstRateGroups + OppDurationGroup + EstHoursGroups + ITRGroups + ServiceOffering, 
               data = opptrain, family=binomial(link="logit"))

# Applying the logistic regression model. Create prediction column in both Train and Test
opptrain$LogRePred <- predict(lrModel, newdata=opptrain, type="response")
opptest$LogRePred <- predict(lrModel, newdata=opptest, type="response")

# Plotting distribution of prediction score grouped by known outcome. The False line should be focused on the Left, and the 
# True density should be focused on the Right for a good distribution.
ggplot(opptrain, aes(x=LogRePred, color=WonLost, linetype=WonLost)) +
  geom_density()

## This plot is pretty good

# Create confusions matrix with LogRe predictions (On TEST data set):
LRConfusion <- (with(opptest, table(y = WonLost, LogRePred >= 0.5)))
LRConfusion
LRprecision <- LRConfusion[2,2]/sum(LRConfusion[2,])
LRprecision

LRrecall <- LRConfusion[2,2]/sum(LRConfusion[,2])
LRrecall

LRenrich <- LRprecision/mean(as.numeric(opptest$WonLost))
LRenrich

# Analyze Coefficients for this model:
coefficients(lrModel)

# the AIC criterion. The log likelihood adjusted for the number of coefficients
loglikelihood <- function(y, py) {
  sum(y * log(py) + (1-y) * log(1-py))
}
LRaic <- 2*(length(lrModel$coefficients) - loglikelihood(as.numeric(opptrain$WonLost), opptrain$LogRePred))
LRaic
## !! The AIC is usually used to decide which, and how many, input variables to use in the model.
# If you train many different models with different sets of variables, you can consider the model
# with the lowest AIC to be the best fit. !!

LogReComparo <- data.frame(WonLost = opptest$WonLost, LogRegPredProb = opptest$LogRePred, RFPred = opptest$RFPrediction,
                           LogRegPred = 0) 

LogReComparo$LogRegPred[LogReComparo$LogRegPredProb >= 0.80] <- 1
#LogReComparo <- LogReComparo[, ] %>%
 # arrange(opptest.WonLost)

table(LogReComparo$opptest.WonLost)
prop.table(table(LogReComparo$opptest.WonLost))

RFConfusion <- (with(LogReComparo, table(y = WonLost, RFPred)))
RFConfusion
RFAccuracy <- (RFConfusion[1,1] + RFConfusion[2,2]) / sum(RFConfusion[,])
RFAccuracy
RFPrecision <- RFConfusion[2,2]/sum(RFConfusion[2,])
RFPrecision


LogConfusion <- (with(LogReComparo, table(y = WonLost, LogRegPred)))
LogConfusion
LogAccuracy <- (LogConfusion[1,1] + LogConfusion[2,2]) / sum(LogConfusion[,])
LogAccuracy
Logprecision <- LogConfusion[2,2]/sum(LogConfusion[2,])
Logprecision

########### END Basic Analysis : The models above predicted VERY well for Won/Lost response variable. The Logistic
###########                       Regression model above is 87% accurate in predicion.          #####################

## write.csv(opptrain, file = "oppTrainData.csv")







                         