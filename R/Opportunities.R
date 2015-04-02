
setwd("~/GitHub/Opportunities")
##### Load and Setup Data Frame(s) #####
oppdata <- read.csv("~/GitHub/Opportunities/src/opportunitydata.csv")  # Don't forget to add this argument for the NA's
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





