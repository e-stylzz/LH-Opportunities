##### File 2: Data Preperation #####

# Remove 'In Progress' opportunities from primary data frame:
data <- data.frame(data[,]) %>%
  filter( data$Status != "In Progress")

# Set dates to 'Date' type(called "class" in R)
data$AvailabilityDate <- as.Date(data$AvailabilityDate, format = "%m/%d/%Y")
data$OppoEndDate <- as.Date(data$OppoEndDate, format = "%m/%d/%Y")
data$OppoStartDate <- as.Date(data$OppoStartDate, format = "%m/%d/%Y")
data$TargetCloseDate <- as.Date(data$TargetCloseDate, format = "%m/%d/%Y")
data$CreatedDate <- as.Date(data$CreatedDate, format = "%m/%d/%Y")

# Create groups for the oppo Duration:
data$OppDurationGroup <- '0-30'
data$OppDurationGroup[data$OppoDuration >= 30 & data$OppoDuration < 60] <- '30-60'
data$OppDurationGroup[data$OppoDuration >= 60 & data$OppoDuration < 90] <- '60-90'
data$OppDurationGroup[data$OppoDuration >= 90 & data$OppoDuration < 120] <- '90-120'
data$OppDurationGroup[data$OppoDuration >= 120 & data$OppoDuration < 150] <- '120-150'
data$OppDurationGroup[data$OppoDuration >= 150 & data$OppoDuration < 180] <- '150-180'
data$OppDurationGroup[data$OppoDuration >= 180] <- 'Over 180'
data$OppDurationGroup <- as.factor(data$OppDurationGroup)

# Create 'Estimated Hours' group:
data$EstHoursGroups <- '0-40'
data$EstHoursGroups[data$EstHours >= 40 & data$EstHours < 80] <- '40-80'
data$EstHoursGroups[data$EstHours >= 80 & data$EstHours < 120] <- '80-120'
data$EstHoursGroups[data$EstHours >= 120 & data$EstHours < 160] <- '120-160'
data$EstHoursGroups[data$EstHours >= 160 & data$EstHours < 200] <- '160-200'
data$EstHoursGroups[data$EstHours >= 200 & data$EstHours < 300] <- '200-300'
data$EstHoursGroups[data$EstHours >= 300 & data$EstHours < 400] <- '300-400'
data$EstHoursGroups[data$EstHours >= 400 & data$EstHours < 500] <- '400-500'
data$EstHoursGroups[data$EstHours >= 500 & data$EstHours < 750] <- '500-750'
data$EstHoursGroups[data$EstHours >= 750 & data$EstHours < 1000] <- '500 - 1000'
data$EstHoursGroups[data$EstHours >= 1000] <- 'over 1000'

data$EstHoursGroups <- as.factor(data$EstHoursGroups)

# Create Estimated Rate Groups:
data$EstRateGroups <- '0-100'
data$EstRateGroups[data$EstRate >= 100 & data$EstRate < 125] <- '100-125'
data$EstRateGroups[data$EstRate >= 125 & data$EstRate < 150] <- '125-150'
data$EstRateGroups[data$EstRate >= 150 & data$EstRate < 175] <- '150-175'
data$EstRateGroups[data$EstRate >= 175 & data$EstRate < 200] <- '175-200'
data$EstRateGroups[data$EstRate >= 200] <- 'over 200'
data$EstRateGroups <- as.factor(data$EstRateGroups)

# Create ITR Groups:
data$ITRGroups <- '0-100'
data$ITRGroups[data$ITR >= 100 & data$ITR < 125] <- '100-125'
data$ITRGroups[data$ITR >= 125 & data$ITR < 150] <- '125-150'
data$ITRGroups[data$ITR >= 150 & data$ITR < 175] <- '150-175'
data$ITRGroups[data$ITR >= 175 & data$ITR < 200] <- '175-200'
data$ITRGroups[data$ITR >= 200] <- 'over 200'
data$ITRGroups <- as.factor(data$ITRGroups)

# Create a 'Won/Lost' Binary variable. Options are Status = 'Lost' vs everything else:
data$WonLost <- '1'
data$WonLost[data$Status == 'Lost'] <- '0'
data$WonLost <- as.factor(data$WonLost)

#Create Variables of columns
vars <- colnames(data)
catVars <- vars[sapply(data[,vars],class) %in% c('factor','character')]
numericVars <- vars[sapply(data[,vars],class) %in% c('numeric','integer')]

#Other Variables for later use
outcome <- 'WonLost'
pos <- '1'

#Create Train and Test Data frames
data$RandGroup <- runif(dim(data) [1])
test <- subset(data, data$RandGroup <= 0.3)
trainall <- subset(data, data$RandGroup > 0.3)
#Break up all training data into train and calibration
useForCal <- rbinom(n=dim(trainall) [[1]],size=1,prob=0.1)>0
cal <- subset(trainall,useForCal)
train <- subset(trainall, !useForCal)
