# read data in
CloudData <- read.csv("~/Desktop/Data Science/data/CloudData1.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(CloudData) <- c('v.min', 'v.max', 'v.mean', 'v.mean distribution', 'v.contrast', 'v.entropy', 'v.second angular momentum', 'IR.Min', 'IR.Max', "IR.Mean")
summary(CloudData)
#change all ? values to NA
CloudData[CloudData == "?"] <- NA

#transform all character attributes to numerics
CloudData$v.max <- as.numeric(CloudData$v.max)
CloudData$v.mean <- as.numeric(CloudData$v.mean)
CloudData$`v.mean distribution` <- as.numeric(CloudData$`v.mean distribution`)
CloudData$v.contrast <- as.numeric(CloudData$v.contrast)
CloudData$v.entropy <- as.numeric(CloudData$v.entropy)
CloudData$`v.second angular momentum` <- as.numeric(CloudData$`v.second angular momentum`)
CloudData$IR.Mean <- as.numeric(CloudData$IR.Mean)
CloudData$IR.Max <- as.numeric(CloudData$IR.Max)
CloudData$IR.Min <- as.numeric(CloudData$IR.Min)

#look at histograms and boxplots to find outliers
hist(CloudData$v.min)
hist(CloudData$v.max)
hist(CloudData$v.mean)
boxplot(CloudData$v.mean)
hist(CloudData$`v.mean distribution`)
boxplot(CloudData$`v.mean distribution`)
hist(CloudData$v.entropy)
hist(CloudData$v.contrast)
boxplot(CloudData$v.contrast)
boxplot(CloudData$`v.second angular momentum`)
hist(CloudData$`v.second angular momentum`)
hist(CloudData$IR.Mean)
boxplot(CloudData$IR.Mean)
hist(CloudData$IR.Min)

#remove outliers
install.packages('data.table')
library(data.table)
library(ggplot2)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(CloudData,"v.min",which(as.numeric(CloudData$v.min) > 80),NA)
outlierReplace(CloudData,"v.mean", which(as.numeric(CloudData$v.mean) > 150, NA))
outlierReplace(CloudData,"v.contrast", which(CloudData$v.contrast>2500, NA))
outlierReplace(CloudData,"v.second angular momentum", which(CloudData$`v.second angular momentum`>100, NA))
outlierReplace(CloudData,"IR.Min", which(CloudData$IR.Min < 40, NA))
outlierReplace(CloudData,"IR.Mean", which(CloudData$IR.Mean< 150, NA))
sum(is.na(CloudData))

#check if any rows have more than 20% of data missing
library(DMwR)
manyNAs(CloudData, 0.2)
#remove rows that have more than 20% of data missing
CloudData <- CloudData[-manyNAs(CloudData),]
summary(CloudData)

#imputation

#check if correlation exists
symnum(cor(CloudData, use = "complete.obs"))

#use correlation to estimate missing contrast data based on mean distribution value
lm(v.contrast~ `v.mean distribution`, data= CloudData)
sum(is.na(CloudData$v.contrast))
CloudData[135,"v.contrast"]<- -178.4+12579.4*CloudData[135,"v.mean distribution"]
CloudData[158,"v.contrast"]<- -178.4+12579.4*CloudData[158,"v.mean distribution"]
CloudData[428,"v.contrast"]<- -178.4+12579.4*CloudData[428,"v.mean distribution"]
CloudData[647,"v.contrast"]<- -178.4+12579.4*CloudData[647,"v.mean distribution"]
CloudData[674,"v.contrast"]<- -178.4+12579.4*CloudData[674,"v.mean distribution"]
CloudData[572,"v.contrast"]<- -178.4+12579.4*CloudData[572,"v.mean distribution"]





#use knn imputation to estimate all other NA's 
CloudData <- knnImputation(CloudData, k=32)

#check that there are no more NA
sum(is.na(CloudData))

#look at histograms again to see any interesting patterns
hist(CloudData$v.min)
hist(CloudData$v.max)
hist(CloudData$v.mean)
hist(CloudData$v.entropy)
hist(CloudData$v.contrast)
hist(CloudData$`v.mean distribution`)
hist(CloudData$`v.second angular momentum`)
hist(CloudData$IR.Min)
hist(CloudData$IR.Max)
hist(CloudData$IR.Mean)


#transform/Normalize data 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
CD.normalized <- as.data.frame(lapply(CloudData, normalize))

#save new data as "CloudData.clean"
write.csv(CD.normalized, file = "CloudData.clean.csv")


