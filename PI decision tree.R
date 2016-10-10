library(mlbench)
library(caret)

#load data
data(PimaIndiansDiabetes)
PimaIndiansDiabetes$diabetes <- as.factor(PimaIndiansDiabetes$diabetes)
View(PimaIndiansDiabetes)

#randomize data and split into test(25%) and train(75%)
set.seed(123)
PI_rand <- PimaIndiansDiabetes[order(runif(768)), ]
PI_train <- PI_rand[1:576, ]
PI_test <- PI_rand[577:768, ]

#create model
library(C50)
diabetes_full_model <- C5.0(PI_train[-9], PI_train$diabetes)

#test model using test data
diabetes_prediction <- predict(diabetes_full_model, PI_test)

#results in a confusion matrix
library(gmodels)
CrossTable(PI_test$diabetes, diabetes_prediction,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diabetes', 'predicted diabetes'))


#look at attributes to see if there is a correlation
correlationMatrix <- cor(PI_train[,1:8])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
print(correlationMatrix)
print(highlyCorrelated)

#use random forest to identify top 5 variables
library(mlbench)
library(caret)
library(randomForest)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
attribute_results <- rfe(PI_train[,1:8], PI_train[,9], sizes=c(1:8), rfeControl=control)
print(attribute_results)
predictors(attribute_results)
plot(attribute_results, type=c("g","o"))

#notice that age and pregnant are highly correlated, so try removing each and see what the results look like
#also notice that the accuracy is highest with 7 variables, so make a model with everything but triceps
reduced_train1 <- PI_train[ ,c(-1,-3,-4,-7)]
reduced_train2 <- PI_train[ ,c(-8,-3,-4,-7)]
reduced_train3 <- PI_train[ ,-4]
View(reduced_train2)

#make new model with each training set
reduce_model_1 <- C5.0(reduced_train1[-5], reduced_train1$diabetes)
reduce_model_2 <- C5.0(reduced_train2[-5], reduced_train2$diabetes)
reduce_model_3 <- C5.0(reduced_train3[-8], reduced_train3$diabetes)

#test each model using test sets
reduced_prediction_1 <- predict(reduce_model_1, PI_test)
reduced_prediction_2 <- predict(reduce_model_2, PI_test)
reduced_prediction_3 <- predict(reduce_model_3, PI_test)

#plot confusion matrix of each to compare results
CrossTable(PI_test$diabetes, reduced_prediction_1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diabetes', 'predicted diabetes'))
CrossTable(PI_test$diabetes, reduced_prediction_2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diabetes', 'predicted diabetes'))
CrossTable(PI_test$diabetes, reduced_prediction_3,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diabetes', 'predicted diabetes'))

#The second model (using pregnant, glucose, insulin, and mass) had the most correct diagnoses,
#and more false positives than false negatives, which is probably better when diagnosing 
#diabetes, as you probably want to be on the safer side. Thus I would choose reduce_model_2 as
#a subset

#The reduced model had more correct diagnoses (135 vs. 131) It also had fewer false negatives (24 vs.37) and more false positives (33 vs 24)

