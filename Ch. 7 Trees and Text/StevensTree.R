# Load data
stevens <- read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)

# create train and test data sets
split = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train= subset(stevens, split == TRUE)
Test = subset(stevens, split == FALSE)
library(rpart)
library(rpart.plot)

# create CART model
StevensTree = rpart(Reverse~Circuit + Issue + Petitioner + Respondent + LowerCourt + 
                      Unconst, data = Train, method="class", control=rpart.control(minbucket=25))
prp(StevensTree)
PredictCART = predict(StevensTree, newdata=Test, type="class")
table(Test$Reverse, PredictCART)
#PredictCART
#    0  1
# 0 41 36
# 1 22 71
(41+71)/(41+36+22+71)
#[1] 0.6588235

# Plot ROC graph
library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC
pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# Create a random forest model
library(randomForest)
StevensForest = randomForest(Reverse~Circuit + Issue + Petitioner + Respondent + 
                               LowerCourt + Unconst, data=Train, nodesize=25, ntree=200)
#Warning message:
#In randomForest.default(m, y, ...) :
  #The response has five or fewer unique values.  Are you sure you want to do regression?

# Change our y variable to a factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
StevensForest = randomForest(Reverse~Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize=25, ntree=200)

# Use the predict function on our model using the test data set to determine the accuracy of our model
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
#PredictForest
#   0  1
#0 40 37
#1 18 75
(40+74)/(40+37+18+75)
#[1] 0.6705882
# The model accurately predicted 67% of the actual outcomes, better than the CART Model at 65%





