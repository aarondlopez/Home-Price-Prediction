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
StevensTree = rpart(Reverse~Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", control=rpart.control(minbucket=25))
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