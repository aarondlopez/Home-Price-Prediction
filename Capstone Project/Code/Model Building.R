clean_df_rmna <- na.omit(clean_df)

# Test models
set.seed(123)
# create test and train data sets
split = sample.split(clean_df_rmna$AvgPriceHome, SplitRatio = 0.7)
train = subset(clean_df_rmna, split==TRUE)
test = subset(clean_df_rmna, split==FALSE)
# create linear regression model
linereg <- lm(AvgPriceHome~AvgAptRent+URateSJ+Rates, data = train)
linereg.pred = predict(linereg, newdata = test)
linereg.rmse = sqrt(mean((linereg.pred - test$AvgPriceHome)^2))
linereg.rmse
# [1] 42637.19
# create regression tree model
treereg <- rpart(AvgPriceHome~AvgAptRent+URateSJ+Rates, data = train)
prp(treereg)
treereg.pred = predict(treereg, newdata = test)
treereg.rmse = sqrt(mean((treereg.pred - test$AvgPriceHome)^2))
treereg.rmse
# [1] 24843.1

# cross validation using complexity parameter (cp)
library(caret)
library(e1071)
# using 10 folds 
tr.control = trainControl(method="cv", number = 10)
# create a grid for all cp values to try
cp.grid = expand.grid(.cp=(0:10)*0.001)
treebest <- train(AvgPriceHome~NumJobs+AvgAptRent+Avg1bdAptRent+Avg2bdAptRent+AvgPriceCondo+TotalJobs+URateSJ+URateSJMetro+Rates, data=train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
treebest
# RMSE was used to select the optimal model using  the smallest value.
# The final value used for the model was cp = 0.
treebest <- treebest$finalModel
prp(treebest)
best.tree.pred = predict(treebest, newdata = test)
best.tree.rmse = sqrt(mean((best.tree.pred - test$AvgPriceHome)^2))
best.tree.rmse
# [1] 3591.5


