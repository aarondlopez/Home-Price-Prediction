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