# Boston Exercise Regression Trees for Housing Data

# Load data
boston <- read.csv("boston.csv")
str(boston)

# Exploring the data with plots

plot(boston$LON, boston$LAT)
# census tracs along the Charles river
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1], col="blue", pch=19)
# MIT census trac
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531], col="red", pch=19)
# checking out pollution stats
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55], col="green", pch=19)
# reset the plot
plot(boston$LON, boston$LAT)
# Looking at median home prices
summary(boston$MEDV)
# Areas where home prices are above average
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)

# checking out predicting house prices based on latitutde and longitutde
latlonlm = lm(MEDV~ LAT + LON, data=boston)
summary(latlonlm)
# plot the regression line on the plot graph
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values>=21.2], boston$LAT[latlonlm$fitted.values>=21.2], col="blue", pch="$")

# use regression trees
library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV~LAT + LON, data = boston)
prp(latlontree)
plot(boston$LAT, boston$LON)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>=21.2], boston$LAT[fittedvalues>=21.2], col="blue", pch="$")

# create a much more simple regression tree using minbucket
latlontree = rpart(MEDV~LAT + LON, data = boston, minbucket=50)
plot(latlontree)
text(latlontree)

plot(boston$LON, boston$LAT)
#plot the lines of the tree
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)

# test which model is better, linear regression versus regression tree
library(caTools)
# create our training and testing data
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio=0.7)
train = subset(boston, split==TRUE)
test = subset(boston, split==FALSE)
# create the linear model
linreg = lm(MEDV~LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data=train)
summary(linreg)
# calculate SSE
linreg.pred = predict(linreg, newdata = test)
linreg.see = sum((linreg.pred - test$MEDV)^2)
linreg.see
# [1] 3037.088, can we beat this with our regression tree model???
tree = rpart(MEDV~LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data=train)
# interesting to see what variables it uses versus the linear model
prp(tree)
# calculate SSE
tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse
#[1] 4328.988, the linear model does a better job at predicting home prices than the regression tree model


# cross validation using cp
library(caret)
library(e1071)
# using 10 folds
tr.control = trainControl(method="cv", number = 10)
# create a grid of all the cp values we want the model to try
cp.grid = expand.grid(.cp = (0:50)*0.01)
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr
# RMSE was used to select the optimal model using  the smallest value.
# The final value used for the model was cp = 0.001.
best.tree = tr$finalModel
prp(best.tree)
best.tree.pred = predict(best.tree, newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
# [1] 3675.766 better than our last regression tree model but still not better than the linear model. Always use cross validation to get the best 
# regression tree model
