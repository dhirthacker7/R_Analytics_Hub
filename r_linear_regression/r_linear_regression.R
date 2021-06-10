# Loading ggplot
library(ggplot2)

# Importing dataset
trainingSet = read.csv('train.csv')
View(trainingSet)

# Checking for NA and missing values
numberofNA = length(which(is.na(trainingSet) == T))
if(numberofNA > 0) {
  cat('Number of missing values found: ', numberofNA)
  cat('\nRemoving missing values...')
  trainingSet = trainingSet[complete.cases(trainingSet), ]
}

# Checking for outliers
# Dividing graph area into two columns
par(mfrow = c(1,2))

# BOxplot for x
boxplot(trainingSet$x, main='X', sub=paste('Outliers: ', 
                                           boxplot.stats(trainingSet$x)$out))

# BOxplot for y
boxplot(trainingSet$y, main='Y', sub=paste('Outliers: ', 
                                           boxplot.stats(trainingSet$y)$out))

# Finding correlation

cor(trainingSet$x, trainingSet$y)

# 0.99 shows a very strong correlation

# Fitting simple linear regression
# . is used to fit predictor using all independent variables
regressor = lm(formula = y ~.,
               data = trainingSet)

summary(regressor)

# P value has 3 stars which means x is of very high statistical significance.
# P value is less than 0. Generally below 0.05 is considered good.

# R-Squared tells us is the proportion of variation in the dependent(response)
# variable that has been explained by this model.

# R square is 0.99 which shows very good variation between 
# dependent variable(y) and independent variable(x).

# Visualizing training set results
ggplot() + 
  geom_point(aes(x = trainingSet$x, y = trainingSet$y),
             colour = 'red') + 
  geom_line(aes(x = trainingSet$x,
                y = predict(regressor, newdata = trainingSet))) + 
  ggtitle('X vs Y (Training set)') + 
  xlab('X') + 
  ylab('Y')

# No outliers present and there is a linear relationship



# Importing test data
testSet = read.csv('test.csv')
View(testSet)

# Predicting test results
y_pred = predict(regressor, newdata = testSet)

# Visualizing the test set results
ggplot() +
  geom_point(aes(x = testSet$x, y = testSet$y),
             colour = 'red') +
  geom_line(aes(x = trainingSet$x, y = predict(regressor, newdata = trainingSet)),
            colour = 'blue') +
  ggtitle('X vs Y (Test set)') +
  xlab('X') +
  ylab('Y')

# Plot shows model was a good fit

#Finding accuracy
compare <- cbind(actual=testSet$x, y_pred)
mean(apply(compare, 1, min)/apply(compare, 1, max))
mean(0.9, 0.9, 0.9, 0.9)

# Check for residual mean and distribution
plot(trainingSet$y, resid(regressor), 
     ylab="Residuals", xlab="Price", 
     main="Residual plot") 
mean(regressor$residuals)