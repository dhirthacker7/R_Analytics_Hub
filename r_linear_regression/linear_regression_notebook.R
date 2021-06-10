---
title: "R Notebook"
output: html_notebook
---

Loading ggplot
&
Importing dataset
```{r}

library(ggplot2)

trainingSet = read.csv('train.csv')
View(trainingSet)
```

Checking for NA and missing values

```{r}

numberofNA = length(which(is.na(trainingSet) == T))
if(numberofNA > 0) {
  cat('Number of missing values found: ', numberofNA)
  cat('\nRemoving missing values...')
  trainingSet = trainingSet[complete.cases(trainingSet), ]
}
```

Checking for outliers
Dividing graph area into two columns

```{r}

par(mfrow = c(1,2))
```

Boxplot for x

```{r}

boxplot(trainingSet$x, main='X', sub=paste('Outliers: ', 
                                           boxplot.stats(trainingSet$x)$out))
```

Boxplot for y

```{r}

boxplot(trainingSet$y, main='Y', sub=paste('Outliers: ', 
                                           boxplot.stats(trainingSet$y)$out))
```

Finding correlation

```{r}
cor(trainingSet$x, trainingSet$y)
```

0.99 shows a very strong correlation

Fitting simple linear regression
. is used to fit predictor using all independent variables

```{r}
regressor = lm(formula = y ~.,
               data = trainingSet)

summary(regressor)
```

In Linear Regression, the Null Hypothesis is that the coefficients associated with the variables is equal to zero. 

The alternate hypothesis is that the coefficients are not equal to zero 
(i.e. there exists a relationship between the independent variable in question and the dependent variable).

P value has 3 stars which means x is of very high statistical significance.

P value is less than 0. Genraaly below 0.05 is considered good.

R-Squared tells us is the proportion of variation in the dependent (response) variable that has been explained by this model.

R square is 0.99 which shows very good variation between dependent variable(y) and independent variable(x).


Visualizing training set results

```{r}
ggplot() + 
  geom_point(aes(x = trainingSet$x, y = trainingSet$y),
             colour = 'red') + 
  geom_line(aes(x = trainingSet$x,
                y = predict(regressor, newdata = trainingSet))) + 
  ggtitle('X vs Y (Training set)') + 
  xlab('X') + 
  ylab('Y')
```

No outliers present and there is a linear relationship


Importing test data
```{r}
testSet = read.csv('test.csv')
```

Predicting test results
```{r}
y_pred = predict(regressor, newdata = testSet)
```

Visualizing the test set results

```{r}
ggplot() +
  geom_point(aes(x = testSet$x, y = testSet$y),
             colour = 'red') +
  geom_line(aes(x = trainingSet$x, y = predict(regressor, newdata = trainingSet)),
            colour = 'blue') +
  ggtitle('X vs Y (Test set)') +
  xlab('X') +
  ylab('Y')
```
Plot shows model was a good fit

Finding accuracy

```{r}
compare <- cbind(actual=testSet$x, y_pred)
mean(apply(compare, 1, min)/apply(compare, 1, max))
mean(0.9, 0.9, 0.9, 0.9)
```

Check for residual mean and distribution

```{r}

plot(trainingSet$y, resid(regressor), 
     ylab="Residuals", xlab="Price", 
     main="Residual plot") 
mean(regressor$residuals)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
