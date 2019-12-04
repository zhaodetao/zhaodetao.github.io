---
layout:     post
title:      Regularization Challenge
subtitle:   
date:       2019-12-04
author:     DETAO Yifeifang
header-img: img/data-analytics1.jpg
catalog: true
tags:
    - Data Analysis
    - Lasso
    
---

## Introduction
In this report we use the data from China General Social Survey (CGSS) data to find out what indicators are good for predicting a Chinese household's income. After our filtering, we used 1325 observations and 132 variables. These variables cover Socio-demographic attributes of main labor force in a family, education level, religion belief, health condition,life style, social attitude, class cognition, personal cognitive ability, labor market and social security. In addition we include the other family members information especially of their parents.  


## Data Analysis

```{r}
data=read.csv("C:/Users/11525/Desktop/CGSS_2015.csv")
dim(data) 
```

In our sample we totally have 1325 obsvevations and 132 variables.

```{r}
set.seed(1)
sam=sample(nrow(data)+1,(nrow(data)+1)/2)
train=data[sam,]
test=data[-sam,]
```

First of all, we divide our sample into two parts: training data and test data. We randomly select half of sample as our training data and the other test data.

```{r}
library(glmnet)
library(Matrix)
x.train=model.matrix(householdin~.,train)[,-126]
y.train=train$householdin
grid=10^seq(10,-2,length=100) #range of lamda
lasso.fit=glmnet(x.train,y.train,alpha = 1,lambda = grid)
plot(lasso.fit)
```
![png](/img/1241.png)

Then I used the train data to estimate our model. Generally, people think that when $ \lambda $ belongs to $ \ 10^{-2} $ and $\ 10^{10} $ coukd cover all the condition. And we use different $ \lambda $to run our model and plot the results. We find that if we want to reduce our dimension we need large $ \lambda $. But how large is suitable we still don't know.

```{r}
set.seed(2)
cv.out=cv.glmnet(x.train,y.train,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.fit1=glmnet(x.train,y.train,alpha = 1,lambda =bestlam)
coe=coef(lasso.fit1)
coe[which(rowSums(coe==0)==0),]  #find out nonzero coefficient
```

![png](/img/1242.png)

| variable |     coefficient  |
| ----------| --------- | 
| a2       |    20637      |
| a8a      |     1.69      |
| a281     |      1373     |
| a34      |       -498    |
| a35      |     -1865     |
| a65      |   20859       |
| a6703    |   94174       |
| a81a     |   789         |
| a81b     |   2402        |
| a89h     |    4160       |

In order to approach our puzzle above, we use the cross-validation to select our best $ \lambda$, and we find that proper lambda is indeed large. And we use selected value to eatimate our model. A person's gender, total income, how often read newspaper, whether like to free ride, attitude to social fairness, amount of house property, whether invest in stock market, whether spouse has a part-time job and the nature of parents' company is important to predict a househould income.

```{r}
x.test=model.matrix(householdin~.,test)[,-126]
y.test=test$householdin
lasso.predict=predict(lasso.fit1,s=bestlam,newx =x.test)
mean((lasso.predict-y.test)^2)
```
In the end we test our prediction result (5240664834) which is very large out of our exception. Mainly because our data has a lot dummy variables and category variables. This is what we need to improve in the future.
