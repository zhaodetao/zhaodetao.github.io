---
layout:     post
title:      Summary on linear regression--Based on advertisement data
subtitle:   如何切换Xcode命令行工具
date:       2019-10-23
author:     DETAO
header-img: img/data-analytics1.jpg
catalog: true
tags:
    - Data Analysis
    -Linear model
    
---
# Backgroud 

Linear model is coommonly used in economic research. In this eassy I will use a practical case to very its assumption. The dataset contain 200 pieces of observation which is sales, TV, radio and newspaper. TV,radion and newspaper is amount of budget on these three means of advertising. Sales is amount of products sold. Now I will need to answer the follwing questions:

* Is abvertisement budget related on sales?
* which media could contribute on sales?
* How to accurate estimate each media's effect on sales?
* Is this relationship linear?
* Is there synergistic effect between different media?

# Main body

## Simple linear regession
```{r}
data=read.csv("C:/Users/11525/Desktop/advertise.csv")
fit=lm(Sales~TV,data = data)
fit1=lm(Sales~Radio,data = data)
fit2=lm(Sales~Newspaper,data = data)
plot(data$TV,data$Sales, xlab = "TV", ylab = "Sales")
abline(coef(fit),col="red")
summary(fit)
plot(data$Radio,data$Sales, xlab = "Radio", ylab = "Sales")
abline(coef(fit1),col="red")
summary(fit1)
plot(data$Newspaper,data$Sales, xlab = "Newspaper", ylab = "Sales")
abline(coef(fit2),col="red")
summary(fit2)        
```
