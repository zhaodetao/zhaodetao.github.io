---
layout:     post
title:      Summary on linear model--Based on advertisement data
subtitle:   
date:       2019-10-23
author:     DETAO
header-img: img/data-analytics1.jpg
catalog: true
tags:
    - Data Analysis
    - Linear Model
    
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

From regression result we could find that these advertisements could contribute sales. Until now our story is not over, what we get from above could not answer the quetion.

## Multiple linear regression

From above discussion we may have omitted variable prolem. Mainly because TV, newspaper and radion are related to each other and could determine sales, this phenomenon may give rise to omitted problem. Therefore doing a multiple regression is necessary.

```{r}
fit3=lm(Sales~TV+Radio+Newspaper,data = data)
summary(fit3)
```

From regression result we find that newspaper is not statiscally significant, which is controdict to single linear regression results. Then we use different ways to check whether they satisfy our model assumption.

### normality

Why need normality? Mainly because when dependent variables are normal distribution, it is possible for error term to normally distribute with zero mean. Normality is important especially when sample is small. Otherwise we may overestiamte or underestimate statistical inference. In R we often use qqplot functin to check normality. 

```{r}
library(car)
qqPlot(fit3,labels=row.names(data),id.metod="identity",simulate = T,main="QQ-plot")
data[6,]
data[131,]
```
From the qq-pot we could see that most point lay within line which mean that normality satisties except the 6th and 131th observation. Normility is meet.

### Correlation of error term

Before refression, we need to gurantee error is independent among error term. Of course it is diffcult to judge whether they are related from our intuition. We frequently identify through data. In R we use durbinWatsonTest function to check our assumotion. If p-value is not significant, our assumption is satisfied.

```{r}
durbinWatsonTest(fit3)
```
From result we that p-value is not significant which means that denpent variables are independent.

### Linearity

When using linear regression, we assume that the ture relationship is linear. If actual relationship is not linear, linear regression may be not a good choice. Generally, we use residual plot to distinguish whether it is linear or not.

```{r}
crPlots(fit3)
```

From partical residual plot we could conclude that our linear assumption is right. 

### Non-constant variance of error term

Non-constant variance of error term issue commonly exists in our real life. But it has very important influence on our result. If variance of error term is diferent to each other. The standard error calculated may be not correct, as a result statiscal inferebce may be meaningless. In R we use ncvTest function to find this issue.

```{r}
ncvTest(fit3)
spreadLevelPlot(fit3)
```

In this regression we find that the issue of non-constant variance of error term is not serious. 

### Multicollinearty

In multuple linear regression  multicollinearty is a big issue that we need to take into account. One concern is that when  multicollinearty exists, we hardly distinguish single regressor's effect on response variable. In order to solve this problem we bring in varision inflation factor to judge. Usually when VIF is large we could say that there is multicollinearty. Meanwhile we use correlatin matrix to identify the relationship between regressors.

```{r}
library(corrplot)
res=cor(data[,-c(1,5)])
corrplot(res,,type="upper")
vif(fit3)
```
 Since indexes are small, there is no mulcollineaty problem. But we find that radio is positively related neaspaper. This may explain that newspaper avtually affect sales. As reason why newspaper is significant is that in the market where people invest more radion, more newspaper is invested. The factor that turly determine sales is radio not newspaper.
 
### Outlier

We know that outlier could have significant effect on regression result, in R we could use outTest() funcion to find out the outliers.

```{r}
outlierTest(fit3)
```

From resut we know that the 131th observation is a outlier. So when we do regression this piont should be omited.

### High-leverage point

Another important issue is that high-leverage point also infulences the accuracy of the estimator. And high-leverage point is different from the oultier. The former is the set that contains some special value in independent variables, while the latter is the set that is made of specia dependent values. In R we usually use hat statistic to check high-leverage point.

```{r}

hat.plot=function(fit3){
  p=length(coefficients(fit3))
  n=length(fitted(fit3))
  plot(hatvalues(fit3),main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
}
hat.plot(fit3)
```

Figure above show that in this data set there certain high-leverage points which may cause inaccuracy in regression.

### Strong influence point

With strong influence point omited our model may change a lot. This means that our model may not stable in the influence of strong point.In R we use Cook distance to measure strong point. Usually when Cook distance is lager than 4, we regard it as strong influence point.

```{r}
cutoff=4/(nrow(data)-length(fit3$coefficients)-2)
plot(fit3,which = 4,cook.levels = cutoff)
abline(h=cutoff,lty=2,col="red")
```

Surprsingly there only three strong influence points in our dataset. What's more the 6th and 131th point are also outlier. In addition, in R we could use influence plot to check outlier, high-leveragr and strong influence point in the same time.

```{r}
influencePlot(fit3,main="Influence plot",sub="Circle size is proportionak to cook 's distance")
```

Through our check know that there are 4 points that we should't include in our regression dataset. With them our regression result may be not accurate.

After checking our assumption, we need to modify our data and model.

```{r}
new_data=data[-c(6,17,102,131),]
fit4=lm(Sales~TV+Radio+Newspaper,data = new_data)
summary(fit4)
```


# Conclusion

Admittedly liear model is easily to use in some cases, however we should check its assumption befor we get our conclusion. Common assumptions that we need to check are normality of dependent variables, correlaion of error term, non-constant variance of error term, mulcollineraty, outlier, high-leverage and influential observation problems. Only after checking asumption satisfied could we get relatively reasonable results and explaination. 

Now we could answer the question put forward before, advertisement budget is certainly related with sales. Both Tv and radio contribute to the sales of product. In additon, holding on other factors fixe increasing  1000 dollars on TV abvertisement budget could sell more 44 units of products. As well investing another 10000 dollars on radion would increase about 196 units of sold goods. Therefore we could conclude that it is reasonable to invest more on radio advertisement. Fortunately, we find that linear model is a wonderul estimate according to partial residual plot. Of course, newspaper is related to the radio and newspaper actually affect sales. As for the reason why single linear regression it is significant is because when more newspaper is invested, more radio is also added.
