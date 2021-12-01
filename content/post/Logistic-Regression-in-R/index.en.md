---
# Documentation: https://wowchemy.com/docs/managing-content/

title: "Logistic Regression in R"
subtitle: ""
summary: ""
authors: []
tags: []
categories: []
date: 2021-11-24T20:47:51-05:00
lastmod: 2021-11-24T20:47:51-05:00
featured: false
output:
  blogdown::html_page:
    toc: false
draft: false
runtime: shiny

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder.
# Focal points: Smart, Center, TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight.
image:
  caption: ""
  focal_point: ""
  preview_only: false

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects: []
---


Logistic regression,(also known as logit model) is a regression model used to model dichotomous outcome variables. The underlying similarities between Logistic and Linear regression models can be both a blessing, and a curse. Providing students that have some prior familiarity with linear models many opportunities to bridge their knowledge between the two models, but at the same time creating treacherous pitfalls in how logit models should be interpreted differently from linear models.

{{< toc >}}


Here are the packages that we will be using throughout this tutorial

```{r}
library(tidyverse)
library(gt)
library(MASS)
library(ggplot2)
library(ggthemes)
library(car)
library(titanic)
```

In this blog post we will approach Logistic Regression in a manner similar as to how we did so for linear models in class. We will first start by analyzing and understanding the underlying mathematical principles of the logit, followed by discussing the crucial assumptions of the model,and then applying these concepts to three logistic models; An intercept only mode, a single-predictor model, and a multiple predictor model.

We will finalize this blog-post by plotting some visualizations of our model, but before we move on I think it is important to visualize what a traditional logistical regression plot actually looks like before we even begin talking about it.

This post will assume that you have some familiarity with the GLM approach to data analysis, and therefore you should probably know by now that in a traditional linear modelling approach, our model is fitting a line to the data (hence the name linear modelling). In the case of logistic regression, we plot a regression 'curve' that has a characteristic sinusoidal shape. 

```{r eval=FALSE, include=FALSE}
ggplot(mtcars, aes(x=hp, y=mpg)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE)
```
{{< figure library="true" src="lin_cars.png" >}}

Here is an example of a logistic regression curve being plotted on the classic mtcars data set, where horsepower is being used to predict whether the car model has a "V"(0) or "Straight"(1) engine. 

```{r}
ggplot(mtcars, aes(x=hp, y=vs)) + 
  geom_point(size = 2,alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial), col = "darkred")+
  theme_bw(base_size = 15)
```

{{< figure library="true" src="log_cars.png" >}}

We will see in more detail throughout this blog post what exactly is going on behind the scenes for logit models.

## Part I: Logistic regression and the logit

Logistic regression is a modelling approach that fundamentally deals with probabilities. Our main goals with this model are to model and estimate the probability of an event occurring, given specific values of a set of independent variables. This consequently allows us to predict the effect of these variables on a dependent variable of binary response, and therefore classify observations by the probability of belonging to a category related to the occurrence of this binary event.

It is important to note that these principles already bring some important distinctions between logistic and linear regression models. First, binary data does not follow a normal distribution, one of the primary assumptions needed to employ linear models. And secondly, the output of a logit regression may not be as clear cut to interpret as in a traditional linear regression, so let's go over things step by step!

As mentioned above, logistic regression deals primarily with probabilities. You may recall that calculating the probability(p) of a given event occurring is as simple as dividing the number of occurrences(Sn) of this given event by the the total number of observations(n), that is:

$$
p = \frac{Sn}{n}
$$
However, importantly, logit models work with not just probabilities, but the _Odds_ of a given event happening. These are calculated as the ratio of the probability of an event happening(p) over the probability of the event not happening. Alternatively, the odds of an event can also be calculated by dividing the number of successes (Sn) over the number of failures (Fn):

$$
Odds = \frac{Sn}{Fn} = \frac{\frac{Sn}{n}}{\frac{Fn}{n}}= \frac{p}{1-p}
$$
I would also like to introduce another important concept in logistic regression, whose importance will become more clear later on, that of _Odds Ratio(OR)_  which is simply the ratio of two odds:

$$
OR = \frac{\frac{p_1}{1-p_1}}{\frac{P_0}{1 -P_0}}
$$

Now that we  had this brief overview of some fundamental concepts in probability, we can finally go over the definition of the most important concept in logistic regression, the _logit_, which is defined as the _log(odds)_ of a given event occurring. Particularly, in logit modelling, we will model the logit with a linear function of the regressors:

$$
Odds =log(\frac{p_x}{1-p_X}) = \beta_0 + \beta_1 X
$$
Where in this formula, $p_x$ is the probability of a given event happening given $X$. In this equation, we also have the values for the coefficients, similarly as to how we see in a linear regression represented by $\beta_{0}$ (the intercept) and  $\beta_{1}$ ( the slope for variable 1). However, the interpretation of these coefficients is a bit less immediately clear than in linear regression, as we can see that they all relate to the _log(Odds)_ of an event. We will approach this in more detail when dealing with a practical example.

As we mentioned before, one of the main goals of logistic regression is _estimating the probability of a binary event occurring, given a specifc set of variables_. therefore, much like in linear regression we estimate $\hat{y}$ for a given combination of variables, in logistic regression, we can estimate $\hat{p}$ for a linear combination of the independent variables. Therefore, by rewriting the previous equation in solving for $p$, we obtain the following equation, representig the _logit equation in probability form_:

$$
\hat{p} \frac{e^{\beta_0+\beta_1 x1}}{1 + e^{\beta_0+\beta_1 x1}}
$$
Importantly, although logit models generally aim to estimate $\hat{p}$, fitting the data mathematically involves estimating the combination of values for $\beta_{0}$ and $\beta_{1}$ that yield the largest likelihood for our data. Therefore, when computing these models in R, the background process of 'fitting the data' that happens is mostly solving 'for' $\hat{beta_{0}}$ and $\hat{beta_{1}}$ that fulfill these requirements, also known as the _Maximum Likelihood Estimates (MLE)_. This is notably distinct from the process of fitting that happens in linear regression that aims to minimize residuals.

Finally, another important mathematical aspect of logit modelling for interpreting our  output, is that exponentiating $\hat{beta_{1}}$ results in an estimate of the odds ratio for the effect for that variable:

$$
e^{\hat{\beta_{1}}} = OR
$$
These concepts will hopefully become more clear once we get into specific modelling examples later on.

## Part II: Assumptions of the logistic regression model

Similar to traditional linear models, logit models also have key assumptions that must be respected.

1. Binary Variable: The dependent variable must be a dichotomous response, or the sum of dichotomous responses.

2. Independence: Observations should be independent form one another (I.e. you should not use repeated measurements)

3. Absence of Multicolinearity: Independent variables should not be highly correlated.

4. Linearity: Unlike linear regression, logistic regression does not require a linear relationship between the independent and dependent variables. However, there is an assumption of the independent variables being linearly related to the log(odds) of the outcome.

For the purposes of this tutorial, our assumptions are generally respected by virtue of the nature of the employed data set.

## Part III: Let's model some data!

### Intercept-only model

Throughout all of our examples, we will be using the very popular Titanic dataset, that contains information about the passengers of the iconic maiden and final voyage of the historical vessel of the same name.


```{r}
# Installing and loading the package that contains the data set
#install.packages("titanic")
library(titanic)
```

```{r}
# Loading the data set, converting categorical predictors into factors, removing NAs

df = titanic_train %>% 
  filter(!is.na(Age)) 

df$Age <- as.numeric(df$Age)
df$Sex <- as.factor(df$Sex)
df$Pclass <- as.factor(df$Pclass)
  
```

Throughout our models, we will be modeling the probability of surviving the sinking of the Titanic. In order to make it easier to understand some of the math going forward, let us first calculate a few important numbers:

```{r}

survival <-  as.data.frame(table(df$Survived)) %>% 
  pivot_wider(names_from = Var1, values_from = Freq) %>% 
  rename(Yes = '1', No = '0')
  

gt(survival) %>% 
  tab_header( title = "Survived the sinking of the Titanic?") %>% 
  cols_align(
    align = "center")

```
{{< figure library="true" src="tab_1.png" >}}

From this we can also calculate both the probability of survival, and the odds of survival:

$$
p = \frac{290}{714}=0.406
$$
$$
Odds = \frac{\frac{290}{714}}{\frac{424}{714}} = 0.683
$$

So according to these results, the probability of surviving the sinking of the Titanic is approximately $0.406$ or around $40%$, while the odds of surviving are $0.660$ to $1$.

Next, we will model the log(odds) of survival on a model with no predicting variables, and therefore known as a predictor-only model. This isn't very useful but can help with a gentle introduction to the modelling approach. 


```{r}

# Intercept-only model

predict_survival <- glm( Survived ~ 1, family = binomial, data = df)
summary(predict_survival)
``` 

And this is the output of this code:

```
Call:
glm(formula = Survived ~ 1, family = binomial, data = df)

Deviance Residuals: 
   Min      1Q  Median      3Q     Max  
-1.021  -1.021  -1.021   1.342   1.342  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.3799     0.0762  -4.985  6.2e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 964.52  on 713  degrees of freedom
Residual deviance: 964.52  on 713  degrees of freedom
AIC: 966.52

Number of Fisher Scoring iterations: 4

```


Admittedly, there isn't much to see here,as pretty much the only result of interest is the coefficient of the intercept :$-0.3799$. While in traditional linear models this basically represents the mean of the outcome, in a logistic regression model, the intercept represents the log(odds) of belonging to the category of the outcome, in this case, it basically means the estimated log(odds) of surviving the Titanic sinking. By exponentiating the intercept, we can see how this result simply returns an estimation of the Odds of survival: $e^{-0.3799} = 0.683$.


### Single-predictor model

Next, we will have something a little more substantial to chew on. We will now add the binary predictor of Sex to the model, and see how this affects the prediction of log(odds) of survival. First, let us once again calculate a few preliminary information that we can look back on for checking the results of our model:

```{r}
survival_gender <-  df %>% 
  dplyr::select(Survived, Sex)
  
survival_gender <- as.data.frame(table(survival_gender))

Gender <- c('Male',' Female')
Survivors <- c(109,233)
Deaths <- c(468,81)

survival_gender <- as.data.frame(cbind(Gender,Survivors,Deaths))

survival_gender$Survivors <- as.numeric(survival_gender$Survivors)
survival_gender$Deaths <- as.numeric(survival_gender$Deaths)
  

gt(survival_gender) %>% 
  tab_header( title = "Titanic survivors by gender") %>% 
  cols_align(
    align = "center")


```

{{< figure library="true" src="tab_1.png" >}}

Therefore with this information we can calculate the probability, odds, and odds ratio of survival relative to gender, with the probability of men surviving being $0.189$, and for women being $0.742$. Additionally, the odds of survival for men were $0.233$, and for women $2.877$. Finally, since we have the odds of survival for both males and females, we are also able to calculate the odds ratio of survival of men over women:$0.081$.



Now we can do the modelling proper, and see how the results compare to some of the numbers we calculated manually before


```{r}

# Single predictor model

log_sex = glm(Survived ~ Sex, family = binomial, data = df)
summary(log_sex)

```
And once again the output for the code:

```
Call:
glm(formula = Survived ~ Sex, family = binomial, data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.6767  -0.6779  -0.6779   0.7501   1.7795  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)   1.1243     0.1439   7.814 5.52e-15 ***
Sexmale      -2.4778     0.1850 -13.392  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 964.52  on 713  degrees of freedom
Residual deviance: 750.70  on 712  degrees of freedom
AIC: 754.7

Number of Fisher Scoring iterations: 4

```

We can interpret these results in the form of the equation for the logit model. First, as the logit for when the person is a woman, which can be converted into the odds of survival for women that we calculated earlier:


$$
log(\frac{p_0}{1-p_0}) = 1.1243=e^{1.1243}=3.077
$$

And for when the person is a man:

$$
log(\frac{p_1}{1-p_1}) = 1.1243 - 2.4778=-1.3535=e^{-1.3535}=0.258
$$


Now this model has results that are a bit more interesting to analyze. We can see how apart from the intercept, we now also have information regarding our predictor variable 'dummy male'. More specifically, we have:

1. An estimate for the coefficient of dummy_male, that of $-2.4778$. This value represents the log(odds) ratio between the female group and the male group (as we have males as the reference group). By exponentiating this coefficient we can obtain the odds ratio: $e^{-2.477825} = 0.083$, which is the same value we got before! What we can interpret from that is that the odds of a man surviving the sinking of the titanic were only roughly 8% than that for women!

2. We also obtain results from statistical tests for significance of coefficients. Namely, the test performed is the Wald test, which here indicates through a p <0.05 that gender is a useful predictor of the probability of surviving the Titanic sinking.

3. There are also outputs in this summary that relate to how well our model fits. These are:
  +(i) Deviance Residuals: These are not super useful, seeing as the logit model fitting being done through the maximum likelihood method means that residuals by themselves are not immediately useful as a measure of model fit. It is interesting, however, to make sure that these are roughly symmetric.
  +(ii) Null Deviance and Residual Deviance: These values can be used to statistically compare models. We can also use them to compute a pseudo $R^2$ that can help us better understand model comparisons, we will talk about this later but do note that this $R^2$ is fundamentally different than that for traditional linear regression.
  +(iii) Aikake Information Criterion(AIC): This is the residual deviance adjusted for the number of parameters in the model, and has a roughly analogous purpose to the ' Adjusted $R^2$' in a traditional linear regression.
  
We will talk more about model comparison once we have another model to actually compare to, so now we will proceed to our second model.

### Multiple-Predictors Model

Let us now make a more complex model that predicts the probability of survival of the sinking of the Titanic in relation to the variables of gender, age, and passenger class. Hopefully by now you have a more general understanding of both the model and how it relates to the concepts of probability, odds, and odds ratio, and so now I will be a bit more brief in discussing this model.

Our previous one-predictor model was using a categorical variable as a predictor, but now we will also include a continuous variable(Age). These predictor categories have important distinctions in how we interpret the output of our model so keep that in mind.

```{r}

log_Sex_Age_Class <- glm(Survived ~ Sex + Age + Pclass, family=binomial, data=df)
summary(log_Sex_Age_Class)

```
And one more time the output of our model

```
Call:
glm(formula = Survived ~ Sex + Age + Pclass, family = binomial, 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7303  -0.6780  -0.3953   0.6485   2.4657  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.777013   0.401123   9.416  < 2e-16 ***
Sexmale     -2.522781   0.207391 -12.164  < 2e-16 ***
Age         -0.036985   0.007656  -4.831 1.36e-06 ***
Pclass2     -1.309799   0.278066  -4.710 2.47e-06 ***
Pclass3     -2.580625   0.281442  -9.169  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 964.52  on 713  degrees of freedom
Residual deviance: 647.28  on 709  degrees of freedom
AIC: 657.28

Number of Fisher Scoring iterations: 5


```

In the output of this model we can once again see all of the elements present in our previous model, the main difference is that now we have additional coefficients in relation to our previous one. Notably, all of our predictors seem to be showing significant p values as useful predictors of odds of surviving the sinking! We already went over how to interpret categorical predictors when discussing the effect of gender on our model, and so we can similarly visualize how the coefficients for class behave in a similar manner. Note how the categorical predictor of class has more factor levels (two, as opposed to one, for gender) it should be kept in mind that these coefficients are in relation to the reference group (Pclass1). 

But what does the 'estimate' mean in the context of a continuous predictor, such as Age? In this case, the 'estimate' of Age is $-0.36985$, when we exponentiate this value we obtain the estimated odds ratio for age:$e^{-0.0369} = 0.96$ and it can be interpreted as: " for any increase in age by 1, that is, for every year older someone is, their odds ratio for survival decline by 0.034%.



But, what if we wanted to compare these last two models we have built?

### Comparing models

This is a bit more complicated in logistic models than in traditional linear models, chiefly due to the notable absence of a very intuitive and direct measure of fit in the form of the $R^2$ value.

One way of measuring the fit of a model is through a likelihood ratio test, which compares the measures of fit from our model from a null model (an intercept-only model):

```{r}

# We compare the deviance from the two models. Recall that both of these values were already present in the single-predictor model output

# We then calculate the degrees of freedom for the difference between the models is equal to the number of predictors used in the model.

# We can then obtain a p-value for the difference between these two models.

with(log_sex, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

```
This returns a value of:

```
[1] 2.020274e-48
```

In this case, our single-predictor model, with a p-value lower than 0.05, is a better fit to the data than an intercept-only model

A similar comparison between deviances can be done between two models, in what is known as a ' drop-in-deviance test', and will similarly help us define whether a given model is significantly more well-fit to the data than another.

```{r}
# Here we will perform a drop-in-deviance test comparing our single-predictor model with our two-predictor model

drop_in_dev <- anova(log_sex, log_Sex_Age_Class, test = "Chisq")
drop_in_dev

```
Which returns:
```
Analysis of Deviance Table

Model 1: Survived ~ Sex
Model 2: Survived ~ Sex + Age + Pclass
  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1       712     750.70                          
2       709     647.28  3   103.42 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

```

This comparison confirms what we had seen before, that with age and class being useful predictors of probability of survival, its inclusion in the model does improve it!

Another option for evaluating our model stems from calculating the confidence intervals for our model coefficients:

```{r}

exp(confint(log_Sex_Age_Class))

```
Returns:
```
Waiting for profiling to be done...
                  2.5 %     97.5 %
(Intercept) 20.37890724 98.3863313
Sexmale      0.05293643  0.1194848
Age          0.94905346  0.9780124
Pclass2      0.15515074  0.4621731
Pclass3      0.04299250  0.1297997

```

We can see here that while 1 is not part of the confidence interval for any of the coefficients, once again confirming the results we have been discussing so far!

Finally, although the logit modelling function in R doesn't provide us with an $R^2$ we can, however, calculate what we know as a 'pseudo-$R^2$' , that approximately fulfills the absence of a traditional $R^2$. However, since this is not part of the fundamental process of logit modelling, there are quite a few ways of doing so, mostly related to how we can achieve a similar result to what our interpretation of $R^2$ in a traditional linear model actually means.

1. Do we think of $R^2$ as explained variability?
2. As a measure of improvement from the null model to a fitted model?
3. As the square of the correlation between the model's fitted values and the actual values?

Depending on what your desired approach to thinking about $R^2$ is, there are different ways of manipulating the logit modelling output to obtain the answer you are looking for. 

Explaining all of the different approaches to pseudo-$R^2$ is beyond the scope of this assignment, and as such I will do so for only one, McFaden's pseudo-$R^2$, seeing as it seems to be the easiest one to interpret in relation to a traditional $R^2$ in linear modeling.

```{r}
# In this approach, we treat the log likelihood of the null model as a ' total sum of squares' and the log likelihood of the model as a 'sum of squared errors' in calculating the Rsqrd similarly to how we do it for traditional linear regression.

ll.null <- log_Sex_Age_Class$null.deviance/-2
ll.fitted <- log_Sex_Age_Class$deviance/-2

# We then obtain a value can generally be interpreted as total effect size for our predictors

(ll.null - ll.fitted)/ll.null

# Finally, it is possible to test the statistical significance of this R squared using a Chi-square distribution, not wholly unlike what we did for some of our other measurements of model fit

1-pchisq(2*(ll.fitted-ll.null),df=(length(log_Sex_Age_Class$coefficients)-1))

```
Returns:
```
[1] 0.3289037
[1] 0

```
Those being our pseudo-$R^2$ and the results of the statistical significance test (here a value so small it shows up as zero, so far below a p value of $0.05$).

## Part IV: Visualizing our model

There are many ways to plot the outputs of a logistic regression model. I will show only a couple of them. 

Unfortunately due to the nature of our data and predictors, we don't have a reliable continuous predictor with a strong effect size so as to achieve the classic ' sinusoidal' visualization of a fitted line, with our best predictors being the categorical predictors of gender and class. Therefore, I will instead plot two visualizations aiming at simply showing the performance of our model in predicting the actual fate of the Titanic's passengers survival.

Both of my plots will try to show the same thing, albeit in different ways, that being visualizing how good of a job our model is doing at predicting the survival of the passengers of the Titanic. An important component of these plots is the use of the MASS package to do a step wise modelling that allows me to save the predicted probability of survival for each passenger individually.

In the first plot, we have the Rank of the passengers' predicted probabilities of survival, with the estimated probability itself on the y axis. The color of the points represents the actual survival status of the passenger. This plot allows us to visualize if our model did a good job, and what were some outliers in the model that had very high or very low predicted probabilities of survival and yet ended up subjected an unlikely fate. 

```{r}

log_Sex_Age_Class_stepwise <- log_Sex_Age_Class %>% stepAIC(direction='both',trace = FALSE)

predicted.data <- data.frame(probability.of.survival = log_Sex_Age_Class_stepwise$fitted.value,Survived=df$Survived,Sex=df$Sex)
predicted.data <- predicted.data[order(predicted.data$probability.of.survival,decreasing = FALSE),]

predicted.data$Rank <- 1:nrow(predicted.data)
predicted.data$Survived_Color <- as.factor(predicted.data$Survived)

plot1 <- ggplot(data=predicted.data,aes(x=Rank,y=probability.of.survival))+
  geom_point(aes(color=Survived_Color),alpha=0.7,shape=1,stroke=1,size=2)+
  scale_color_manual(name = "Survived",
                     values = c("0" = "cyan",
                                  "1" = "magenta"),
                     labels = c("No", "Yes"))+
  xlab('Rank')+
  ylab('Predicted Probability of Survival')+
  theme_bw(base_size = 12)+
  theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5))+
  ggtitle('Model Performance')+
  coord_fixed(ratio = 1000)

```
{{< figure library="true" src="log_perform.png" >}}

As previously mentioned, both plots aim to achieve similar objectives, and as such the second plot also aims at visualizing our model performance. This time, we have the predicted probability of survival on the x axis, and the actual survival status on the y axis. In addition, this plot includes two sigmoid curves aiming to better visualize the predicted probabilities of survival by gender. With this plot we can now clearly see the visual representation of the drastic difference in survival for men vs. women that we had been seeing only in terms of statistics so far.

```{r}
plot2 <- ggplot(data=predicted.data,aes(x=probability.of.survival,y=Survived,col=Sex))+
  geom_point(alpha=0.7,shape=1,stroke=1)+
  xlab('Predicted probability of surviving the Titanic sinking')+
  ylab('Survived')+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE,fullrange = TRUE)+
  theme_bw(base_size = 12)+
  theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5))+
  ggtitle('Survival by Gender')

```
{{< figure library="true" src="log_perform_gender.png" >}}

## Wrap-up

Hopefully by now you have a good understanding of logistic regression models. This is in my opinion much less intuitive and a bit harder to approach than simple linear regression, but by going step by step in approaching these modelling techniques, I hope I was able to provide a gentle introduction to this topic.

{{< figure library="true" src="ponder.png" >}}

