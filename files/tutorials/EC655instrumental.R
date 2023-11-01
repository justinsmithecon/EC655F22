library(tidyverse)
library(magrittr)
library(ggthemes)
library(car)
library(lmtest)
library(sandwich)
library(vtable)
library(AER)
library(stargazer)
library(estimatr)

set.seed(9810078)

#Create data with constant treatment effects.  In this setup
#we also create a potential treatment, which is what people
#do in the event they are assigned to treatment (w1) and when
#they are not (w0).  The key thing to remember is that people
#assigned to treatment might not actually take it (w1 = 0),
#and people not assigned to treatment might take it (w0 = 1).
#Below, it is set up so that everyone who is assigned to treatment
#actually takes it, and people not assigned to treatment take it
#when y0 < 1.  Assignment to treatment (z) is completely random.


data <- data.frame(eta=rnorm(100000,0,1)) %>%
  mutate(y0 = 2 + eta,
         y1 = y0 + 5,
         w0 = if_else(y0 < 1,1,0),
         w1 = 1,
         z = if_else(runif(100000) > .5,1,0),
         w = w0 + (w1 - w0)*z,
         y = y0 + (y1 - y0)*w)


#Is mean independence satisfied? No.  Table below shows that neither
#the mean of y0 or y1 is constant between the treatment and control 
#groups.  So we cannot just take the difference in observed outcomes
#to estimate the treatment effect (which we know = 5)

sumtable(data, summ=c('notNA(x)','mean(x)'), group = 'w')

ggplot(data, aes(x=y0, color=as.factor(w))) +
  geom_density(alpha = .4, size=2) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  labs(title = "Distribution of Y0")   

ggplot(data, aes(x=y1, color=as.factor(w))) +
  geom_density(alpha = .4, size=2) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  labs(title = "Distribution of Y1")  

ols <- lm(y ~ w, data = data)
stargazer(ols, type = "text")

#Remember though that z IS randomly assigned.  We
#can see that by looking at the potential outcomes
#across the two values of z

sumtable(data, summ=c('notNA(x)','mean(x)'), group = 'z')

ggplot(data, aes(x=y0, color=as.factor(z))) +
  geom_density(alpha = .4, size=2) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  labs(title = "Distribution of Y0")   

ggplot(data, aes(x=y1, color=as.factor(z))) +
  geom_density(alpha = .4, size=2) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  labs(title = "Distribution of Y1") 



#Because z is randomly assigned, it is by definition unrelated to 
#the model error and we have designed it to be related to the endogenous
#variable.  In this case, it should work out that
#estimating TSLS gives us the treatment effect.
#In R, there are several packages that you can use: AER, ivreg,
#and estimatr. We will use ivreg.

iv <- ivreg(y ~ w | z, data = data)
summary(iv)


#An alternative to stargazer is modelsummary, which handles
#more types of models.

modelsummary(list("OLS" = ols, "TSLS" = iv), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type",
             stars = TRUE, metrics = "all", vcov = "HC1")




#It is common to also produce the first stage and reduced form
#for the model.  You can run those separately

fs <- lm(w ~ z, data = data)
rf <- lm(y ~ z, data = data)

modelsummary(list("OLS" = ols, "TSLS" = iv, "First Stage" = fs,
                  "Reduced Form" = rf), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type",
             stars = TRUE, metrics = "all", vcov = "HC1")



#Now pretend that the treatment effects are not constant.  
#Below we set it up so that average treatment effect is
#5, but it varies across the population.  Here we again
#set this up so that there are no defiers, and monotonicity
#is satisfied

data1 <- data.frame(y0 = 5 + rnorm(100000,0,3)) %>%
  mutate(y1 = 10 + rnorm(100000,0,3),         
         w0 = if_else(y1 - y0 >= 7,1,0),
         w1 = 1,
         z = if_else(runif(100000) > .5,1,0),
         w = w0 + (w1 - w0)*z,
         y = y0 + (y1 - y0)*w,
         treat = y1-y0)


#compute various treatment effects

#ATE

sumtable(data1,vars='treat', summ=c('notNA(x)','mean(x)'))

#LATE for compliers

sumtable(filter(data1, w1 == 1 & w0 == 0),vars='treat', summ=c('notNA(x)','mean(x)'))

#Treatment for always-takers

sumtable(filter(data1, w1 == 1 & w0 == 1),vars='treat', summ=c('notNA(x)','mean(x)'))

#If you run two-stage least squares, it produces the LATE for compliers

iv2<-ivreg(y ~ w | z, data = data1)

modelsummary(list("TSLS" = iv2), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type",
             stars = TRUE, metrics = "all", vcov = "HC1")

#What if there are defiers?
#Set up so that treatment effects for compliers and defiers is roughly equal
#reduced form is near zero, and two-stage least squares is biased


data2 <- data.frame(y0 = 5 + rnorm(100000,0,3)) %>%
  mutate(y1 = 10 + rnorm(100000,0,3))

lq <- summary(data2$y1 -data2$y0)[2]
uq <- summary(data2$y1 -data2$y0)[5]

data2 %<>% mutate(w0 = if_else(y1-y0 >=uq | y1-y0 <= lq,1,0),
         w1 = if_else(y1-y0 >= uq | y1-y0 <= lq,0,1),
         z = if_else(runif(100000) > .5,1,0),
         w = w0 + (w1 - w0)*z,
         y = y0 + (y1 - y0)*w,
         treat = y1-y0,
         comply = if_else(w1 == 1 & w0 == 0,1,0))



#LATE for compliers

sumtable(filter(data2, w1 == 1 & w0 == 0),vars='treat', summ=c('notNA(x)','mean(x)'))

#LATE for defiers

sumtable(filter(data2, w1 == 0 & w0 == 1),vars='treat', summ=c('notNA(x)','mean(x)'))

#Reduced form equals zero because effect on compliers and defiers cancels

rf3 <- lm(y ~ z, data = data2)
fs3 <- lm(w ~ z, data = data2)
iv3<- ivreg(y ~ w | z, data = data2)

modelsummary(list("TSLS" = iv3, "First Stage" = fs3,
                  "Reduced Form" = rf3), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type",
             stars = TRUE, metrics = "all", vcov = "HC1")