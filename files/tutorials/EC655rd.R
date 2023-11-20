library(tidyverse)
library(magrittr)
library(ggthemes)
library(car)
library(lmtest)
library(sandwich)
library(vtable)
library(AER)
library(stargazer)




# (1) Sharp RD with Constant Treatment Effects

#In this simulation, we make treatment a function of some running variable
#s, but the effect of the treatment is the same at all values of s.  Recall
#That potential outcomes are a function of s, and so is w, so if you just 
#compare the observed value of y between people who get treated and people
#who don't, you will get bias.  Controlling for s, we identify this as
#the jump in y at the cutoff point.


# Create the data
data <- data.frame(eta=rnorm(1000,0,5)) %>%
  mutate(s = runif(1000,0,10),
         y0 = 2 + 5*s + eta,
         y1 = y0 + 10,
         w = if_else( s >= 5,1,0),
         y = y0 + (y1 - y0)*w)


#Plot the data
ggplot(data = data, aes(x = s, y = y)) +
  geom_point(alpha = .2) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Sharp RD with Equal Slopes") 
  

#Estimate parameters by OLS.  First regression is biased
#Second regression is unbiased
lm1 <- lm(y ~ w, data = data)
lm2 <- lm(y ~ w + s, data = data)
stargazer(lm1, lm2, type = "text")


#Plot the predicted values on top of the data
ggplot(data = data, aes(x = s, y = y)) +
  geom_point(alpha = .2) +
  geom_line(aes(y=predict(lm2)), color = "red", size = 1.5) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Sharp RD with Equal Slopes") 



# (2) Sharp RD with Treatment Effects that Vary Across s


#In this simulation, we make treatment a function of some running variable
#s, but now the treatment effect is different depending on s.  Recall
#That potential outcomes are a function of s, and so is w, so if you just 
#compare the observed value of y between people who get treated and people
#who don't, you will get bias.  Controlling for s, we identify this as
#the jump in y at the cutoff point.

#There are two key things to keep in mind here: 1) the slope of the observed
#y will be different on each side of the discontinuity, so we must allow for that
#in the regression by interacting s and w.  2) we need to measure the jump in y 
#at the cutoff point.  If you just run a regression with s, w, and s*w in there, it
#the coefficient on w will measure the shift in the intecept when s = 0.  This is not
#the actual treatment effect.  We want to measure the jump in the intercept when
#s = 5, so we need to recenter s so that it equals zero when s = 5. 


# Create new dataset
data1 <- data.frame(eta=rnorm(1000,0,5)) %>%
  mutate(s = runif(1000,0,10),
         y0 = 2 + 5*s + eta,
         y1 = y0 + 3*s,
         w = if_else( s >= 5,1,0),
         y = y0 + (y1 - y0)*w)  

#Treatment effect at the cutoff is E[y1 - y0 | s = 5] = 3*5 = 15

#Plot the data
ggplot(data = data1, aes(x = s, y = y)) +
  geom_point(alpha = .2) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
labs(title ="Regression Discontinuity with Unequal Slopes") 

#Estimate regression with unequal slopes
lm3 <- lm(y ~ w*s, data = data1)

#Plot regression on top of data
ggplot(data = data1, aes(x = s, y = y)) +
  geom_point(alpha = .2) + 
  geom_line(aes(y=predict(lm3)), color = "red", size = 1.5) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Regression Discontinuity with Unequal Slopes") 

#Create recentered value of s
data1 %<>% mutate(ms = s-5)

lm4 <- lm(y ~ w*ms, data = data1)

#Plot regression on top of data again
ggplot(data = data1, aes(x = ms, y = y)) +
  geom_point(alpha = .2) + 
  geom_line(aes(y=predict(lm4))) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(title ="Regression Discontinuity with Unequal Slopes") 

#Compare coefficients
stargazer(lm3, lm4, type = "text")



# (3) Sharp RD with Treatment Effects that Vary Across s and Nonlinearities


#Not all potential outcomes are linear.  Here we allow for non-linear outcomes
#which can be different for y0 and y1.  All of the caveats from (2) apply here,
#which are that we must allow for different slopes on each side of the cutoff,
#and we must recenter s so that it measures the intercept shift at s = 5.  Note
#that if we don't specify the nonlinearity correctly, we will get incorrect
#treatment effects.



data2 <- data.frame(eta=rnorm(1000,0,5)) %>%
  mutate(s = runif(1000,0,10),
         y0 = 2 + 3*s - 0.5*s^2 + 0.1*s^3 + eta,
         y1 = y0 -s - .3*s^2 +0.2*s^3,
         w = if_else( s >= 5,1,0),
         y = y0 + (y1 - y0)*w)  

#Treatment effect at the cutoff is E[y1 - y0 | s = 5] = -5 - .3*5^2 + 0.2*5^3 = 12.5

#Plot the data
ggplot(data = data2, aes(x = s, y = y)) +
  geom_point(alpha = .2) + 
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Sharp RD with Nonlinear Effects")


data2 %<>% mutate(ms = s-5, ms2 = ms^2, ms3 = ms^3)

lm5 <- lm(y ~ w*poly(ms,3,raw=TRUE), data = data2)
stargazer(lm5, type = "text")


ggplot(data = data2, aes(x = s, y = y)) +
  geom_point(alpha = .2) + 
  geom_line(aes(y=predict(lm5)), color = "red", size = 1.5) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(title ="Sharp RD with Nonlinear Effects")

#(4) Fuzzy RD 


#Sometimes people do not comply with their treatment status: they may be
#assigned to take the treatment when they're on one side of the cutoff, but
#then not take it, and vice versa.  Here we treat this as an instrumental
#variables setup, using the assignment to treatment (z) as an instrument for 
#actual treatment (w). Here we model w0 and w1 as potential treatment statuses
#with probability a function of s, the running variable.
  
#Create the data  
data3 <- data.frame(eta=rnorm(1000,0,5)) %>%
  mutate(s = runif(1000,0,10),
         y0 = 2 + 5*s + eta,
         y1 = y0 + 10,
         w0 = rbinom(1000,1,0.01*s),
         w1 = rbinom(1000,1,0.01*s + 0.6),
         z = if_else( s >= 5,1,0),
         w = w0 + (w1 - w0)*z,
         y = y0 + (y1 - y0)*w) 

#Plot first stage data
ggplot(data = data3, aes(x = s, y = w)) +
  geom_point(alpha = .2) + 
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Fuzzy RD First Stage")

#Estimate first stage
lm6 <- lm(w ~ z + s, data = data3)
stargazer(lm6, type = "text")

#Plot first stage with prediction
ggplot(data = data3, aes(x = s, y = w)) +
  geom_point(alpha = .2) + 
  geom_line(aes(y=predict(lm6)), color = "red", size = 1.5) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Fuzzy RD First Stage")


#Plot reduced form
ggplot(data = data3, aes(x = s, y = y)) +
  geom_point(alpha = .2) + 
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Fuzzy RD Reduced Form")

#Estimate reduced form
lm7 <- lm(y ~ z + s, data = data3)
stargazer(lm7, type = "text")

#Pl
ggplot(data = data3, aes(x = s, y = y)) +
  geom_point(alpha = .2) + 
  geom_line(aes(y=predict(lm7)), color = "red", size = 1.5) +
  theme_pander(nomargin=FALSE, boxes=TRUE) +
  geom_vline(xintercept = 5, linetype="dotted") +
  labs(title ="Fuzzy RD Reduced Form")


lm8<- ivreg(y ~ s + w |s + z , data = data3)

stargazer(lm6, lm7, lm8, type = "text")
