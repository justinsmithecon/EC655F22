library(tidyverse)
library(vtable)
library(plm)
library(magrittr)
library(stargazer)
library(modelsummary)
library(fixest)

# set seed
set.seed(12345)


#First create data so that w is assigned randomly
#All methods should estimate the treatment effect

create_panel_data <- function(N,T) {
  panel_data <- crossing(ID = 1:N, t = 1:T) %>%
    mutate(eta = rnorm(n(), mean=0, sd=10))
  
  indiv_data <- data.frame(ID = 1:N,
              a = rnorm(N,0,10))

  panel_data %<>%
    full_join(indiv_data, by = 'ID') %>%
    mutate(y0 = 2 + a + eta,
           y1 = y0 + 4,
           w = ifelse(runif(n()) > 0.5 & t >= 3, 1, 0),
           y = y0 + (y1 - y0)*w)

  return(panel_data)
}  
  



data <- create_panel_data(500,5)

# Declare data to be panel type
data %<>% pdata.frame(index = c("ID", "t"))

# Note all models use clustered errors

# pooled OLS
model1 <- feols(y ~ w, data = data, cluster = "ID")

# fixed effects
model2 <- feols(y ~ w | ID, data = data, cluster = "ID")

# dummy variable regression
model3 <- feols(y ~ w + factor(ID), data = data, cluster = "ID")

# first differencing
model4 <- plm(y ~ w , data = data, model="fd")
model4$vcov <- vcovCR(model4, cluster = "ID", type = "CR1S")

# random effects
model5 <- plm(y ~ w , data = data, model="random")
model4$vcov <- vcovCR(model4, cluster = "ID", type = "CR1S")



modelsummary(list("Pooled OLS" = model1, "FE" = model2, "DVR" = model3, "FD" = model4, "RE" = model5), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|F|se_type", coef_omit = "ID",
             stars = TRUE)




# Now assume people with high a get treatment and low a do not
# In this case the independent variable is correlated with the
# unobserved effect a.  Pooled OLS and Random Effects will be
# biased.  Fixed Effects, Dummy Variable Regression, and First
# Differencing will be unbiased.

create_panel_data2 <- function(N,T) {
  panel_data <- crossing(ID = 1:N, t = 1:T) %>%
    mutate(eta = rnorm(n(), mean=0, sd=10))
  
  indiv_data <- data.frame(ID = 1:N,
                           a = rnorm(N,0,10))
  
  panel_data %<>%
    full_join(indiv_data, by = 'ID') %>%
    mutate(y0 = 2 + a + eta,
           y1 = y0 + 4,
           w = ifelse(a > 15 & t >= 3, 1, 0),
           y = y0 + (y1 - y0)*w)
  
  return(panel_data)
}  


data <- create_panel_data2(500,5)
data %<>% pdata.frame(index = c("ID", "t"))


# pooled OLS
model1 <- feols(y ~ w, data = data, cluster = "ID")

# fixed effects
model2 <- feols(y ~ w | ID, data = data, cluster = "ID")

# dummy variable regression
model3 <- feols(y ~ w + factor(ID), data = data, cluster = "ID")

# first differencing
model4 <- plm(y ~ w , data = data, model="fd")
model4$vcov <- vcovCR(model4, cluster = "ID", type = "CR1S")

# random effects
model5 <- plm(y ~ w , data = data, model="random")
model4$vcov <- vcovCR(model4, cluster = "ID", type = "CR1S")



modelsummary(list("Pooled OLS" = model1, "FE" = model2, "DVR" = model3, "FD" = model4, "RE" = model5), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|F|se_type", coef_omit = "ID",
             stars = TRUE)



#Create w so that it is correlated with a and the time varying error
#In this case the assumption of strict exogeneity is violated and
#None of the methods work

create_panel_data3 <- function(N,T) {
  panel_data <- crossing(ID = 1:N, t = 1:T) %>%
    mutate(eta = rnorm(n(), mean=0, sd=10))
  
  indiv_data <- data.frame(ID = 1:N,
                           a = rnorm(N,0,10))
  
  panel_data %<>%
    full_join(indiv_data, by = 'ID') %>%
    mutate(y0 = 2 + a + eta,
           y1 = y0 + 4,
           w = ifelse(a > 15 & t >= 3 & eta>0, 1, 0),
           y = y0 + (y1 - y0)*w)
  
  return(panel_data)
}  


data <- create_panel_data3(500,5)
data %<>% pdata.frame(index = c("ID", "t"))

# pooled OLS
model1 <- feols(y ~ w, data = data, cluster = "ID")

# fixed effects
model2 <- feols(y ~ w | ID, data = data, cluster = "ID")

# dummy variable regression
model3 <- feols(y ~ w + factor(ID), data = data, cluster = "ID")

# first differencing
model4 <- plm(y ~ w , data = data, model="fd")
model4$vcov <- vcovCR(model4, cluster = "ID", type = "CR1S")

# random effects
model5 <- plm(y ~ w , data = data, model="random")
model4$vcov <- vcovCR(model4, cluster = "ID", type = "CR1S")



modelsummary(list("Pooled OLS" = model1, "FE" = model2, "DVR" = model3, "FD" = model4, "RE" = model5), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|F|se_type", coef_omit = "ID",
             stars = TRUE)


                                                     
                              