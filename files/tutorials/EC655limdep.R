# Load required libraries
library(tidyverse)
library(modelsummary)
library(fixest)
library(ggthemes)
library(censReg)

# Set seed for reproducibility
set.seed(12345)

# Create a data frame with 500000 observations
data <- tibble(eta = rnorm(50000, mean = 0, sd = 10),
               y0star = 2 + eta,
               y0 = ifelse(y0star > 0, y0star, 0),
               y1star = y0star + 5,
               y1 = ifelse(y1star > 0, y1star, 0),
               w = ifelse(runif(50000) > 0.5, 1, 0),
               y = y0 + (y1 - y0) * w,
               treat = y1 - y0,
               treatstar = y1star - y0star)


# Examine the data

ggplot(data, aes(x = y)) +
  geom_histogram(bins = 100) +
  labs(x = "y", y = "Count") + 
  theme_pander(nomargin = FALSE) 

# First estimate by OLS
ols_model <- feols(y ~ w, data = data, vcov = "HC1")

modelsummary(list("OLS" = ols_model), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|F|se_type",
             stars = TRUE)

# Estimate the Tobit model
tobit_model <- censReg(y ~ w, data = data)


modelsummary(list("OLS" = ols_model, "Tobit" = tobit_model), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|F|se_type",
             stars = TRUE)

# Marginal Effects (effect of treatment on the mean of y)

tobitmf <- margEff(tobit_model, calcVCov = TRUE)
summary(tobitmf)


# Conditional on Positive

ols_cop <- feols(y ~ w, data = filter(data, y>0), vcov = "HC1")

modelsummary(list("OLS" = ols_model, "Tobit" = tobit_model, "OLS COP" = ols_cop), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|F|se_type",
             stars = TRUE)
