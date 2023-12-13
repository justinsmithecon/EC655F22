# Load necessary libraries
library(MASS)
library(lmtest)
library(tidyverse)
library(modelsummary)
library(fixest)
library(ggthemes)
library(margins)

# Set seed for reproducibility
set.seed(12345)

# Simulate data

data <- tibble(eta = rnorm(500000, mean = 0, sd = 10),
               y0 = rbinom(500000, 1, 0.2),
               y1 = rbinom(500000, 1, 0.5),
               w = ifelse(runif(500000) > 0.5, 1, 0),
               y = y0 + (y1 - y0) * w,
               treat = y1 - y0)


# Linear Probability Model
ols_model <- feols(y ~ w, data = data, vcov = "HC1")


modelsummary(list("OLS" = linear_model, 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|F|se_type",
             stars = TRUE))

# Probit Model
probit_model <- glm(y ~ w, family = binomial(link = "probit"), data = data)
probit_me <- margins(probit_model)


modelsummary(list("OLS" = ols_model, "Probit" = probit_model,"Probit ME" = probit_me), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|F|se_type",
             stars = TRUE)

# Logistic Model

data2 <- tibble(eta= rlogis(500000, location = 0, scale = 10 * sqrt(3) / pi),
               y0 = rbinom(500000, 1, 0.2),
               y1 = rbinom(500000, 1, 0.5),
               w = ifelse(runif(500000) > 0.5, 1, 0),
               y = y0 + (y1 - y0) * w,
               treat = y1 - y0)


# Linear Probability Model
ols_model <- feols(y ~ w, data = data2, vcov = "HC1")

# Logit
logit_model <- glm(y ~ w, family = binomial(link = "logit"), data = data2)
logit_me <- margins(logit_model)

modelsummary(list("OLS" = ols_model, "Logit" = logit_model,"Logit ME" = logit_me), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|F|se_type",
             stars = TRUE)



# Hypothesis tests


# LR test
logit_model_w <- glm(y ~ w, family = binomial(link = "logit"))
logit_model_y <- glm(y ~ 1, family = binomial(link = "logit"))

lr_test <- lrtest(logit_model_w, logit_model_y)
print(lr_test)

# Wald test
wald_test <- anova(logit_model_w, test = "Chisq")
print(wald_test)

# LM test
constraint_matrix <- matrix(0, ncol = 2, nrow = n)
constraint_matrix[, 2] <- w
colnames(constraint_matrix) <- c("Intercept", "w")
lm_test <- lrtest(logit_model_w, constraint_matrix)
print(lm_test)
