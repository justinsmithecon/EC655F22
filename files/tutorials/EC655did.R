library(tidyverse)
library(causaldata)
library(modelsummary)
library(magrittr)
library(estimatr)
library(sandwich)
library(fixest)


# load data from the causaldata package
data <- organ_donations

# In this data, the treatment is that California adopted active-choice
# phrasing for their organ donations in the third quarter of 2011 and 
# forward.  We will consider California the treated group, and the
# treated time period is Q32011 and later.  Now we identify this in the
# data

data %<>% mutate(treat = if_else(State == "California", 1, 0),
                 after = if_else(Quarter %in% c('Q32011','Q42011','Q12012'), 1, 0))


# The most basic difference in differences is to regress the outcome
# on a dummy for being the treated group, a dummy for the treated time
# and the interaction between the two

lm1 <- lm(Rate ~ treat*after, data = data)

modelsummary(list("DiD Simple" = lm1), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type",
             stars = TRUE, vcov = "HC1")


# We know that we can also allow for separate dummy variables for each
# state, and for each time period.  

lm2 <- lm(Rate ~ treat:after + factor(State) + factor(Quarter), data = data)

modelsummary(list("DiD Simple" = lm1, "DiD Separate" = lm2), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type",
             stars = TRUE, vcov = "HC1")

# A second way to do the same thing is to use feols. This estimates the
# exact same coefficient as lm2, but does not put individual dummies in the
# regression, and instead does another transformation (that we will learn
# in the panel data unit) to get the same result.  It does not report the
# individual dummies for place and quarter.

lm3 <- feols(Rate ~ treat:after | State + Quarter, data = data)

modelsummary(list("DiD Simple" = lm1, "DiD 2" = lm3), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type|State|Quarter",
             stars = TRUE, vcov = "HC1")


# We can allow the treatment effect to differ across time by interacting the
# california dummy separately with all the post-treatment time periods

data %<>% mutate(after1 = if_else(Quarter %in% c('Q32011'), 1, 0),
                 after2 = if_else(Quarter %in% c('Q42011'), 1, 0),
                 after3 = if_else(Quarter %in% c('Q12012'), 1, 0))

lm4 <- feols(Rate ~ treat:after1 + treat:after2 + treat:after3 | State + Quarter, data = data)

modelsummary(list("DiD Simple" = lm1, "DiD 2" = lm3, "DiD 3" = lm4), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type|State|Quarter",
             stars = TRUE, vcov = "HC1")


# Finally, we can allow an interaction between all time periods and the treatment
# variable to see if there are any visible effects before the treatment actually
# happens


data %<>% mutate(Quarterfac = factor(Quarter, levels = unique(Quarter)))

lm5 <- feols(Rate ~ i(Quarterfac, treat, "Q42010") | State + Quarter, data = data)

modelsummary(list("DiD Simple" = lm1, "DiD 2" = lm3, "DiD 3" = lm4, "Did 4" = lm5), 
             gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type|State|Quarter",
             stars = TRUE, vcov = "HC1")

# You can visualize this nicely with the coefplot function

iplot(lm5, pt.join = TRUE)