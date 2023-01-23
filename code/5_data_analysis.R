### Title:    Introduction to R 5: Data Analyses
### Author:   Kyle M. Lang
### Created:  2016-01-27
### Modified: 2023-01-23

rm(list = ls(all = TRUE))

library(psych)
library(dplyr)
library(magrittr)

bfi <- readRDS(paste0("data/bfi.rds"))


###-Summary------------------------------------------------------------------###

## The base R summary() and str() functions give high-level summaries of R
## objects
summary(bfi)
str(bfi)

### To calculate more pointed summary information, the dplyr::summarise()
### function can be useful.

## Compute the means and standard devaitions of agreeableness and neuroticism
bfi %>% summarise(mA = mean(agree),
                  sdA = sd(agree),
                  mN = mean(neuro),
                  sdN = sd(neuro)
                  )

### We can also combine filter() and summarise() to get summary information for
### subgroups

## Compute the same summary stats as above for only males
bfi %>% filter(gender == "male") %>%
    summarise(mA = mean(agree),
              sdA = sd(agree),
              mN = mean(neuro),
              sdN = sd(neuro)
              )

################################################################################
## PRACTICE PROBLEM 5.1
##
## Use dplyr functions to compute the mean, variance, and range of 'age' for
## females in the 'bfi' data.
##
################################################################################

### The base R rowMeans(), rowSums(), colMeans(), and colSums() functions are
### very useful.

## Compute the means of the scale scores
bfi %>% select(matches("^[a-z]{4,5}$")) %>% colMeans()

## Find the number of missing values in each column
colSums(is.na(bfi))
bfi %>% is.na() %>% colSums()

## Find the proportion of complete cases
(bfi %>% is.na() %>% rowSums() == 0) %>% mean()

################################################################################
## PRACTICE PROBLEM 5.2
##
## Create a logical vector with one entry for every variable in the 'bfi' data.
## This vector should take the value TRUE when males have a higher proportion of
## missing data on that variable than females do.
##
################################################################################

### Unforunately, there are no multivariate analogues of functions like var(),
### sd(), median(), min(), max(), etc. Thankfully, this isn't really a problem,
### because we can easily broadcast any function that takes a single vector as
### input using "apply" functions.

## Get the column names for the scale scores
scaleNames <- grep("^[a-z]{4,5}$", colnames(bfi), value = TRUE)

## Use apply() to calculate the medians of every scale score
apply(bfi[scaleNames], 2, median)

## Do the same as above, but treat the data frame as a list
lapply(bfi[scaleNames], median)

## Do the same as above, but return the result in the simplest format
sapply(bfi[scaleNames], median)

## Find the maximum scale score value in each row
apply(bfi[scaleNames], 1, max)

## Find the column number containing the maximum scale score value in each row
apply(bfi[scaleNames], 1, which.max)

bfi %>% select(matches("^[a-z]{4,5}$")) %>% apply(1, which.max)

################################################################################
## PRACTICE PROBLEM 5.3
##
## Use an appropriate apply function to create a vector containing the variances
## of all numeric variables in the 'bfi' data.
##
################################################################################


###-Aggregation--------------------------------------------------------------###

### We can also aggregate the data according to some summary statistic within
### the groups defined by a grouping factor (or multiple factors)

### For univariate summaries, we can use the base R tapply() function.

## Compute the median age for each educational group
tapply(bfi$age, bfi$education, median)
with(bfi, tapply(age, education, median))
bfi %$% tapply(age, education, median)

################################################################################
## PRACTICE PROBLEM 5.4
##
## Use the tapply() function to compute the average neuroticism value for minors
## and for adults.
##
################################################################################

### For multivariate summaries, we can use the aggregate() function.

## Compute the variances of each scale score for males and females
aggregate(bfi[scaleNames], bfi["gender"], var)

## Compute the means each scale score for all sex X education groups
aggregate(bfi[scaleNames], bfi[c("gender", "education")], mean)

################################################################################
## PRACTICE PROBLEM 5.5
##
## Use the aggregate function to compute SDs for 'extra', 'agree', and 'open'
## within education groups.
##
################################################################################

### If we want to use dplyr functions, we can combine the group_by() and
### summarise() function to get whatever flavor of aggregation we like

## Compute the 5th and 95th percentiles of age for each gender X education group
bfi %>%
    group_by(gender, education) %>%
    summarize(age05 = quantile(age, 0.05), age95 = quantile(age, 0.95))

### We can use the across() funtion to broadcast univariate functions across
### multiple columns within summarise()

bfi %>%
    group_by(gender) %>%
    summarise(across(c("age", "agree", "extra"), var))

bfi %>%
    group_by(gender) %>%
    summarise(across(.fns = class))

bfi %>%
    group_by(education) %>%
    summarise(across(matches("^[a-z]{4,5}$"), list(mean = mean, sd = sd)))

bfi %>%
    group_by(education) %>%
    summarise(across(all_of(scaleNames), list(mean = mean, sd = sd)))

bfi %>%
    group_by(gender, education) %>%
    summarise(across(where(is.factor), nlevels))

################################################################################
## PRACTICE PROBLEM 5.6
##
## Use dplyr functions to compute the means, medians, and variances of all
## numeric variables in the 'bfi' data.
##
################################################################################


###-More Descriptive Statistics----------------------------------------------###

## Calculate the correlation matrix of the scale scores
cor(bfi[scaleNames])                      # use Pearson's rho
cor(bfi[scaleNames], method = "spearman") # use Spearman's rho
cor(bfi[scaleNames], method = "kendall")  # use Kendall's tau

## Calculate covariance matrix of the scale scores
cov(bfi[scaleNames])

## Calculate the covariance matrix of the scale scores for males
bfi %>%
    filter(gender == "male") %>%
    select(all_of(scaleNames)) %>%
    cov()


################################################################################
## PRACTICE PROBLEM 5.7
##
## Create a pipeline to compute the correlation matrix of all numeric variables
## in the 'bfi' dataset.
## - Use Spearman's rho for the correlations.
## - Use only those participants whose level of educational attainment includes,
##   at least, graduating from college.
##
################################################################################

### The 'psych' package has quite a few handy functions for psychometric style
### analyses

## Use the psych::alpha() function to compute Cronbach's Alpha for the
## agreeableness scale:
bfi %>%
    select(matches("^A\\d")) %>%
    alpha(check.keys = TRUE)

## Use the psych::alpha() function to compute Cronbach's Alpha for the
## opennes scale and include boostrapped confidence intervals:
bfi %>%
    select(matches("^O\\d")) %>%
    alpha(n.iter = 1000, check.keys = TRUE)

################################################################################
## PRACTICE PROBLEM 5.8
##
## Compute the internal consistency of the neuroticism scale for adult males.
## - Use the set.seed() function to set the random number seed to 314159.
## - Use 2000 bootstrap samples to estimate confidence intervals for the
##   internal consistency.
## - According to the bootstrap inference, is the internal consistency
##   significantly different from 0.8?
##
################################################################################

## Use the psych::skew() psych::kurtosi() functions to calculate the skew and
## excess kurtosis of the scale scores
skewVec <- bfi %>% select(all_of(scaleNames)) %>% sapply(skew)
kurtVec <- sapply(bfi[scaleNames], kurtosi)

skewVec
kurtVec

## Any really problematic variables?
any(abs(skewVec) > 1.0)
any(abs(kurtVec) > 7.0)


###-Simple Statistical Tests-------------------------------------------------###

### We can use the t.test() function to do bivariate t-tests

## Do the average levels of agreeableness differ significantly from the average
## levels of extraversion within people?
bfi %$% t.test(agree, extra, paired = TRUE)

################################################################################
## PRACTICE PROBLEM 5.9
##
## Use an exposition pipe to replicate the above t.test
##
################################################################################

## Are men significantly more open to new experiences than women?
t.test(open ~ gender, data = bfi, alternative = "greater")

### Earlier, we calculated bivariate correlations, but we did not test their
### significance. We can use the cor.test() function to do significance testing
### for correlations.

## Is there a significant correlation between age and neuroticism for women?
bfi %>%
    filter(gender == "female") %$%
    cor.test(neuro, age)

## Is there a negative correlation between agreeableness and neuroticism?
bfi %$% cor.test(agree, neuro, alternative = "less")

################################################################################
## PRACTICE PROBLEM 5.10
##
## Test for a positive correlation between agreeableness and openness in people
## younger than 30.
##
################################################################################

### We can use the chisq.test() function to do a Pearson's Chi-Squared test for
### independence between two factors.

## Is there an association between gender and educational attainment?
(out <- bfi %$% table(gender, education) %>% chisq.test())

## Where do the differences lie?
out$stdres


###-Linear Modeling----------------------------------------------------------###

### We use the lm() function to fit linear models

## After controlling for age, is there a difference between men and women in
## levels of neuroticism?

fit1 <- lm(neuro ~ age + gender, data = bfi)
summary(fit1)

## Does this difference remain after controlling for the other four personality
## scales?

fit2 <- lm(neuro ~ age + gender + agree + extra + open + consc, data = bfi)
summary(fit2)

################################################################################
## PRACTICE PROBLEM 5.11
##
## Use the full 'bfi' dataset to estimate a linear regression model to test if
## openness predicts agreeableness after controlling for extraversion, age, and
## educational attainment.
## - Is the hypothesis supported?
## - What proportion of variability in agreeableness is explained by the
##   predictors?
##
################################################################################

## How much additional variation in neuroticism is explained by adding the four
## extra scales?
summary(fit2)$r.squared - summary(fit1)$r.squared

## Is that increase in variance explained significant?
anova(fit1, fit2)

## We can also do model comparisons in terms of information criteria
AIC(fit1, fit2)
BIC(fit1, fit2)


###-END----------------------------------------------------------------------###
