### Title:    Introduction to R 5: Data Analyses
### Author:   Kyle M. Lang
### Created:  2016-01-27
### Modified: 2022-01-29

rm(list = ls(all = TRUE))

library(psych)
library(multcomp)
library(rockchalk)

dataDir <- "../data/"

source("4_data_manipulation-888.R")

bfiItems <- bfi %>% select(matches("[A-Z]\\d$|sex|ed|gm|age$"))
bfi      <- bfi %>% select(-matches("^[A-Z]\\d|age_mc|age_std"))

head(bfi)
head(bfiItems)

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
bfi %>% filter(sex == "male") %>%
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
bfi %>% select(matches("^[a-z]{4,5}")) %>% colMeans()

## Find the number of missing values in each column
colSums(is.na(bfi))
bfi %>% is.na() %>% colSums()

## Find the proportion of complete cases
tmp <- bfi %>% is.na() %>% rowSums()
mean(tmp == 0)

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

## Get the column numbers for the scale scores
scaleNames <- grep("^[a-z]{4,5}", colnames(bfi), value = TRUE)

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
tapply(bfi$age, bfi$ed, median)
with(bfi, tapply(age, ed, median))

################################################################################
## PRACTICE PROBLEM 5.4
##
## Use the tapply() function to compute the average neuroticism value for minors
## and for adults.
##
################################################################################

### For multivariate summaries, we can use the aggregate() function.

## Compute the variances of each scale score for males and females
aggregate(bfi[scaleNames], bfi["sex"], var)

## Compute the means each scale score for all sex X education groups
aggregate(bfi[scaleNames], bfi[c("sex", "ed")], mean)

################################################################################
## PRACTICE PROBLEM 5.5
##
## Use the aggregate function to compute SDs for 'extra', 'agree', and 'open'
## within education groups.
##
################################################################################

### If we want to use dplyr functions, we can combine the group_by() and
### summarise() function to get whatever flavor of aggregation we like

## Compute the 5th and 95th percentiles of age for each sex X education group
bfi %>%
    group_by(sex, ed) %>%
    summarize(age05 = quantile(age, 0.05), age95 = quantile(age, 0.95))

### We can use the across() funtion to broadcast univariate functions across
### multiple columns within summarise()

bfi %>%
    group_by(sex) %>%
    summarise(across(c("age", "agree", "extra"), var))

bfi %>%
    group_by(sex) %>%
    summarise(across(.fns = class))

bfi %>%
    group_by(ed) %>%
    summarise(across(matches("^[a-z]{4,5}"), list(mean = mean, sd = sd)))

bfi %>%
    group_by(sex, ed) %>%
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

## Calcualte the covariance matrix of the scale scores for males
bfi %>%
    filter(sex == "male") %>%
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
bfiItems %>%
    select(matches("^a\\d")) %>%
    alpha(check.keys = TRUE)

## Use the psych::alpha() function to compute Cronbach's Alpha for the
## opennes scale and include boostrapped confidence intervals:
bfiItems %>%
    select(matches("^o\\d")) %>%
    alpha(n.iter = 1000, check.keys = TRUE)

################################################################################
## PRACTICE PROBLEM 5.8
##
## Compute the internal consistency of the neuroticism scale for adult males.
## - Use the set.seed() function to set the random number seed to 314159.
## - Use 2000 bootstrap samples to estimate confidence intervals for the
##   internal consistency.
## - According to the bootstrap inference, is the internal consistency
##   significanlty different from 0.8?
##
################################################################################

## Use the psych::skew() psych::kurtosi() functions to calculate the skew and
## excess kurtosis of the scale scores
skewVec <- sapply(bfi[scaleNames], skew)
kurtVec <- sapply(bfi[scaleNames], kurtosi)

skewVec
kurtVec

## Any really problematic varibles?
any(abs(skewVec) > 1.0)
any(abs(kurtVec) > 7.0)


###-Simple Statistical Tests-------------------------------------------------###

### We can use the t.test() function to do bivariate t-tests

## Do the average levels of agreeableness differ significantly from the average
## levels of extraversion within people?
with(bfi, t.test(agree, extra, paired = TRUE))

################################################################################
## PRACTICE PROBLEM 5.9
##
## Use an exposition pipe to replicate the above t.test
##
################################################################################

## Are men significantly more open to new experiences than women?
t.test(open ~ sex, data = bfi, alternative = "greater")

### Earlier, we calculated bivariate correlations, but we did not test their
### significance. We can use the cor.test() function to do significance testing
### for correlations.

## Is there a significant correlation between age and neuroticism for women?
bfi %>%
    filter(sex == "female") %$%
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

## Is there an association between sex and educational attainment?
out <- bfi %$% table(sex, ed) %>% chisq.test()

## Where do the differences lie?
out$stdres


###-Linear Modeling----------------------------------------------------------###

### We use the lm() function to fit linear models

## After controlling for age, is there a difference between men and women in
## levels of neuroticism?

fit1 <- lm(neuro ~ age + sex, data = bfi)
summary(fit1)

## Does this difference remain after controlling for the other four personality
## scales?

fit2 <- update(fit1, ". ~ . + agree + extra + open + consc")
summary(mod2)

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

## Is there a differential effect of age on neuroticism for men an women, after
## controlling for the other personality dimensions?
fit3 <- update(fit2, ". ~ . + age * sex")
summary(fit3)

################################################################################
## PRACTICE PROBLEM 5.12
##
## Modify the model you fit in (5.11) to test if age moderates the effect of
## openness on agreeableness or the effect of extraversion on agreeableness,
## after controlling for educational attainment.
## - Does age moderate either of the focal effects?
## - If you find significant moderation, how does age affect the focal effects?
## - How much more variability in agreeableness have you explained by modifying
##   the model?
## - Is the additional explained variation significant?
##
################################################################################

### We can use the rockchalk::plotSlopes() and rockchalk::testSlopes() function
### to visualize and probe this interaction

## Calculate and visualize the simple slopes of age on neuroticism for each sex
psOut <- plotSlopes(fit3, plotx = "age", modx = "sex")

## Test the simple slopes for significance:
testSlopes(psOut)

### Before we place too much trust in these results, we need to evaluate the
### tenability of the assumptions

## Generate diagnostic plots for our final model
plot(fit3)

## Put all plots on a single canvas
par(mfrow = c(2, 2))
plot(fit3)

################################################################################
## PRACTICE PROBLEM 5.13
##
## (a) Use the rockchalk::plotSlopes() function to visualize the simple slopes
##     for one of the interactions you estimated in (5.12).
##     - Define the simple slopes at the mean of the moderator and one SD above
##       and below the mean of the moderator.
## (b) Use the rockchalk::testSlopes() function to test the simple slopes you
##     estimated in (a) for significance.
##     - Are any of the simple slopes significant?
##     - Interpret any significant simple slopes.
##
################################################################################


###-ANOVA--------------------------------------------------------------------###

### Since ANOVA is just another flavor of the general linear model, we also use
### the lm() function to do ANOVAs; we just summarize the results differently

## One-way ANOVA
fit1 <- lm(extra ~ ed, data = bfi)
summary.aov(fit1)

## ANCOVA
fit2 <- lm(extra ~ ed + age, data = bfi)
summary.aov(fit2)

## Factorial ANCOVA
fit3 <- lm(extra ~ ed * sex + age, data = bfi)
summary.aov(fit3)

## Two-way ANCOVA
fit4 <- lm(extra ~ ed + sex + age, data = bfi)
summary.aov(fit4)

## Check assumptions graphically
par(mfrow = c(2, 2))
plot(fit4)

################################################################################
## PRACTICE PROBLEM 5.14
##
## Estimate a one-way ANCOVA to test if there is a difference in mean openness
## between adults with different levels of educational attainment after
## controlling for age, extraversion, and agreeableness
## - Is the hypothesis supported?
## - Do all predictors combined explain a significant proportion of variability
##   in openness?
##
################################################################################


###-Post Hoc Test------------------------------------------------------------###

### To evaluate hypotheses about specific effects (or between-group differences),
### we use the summary.lm() function instead of the summary.aov() function

## By default factors are coded as dummy codes. So, we'll see test of mean
## differences from the reference groups
summary(fit4)

### We can manipulte the contrast attribute of the independent factors to test
### different comparisons

## Check the defaults:
contrasts(bfi$sex)
levels(bfi$sex)

contrasts(bfi$ed)
levels(bfi$ed)

### We can implement unweighted effects coding by changing the contrast
### attribute to a contr.sum() object

## Change the contrast type for the education factor
newCt <- contr.sum(levels(dragonFeed$diet))
contrasts(bfi$ed) <- contr.sum(levels(bfi$ed))

## For some reason, we don't get names for our contrasts. So, if we want easily
## interpretted output, we should add the names ourselves
colnames(contrasts(bfi$ed)) <- head(levels(bfi$ed), 4)

## Re-estimate the model using the updated factor
fit4.2 <- update(fit4, data = bfi)
summary(fit4.2)

### The glht() function from the 'multcomp' package supports arbitrary posthoc
### comparisons with corrections for multiple testing.

## We can use the glht() function to test all pairwise comparisons with Tukey's
## HSD correction
hsdOut <- glht(fit4, linfct = mcp(ed = "Tukey", sex = "Tukey"))
summary(hsdOut)

## If we don't have any continous covariates we can use the base R TukeyHSD()
## function, but we have to estimate the model using aov()
fit1.2 <- aov(formula(fit1), data = bfi)
TukeyHSD(fit1.2)

################################################################################
## PRACTICE PROBLEM 5.15
##
## Use Tukey's HSD to test all pairwise differences between the different levels
## of educational attaiment from the model you estimated in (5.14).
## - Are any of the groups significanlty different in their mean levels of
##   openness? If so, which?
##
################################################################################


###-END----------------------------------------------------------------------###
