### Title:    Introduction to R: Suggested Solutions for Extra Practice Problems
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2025-01-18

rm(list = ls(all = TRUE))

dataDir <- "data/"

library(dplyr)
library(magrittr)


################################################################################
### e1: Data Analysis                                                        ###
################################################################################

###-e1.1---------------------------------------------------------------------###

## Use dplyr functions to compute the mean, variance, and range of 'age' for
## females in the 'bfi' data.

bfi <- readRDS(paste0(dataDir, "bfi.rds"))

bfi %>%
    filter(gender == "female") %>%
    reframe(age_mean = mean(age), age_var = var(age), age_range = range(age))

## NOTE: We use reframe() instead of summarise() because our result will include
##       more than one row per group.


###-e1.2---------------------------------------------------------------------###

## Create a logical vector with one entry for every variable in the 'bfi' data.
## This vector should take the value TRUE when males have a higher proportion of
## missing data on that variable than females do.

male   <- bfi %>% filter(gender == "male") %>% is.na() %>% colMeans()
female <- bfi %>% filter(gender == "female") %>% is.na() %>% colMeans()

male > female


###-e1.3---------------------------------------------------------------------###

## Use an appropriate apply function to create a vector containing the variances
## of all numeric variables in the 'bfi' data.

bfi %>% select(where(is.numeric)) %>% sapply(var, na.rm = TRUE)


###-e1.4---------------------------------------------------------------------###

## Use the tapply() function to compute the average neuroticism value for minors
## and for adults.

bfi %$% tapply(neuro, age < 18, mean)


###-e1.5---------------------------------------------------------------------###

## Use the aggregate function to compute SDs for 'extra', 'agree', and 'open'
## within education groups.

bfi %>%
    select(extra, agree, open) %>%
    aggregate(by = bfi["education"], FUN = sd)


###-e1.6---------------------------------------------------------------------###

## Use dplyr functions to compute the means, medians, and variances of all
## numeric variables in the 'bfi' data.

bfi %>%
    select(where(is.numeric)) %>%
    summarize(
      across(.cols = everything(),
             .fns = list(mean = mean, med = median, var = var),
             na.rm = TRUE)
             )


###-e1.7---------------------------------------------------------------------###

## Create a pipeline to compute the correlation matrix of all numeric variables
## in the 'bfi' dataset.
## - Use Spearman's rho for the correlations.
## - Use only those participants whose level of educational attainment includes,
##   at least, graduating from college.

bfi %>%
    filter(education %in% c("college graduate", "graduate degree")) %>%
    select(where(is.numeric)) %>%
    cor(use = "pairwise", method = "spearman")


###-e1.8---------------------------------------------------------------------###

## Compute the internal consistency of the neuroticism scale for adult males.
## - Use the set.seed() function to set the random number seed to 314159.
## - Use 2000 bootstrap samples to estimate confidence intervals for the
##   internal consistency.
## - According to the bootstrap inference, is the internal consistency
##   significantly different from 0.8?

set.seed(314159)

bfi %>%
    filter(age >= 18, gender == "male") %>%
    select(matches("^N\\d")) %>%
    psych::alpha(n.iter = 2000, check.keys = TRUE)

## No. The bootstrapped CI for alpha includes 0.8, so we cannot infer a
## significant difference between alpha = 0.8 and the estimated alpha. 


###-e1.9---------------------------------------------------------------------###

## Use an exposition pipe to replicate the above t.test

bfi %$% t.test(agree, extra, paired = TRUE)


###-e1.10--------------------------------------------------------------------###

## Test for a positive correlation between agreeableness and openness in people
## younger than 30.

bfi %>% filter(age < 30) %$% cor.test(agree, open, alternative = "greater")


###-e1.11--------------------------------------------------------------------###

## Use the full 'bfi' dataset to estimate a linear regression model to test if
## openness predicts agreeableness after controlling for extraversion, age, and
## educational attainment.
## - Is the hypothesis supported?
## - What proportion of variability in agreeableness is explained by the
##   predictors?

fit1 <- lm(agree ~ open + extra + age + education, data = bfi)
(s1  <- summary(fit1))

## No. Openness is not a significant predictor of agreeableness after controlling
## for extraversion, age, and education.

s1$r.squared


################################################################################
### e2: Programming                                                          ###
################################################################################

###-e2.1---------------------------------------------------------------------###

## Write a 'hello world' function
##
## HINT: You can use the cat() function to echo a string to stdout

hello <- function() cat("Hello, World!\n")
hello()


###-e2.2---------------------------------------------------------------------###

## Write a function with two arguments that takes a vector of data and a vector
## of weights (with length equal to the data vector) and returns the weighted
## sum of the elements in the data vector.

weightedSum <- function(data, weights) sum(data * weights)

x <- rnorm(100)
y <- rep(1:5, 20)

weightedSum(data = x, weights = y)


###-e2.3---------------------------------------------------------------------###

## (a) Write a for loop that iterates over the rows of the mtcars dataset. For
##     each row, do the following:
##     - Compute the mean of all variables.
##     - Check if this mean is larger than 20.
##     Save the results of this loop in a logical vector.

data(mtcars)

out <- rep(NA, nrow(mtcars))
for(i in 1:nrow(mtcars))
    out[i] <- mtcars[i, ] %>% as.numeric() %>% mean() > 20

## (b) Write a single line of code that accomplishes the same as (a) using only
##     vectorized operations (i.e., no looping, no apply statements).

rowMeans(mtcars) > 20


###-e2.4---------------------------------------------------------------------###

## Write a for loop that iterates over the rows of the mtcars dataset. For each
## row, use an if/else statement to do the following:
## - Print the string "Yay!", if the car has a manual transmission.
## - Print the string "Boo!", if the car has an automatic transmission.

for(i in 1:nrow(mtcars)) {
    manual <- mtcars[i, "am"] == 1
    if(manual) print("Yay!")
    else print("Boo!")
}

## OR, using the ifelse() function ##

for(i in 1:nrow(mtcars)) {
    manual <- mtcars[i, "am"] == 1
    ifelse(manual, print("Yay!"), print("Boo!"))
}


###-END----------------------------------------------------------------------###
