### Title:    Introduction to R 3: Pipes
### Author:   Kyle M. Lang
### Created:  2022-01-04
### Modified: 2026-01-16

rm(list = ls(all = TRUE))

## Load necessary packages
library(dplyr)    # Data manipulation
library(magrittr) # Exposition pipe
library(psych)    # For creating scale scores

## Load the 'bfi' data from the 'psychTools' package to get the coding keys
# data(bfi, package = "psychTools")

## Read our processed version of the 'bfi' data from disk
bfi <- readRDS(here::here("data", "bfi.rds"))

###-Pipes--------------------------------------------------------------------###

### The pipe operator, |> or %>%, allows us to compose functions and chain
### together multiple data processing steps into single intuitive "pipelines"

### The pipe passes the output of one function as input to the next function in
### the pipeline. So pipes allow us to compose functions without drowning in
### nested parentheses.

### Say we want to compose the following series of functions
### 1. Select the five scale scores
### 2. Calculate the covariance matrix of these scores
### 3. Extract the variances from this covariance matrix
### 4. Convert these variances into standard deviations

## In base R, without pipes, we could do these operations with a series of
## nested functions
sqrt(
    diag(
        cov(
            bfi[c("agree", "consc", "extra", "neuro", "open")],
            use = "pairwise"
        )
    )
)

## We could also save intermediate objects along the way
tmp <- bfi[c("agree", "consc", "extra", "neuro", "open")]
tmp <- cov(tmp, use = "pairwise")
tmp <- diag(tmp)
sqrt(tmp)

## A pipeline makes the workflow clearer
bfi |>
    select(agree, consc, extra, neuro, open) |>
    cov(use = "pairwise") |>
    diag() |>
    sqrt()

################################################################################
## PRACTICE PROBLEM 3.1
##
## Use a pipeline to calculate the square root of the mean of the agreeableness
## scale score for males in the 'bfi' data.
##
## TIP: You can use the unlist() function to covert a list to a vector.
##
################################################################################

### We can also use pipes to chain together multistep data processing workflows
### into meaningful computational units

### Consider the following workflow applied to the BFI data
### 1. Center the 'age' variable on 18
### 2. Create scale scores for extraversion and neuroticism
### 3. Select only adult participants
### 4. Select only the extraversion and neuroticism scale scores and the
###    demographic items
### 5. Sort the data on ascending order of extraversion

## First, we prep the variable keys as you did earlier
(keys <- lapply(bfi.keys, tolower))

tmp <- select(bfi, matches("e\\d")) |> scoreVeryFast(keys = keys["extraversion"], items = _)

head(tmp)
?scoreVeryFast
scoreVeryFast(keys = keys)

## With the dplyr pipe, this sequence of operations could be achieved by
tmp1 <- bfi |>
    mutate(age = age - 18,
           extraversion = scoreVeryFast(keys = keys["extraversion"], items = across(everything()))[ , 1],
           neuroticism = scoreVeryFast(keys = keys["neuroticism"], items = across(everything()))[ , 1]) |>
    filter(age >= 0) |>
    select(extraversion, neuroticism, age, sex, edu, gm) |>
    arrange(extraversion)

head(tmp1, 30)

## With base R functions, and no pipes, the workflow will not be as elegant
scores <- scoreVeryFast(keys = keys[c("extraversion", "neuroticism")], items = bfi)
tmp2   <- data.frame(bfi, scores)

tmp2$age   <- tmp2$age - 18

tmp2 <- tmp2[tmp2$age >= 0, c("extraversion", "neuroticism", "age", "sex", "edu", "gm")]
tmp2 <- tmp2[order(tmp2$extraversion), ]

head(tmp2, 30)
all.equal(tmp1, tmp2, check.attributes = FALSE)

## The best approach would probably involve some compromise
scores <- scoreVeryFast(keys = keys[c("extraversion", "neuroticism")], items = bfi)
tmp3 <- bfi |>
    data.frame(scores) |>
    mutate(age = age - 18) |>
    filter(age >= 0) |>
    select(extraversion, neuroticism, age, sex, edu, gm) |>
    arrange(extraversion)

head(tmp3, 30)
all.equal(tmp1, tmp3)

################################################################################
## PRACTICE PROBLEM 3.2
##
## Create a single pipeline that completes the following data processing steps
## on the 'bfi' data.
##
## 1. Create a new variable, 'open4', by centering the 'open' variable on 4
## 2. Create a new variable, 'oaProd', that contains the product of 'open4' and
##    'agree'
## 3. Select only the rows for which 'oaProd' > 0
## 4. Select only the following items:
##    - The five agreeableness items
##    - The five openness items
##    - 'agree', 'open', and 'open4'
##
################################################################################

### Pipes work by passing the output from the upstream function into the
### downstream function as the first argument. Any additional arguments can be
### specified directly in the downstream function.

## These two lines are equivalent
mean(bfi$age)
bfi$age |> mean()

## As are these two
var(bfi$a1, na.rm = TRUE)
bfi$a1 |> var(na.rm = TRUE)

## If the input data are not the first argument to the downstream function, we
## cannot directly apply the pipe
bfi |> lm(extra ~ age)

## All is not lost, however. We can still use the pipe by using the '_'
## character as a placeholder for the data.
bfi |> lm(extra ~ age, data = _)

## If we use the dplyr pipe, the '.' character acts as the placeholder token.
bfi %>% lm(extra ~ age, data = .)

### The exposition pipe, %$%, (provided by the magrittr package) offers anther
### way to pass information forward through a pipeline. The exposition pipe
### doesn't pass a dataset. Rather, it "exposes" the variable names of that
### dataset to the downstream function.

## We can use the exposition pipe to solve our previous dilemma, as well
bfi %$% lm(extra ~ age)

################################################################################
## PRACTICE PROBLEM 3.3
##
## Use the pipe and exposition pipe to calculate the correlation between 'age'
## and 'agree' for adults in the 'bfi' data.
##
## - Your pipeline should return only a single correlation, not an entire
##   correlation matrix
##
## HINT: You can use the cor() function to compute the correlation between two
##       variables.
##
################################################################################

###-END----------------------------------------------------------------------###
