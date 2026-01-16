### Title:    Introduction to R 1: Data Manipulation
### Author:   Kyle M. Lang
### Created:  2022-01-04
### Modified: 2026-01-16

rm(list = ls(all = TRUE))

## Load necessary packages
library(dplyr) # Data manipulation

## Load the 'bfi' data from the 'psychTools' package
data(bfi, package = "psychTools")


###-Base R Subsetting--------------------------------------------------------###

### We've already seen several flavors of subsetting using the '[]', '[[]]', and
###  '$' operators.

## Select the 'age' column
bfi$age
bfi[["age"]]
bfi[ , "age"]
bfi["age"]

## Select the first 10 rows
bfi[1:10, ]

## Select columns 1, 3, and 5
bfi[ , c(1, 3, 5)]

## Select the 'age' and 'gender' columns
bfi[ , c("age", "gender")]
bfi[c("age", "gender")]

## Select the first 5 rows and exclude columns 2 through 4
bfi[1:5, -(2:4)]

### The head() and tail() function let us easily select the first or last rows

## Select the first 10 rows:
head(bfi, 10)

## Select the final 15 rows:
tail(bfi, 15)

### We can also subset using logical vectors.

## Create a logical vector flagging any participant older than 30
(over30 <- bfi$age > 30)

## How many people is that?
sum(over30)

## What proportion of the total sample?
mean(over30)

## We can use this logical vector as a filter by using it to index rows
bfi30 <- bfi[over30, ]

dim(bfi30)
min(bfi30$age)

### We can define our logical filter vector using arbitrarily complex conditions

## Create a logical vector to flag males who are younger than 50 and have a
## bachelor degree or higher
filter <- with(bfi, age < 50 & gender == 1 & !is.na(education) & education >= 4)

## How many did we get?
sum(filter)

## Do the selection
bfi2 <- bfi[filter, ]

## Check the results
nrow(bfi2)
unique(bfi2$gender)
max(bfi2$age)
min(bfi2$education)

with(bfi, table(age, (gender == 1 & !is.na(education) & education >= 4)))

################################################################################
## PRACTICE PROBLEM 1.1
##
## Use base R subsetting procedures to select the five neuroticism items for
## female minors out of the 'bfi' data.
##
################################################################################


###-Tidyverse Subsetting-----------------------------------------------------###

### The 'dplyr' package contains a bunch of handy data manipulation utilities

### We can use the dplyr::filter() function to subset rows or a data frame based
### on logical conditions

## Do the same subsetting as above with dplyr::filter()
bfi3 <- filter(bfi, age < 50, gender == 1, !is.na(education), education >= 4)

## Same result?
sum(bfi2 - bfi3, na.rm = TRUE)

### The dplyr::select() function is a very flexible tool for selecting columns
### from a data frame

## Use dplyr::select() to select the first 10 columns
tmp <- select(bfi, 1:10)
head(tmp)

## Use dplyr::select() to exclude the first 10 columns
tmp <- select(bfi, -(1:10))
head(tmp)

## Be careful with negative signs
select(bfi, -1:10) |> head()
select(bfi, -10:-1) |> head()

-(1:10)
-10:-1
-1:10

## Use dplyr::select() to select the 'gender', 'age', and 'education' columns
tmp <- select(bfi, gender, age, education)
head(tmp)

## Use dplyr::select() to exclude the 'gender' and 'education' columns
tmp <- select(bfi, -gender, -education)
head(tmp)

## Use dplyr::select() to select all columns between "C1" and "O5"
tmp <- select(bfi, C1:O5)
head(tmp)

## Use dplyr::select() to select all variables whose names begin with "A" or "E"
tmp <- select(bfi, starts_with(c("A", "E"), ignore.case = FALSE))
head(tmp)

tmp <- select(bfi, contains("3"))
head(tmp)

################################################################################
## PRACTICE PROBLEM 1.2
##
## 1. Check the documentation for tidyselect::contains()
## 2. Use the contains() function to select only the third item from each of the
##    subscales in the 'bfi' data.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 1.3
##
## Use dplyr subsetting functions to select the same subset as in (4.1)
##
################################################################################


###-Sorting------------------------------------------------------------------###

## We can use the base R sort() function to order the elements of a vector
(x <- runif(6))
sort(x)
sort(x, decreasing = TRUE)

### To sort the rows of a matrix or data frame using base R functions, we can
### use the order() function, but order() can be confusing, so we won't bother.

### The dplyr::arrange() function offers a simple, intuitive way to sort the
### rows of a data frame.

(y <- data.frame(x1 = 1:10, x2 = c(-1, 1), x3 = runif(10)))
arrange(y, x3)
arrange(y, -x1)

## We can also sort on multiple columns
arrange(y, x2, x3)
arrange(y, x2, -x3)

################################################################################
## PRACTICE PROBLEM 1.4
##
## Use the dplyr functions to sort the 'bfi' data on descending order of 'age'
## and ascending order of 'gender'.
## - Sort 'age' within levels of 'gender'
##
################################################################################

###-END----------------------------------------------------------------------###
