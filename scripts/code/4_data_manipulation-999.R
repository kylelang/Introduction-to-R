### Title:    Introduction to R 3: Working with Data
### Author:   Kyle M. Lang
### Created:  2022-01-04
### Modified: 2012-01-04

rm(list = ls(all = TRUE))

## Load necessary packages
library(dplyr)    # Data manipulation
library(magrittr) # Exposition pipe

## Load the 'bfi' data from the 'psychTools' package
data(bfi, package = "psychTools")

###-Subsetting---------------------------------------------------------------###

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

### Now we'll look into some other methods of subsetting rows and columns of a
### data frame

## Select the first 20 rows of all columns with names beginning in "A"
bfi[1:20, grep("^A", colnames(bfi))]

## Select the first 10 rows:
head(bfi, 10)

## Select the final 15 rows:
tail(bfi, 15)

## Select the first 5 rows and exclude columns 2 through 4
bfi[1:5, -(2:4)]

## Select rows 42, 45, and 56 and exclude the 'age' and 'education' columns
bfi[c(42, 45, 56), setdiff(colnames(bfi), c("age", "education"))]

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

## Select males who are younger than 50 and have at least a bachelor degree
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

### The 'dplyr' package contains a bunch of handy data manipulation utilities

### We can use the dplyr::filter() function to subset rows or a data frame based
### on logical conditions

## Do the same subsetting as above with dplyr::filter()
bfi3 <- filter(bfi, age < 50 & gender == 1 & !is.na(education) & education >= 4)

## Same result?
sum(bfi2 - bfi3, na.rm = TRUE)

### The dplyr::select() function is a very flexible tool for selecting columns
### from a data frame

## Use dplyr::select() to select the first 10 columns
tmp <- select(bfi, 1:10)
head(tmp)

## Use dplyr::select() to exclude the first 10 columns
tmp <- select(bfi, -10:-1)
head(tmp)

## Use dplyr::select() to select the 'gender', 'education', and 'age' columns
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


###-Sorting------------------------------------------------------------------###

## sort()
## order()
## dplyr::arrange()


###-Transformation-----------------------------------------------------------###

### One common type of data transformation is converting numeric or character
### variables into factors.

### A quick-and-dirty solution uses the as.factor() function to cast the
### varaible to a factor with default labels

(animals <- sample(c("dog", "cat", "mongoose"), 25, TRUE))

## The quick-and-dirty solution works fine for converting character vectors with
## meaningful values
(animalsF <- as.factor(animals))

levels(animalsF)
table(character = animals, factor = animalsF)

## The quick-and-dirty method isn't so nice for numeric variables:
genderF <- as.factor(bfi$gender)

levels(genderF)
table(numeric = bfi$gender, factor = genderF)

## We can use the factor() function to build exactly the factor we want
bfi0 <- bfi
bfi$gender <- factor(bfi$gender, labels = c("male", "female"))

levels(bfi$gender)
table(numeric = bfi0$gender, factor = bfi$gender)

### The dplyr::mutate() function is the tidyverse function for computing new
### variables

## Mean center age
bfi <- mutate(bfi, age_mc = age - mean(age))
head(bfi)

## Standardize age and convert education into a factor
bfi <- mutate(bfi,
              age_std = scale(age),
              education = factor(education,
                                 labels = c("some high school",
                                            "high school graduate",
                                            "some college",
                                            "college graduate",
                                            "graduate degree")
                                 )
              )
str(bfi)

### To project an operation across a range of variables, we need to use the
### across() function.

## Create mean scores for the five personality scales
bfi <- mutate(bfi,
              agree = rowMeans(across(A1:A5)),
              consc = rowMeans(across(C1:C5)),
              extra = rowMeans(across(E1:E5)),
              neuro = rowMeans(across(N1:N5)),
              open  = rowMeans(across(O1:O5))
              )
head(bfi)

## Mean center all of the agreeableness and extraversion items
bfi <- mutate(bfi,
              across(starts_with(c("A", "E"), ignore.case = FALSE),
                     ~ .x - mean(.x, na.rm = TRUE),
                     .names = "{.col}_mc")
              )
head(bfi)

### We can use the case_when() function inside dplyr::mutate() to build new
### variables from conditional logic

## Use case_when() inside of mutate() to create a new factor called
## 'gendered_maturity' according to the following logic
## - gendered_maturity = "boy" when age < 18 and gender = "male"
## - gendered_maturity = "girl" when age < 18 and gender = "female"
## - gendered_maturity = "man" when age >= 18 and gender = "male"
## - gendered_maturity = "woman" when age >= 18 and gender = "female"

bfi <- mutate(bfi,
              gendered_maturity = case_when(
                  age < 18 & gender == "male" ~ "boy",
                  age < 18 & gender == "female" ~ "girl",
                  age >= 18 & gender == "male" ~ "man",
                  age >= 18 & gender == "female" ~ "woman",
                  TRUE ~ NA_character_
              ),
              gendered_maturity = as.factor(gendered_maturity)
              )
str(bfi)
levels(bfi$gendered_maturity)
with(bfi, table(minor = age < 18, gender, gendered_maturity))

### We can use the dplyr::rename() and dplyr::rename_with() functions to easily
### rename columns in a data frame

## Rename a few of the variables in our bfi data
bfi <- rename(bfi, sex = gender, ed = education, gm = gendered_maturity)

## Convert all scale item names to lower case
bfi <- rename_with(bfi, .fn = tolower, .cols = matches("\\d$"))
head(bfi)


###-Pipes--------------------------------------------------------------------###

## Normal dplyr pipe
## Using the . character
## Exposition pipe
## Assignment pipe
## Base R pipe
