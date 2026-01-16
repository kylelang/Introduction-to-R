### Title:    Introduction to R 2: Data Transformation
### Author:   Kyle M. Lang
### Created:  2022-01-04
### Modified: 2026-01-16

rm(list = ls(all = TRUE))

dataDir <- "data"

## Load necessary packages
library(dplyr)    # Data manipulation
library(magrittr) # Exposition pipe
library(psych)    # For creating scale scores

## Load the 'bfi' data from the 'psychTools' package
data(bfi, package = "psychTools")


###-Converting Object Types--------------------------------------------------###

### Type conversions are one of the simplest and most common types of data
### transformations.

## We can type cast most basic data objects with an appropriate flavor of
## as.XXX() function
(x <- sample(0:1, 10, TRUE))
class(x)

(y <- as.character(x))
class(y)

(z <- as.logical(x))
class(z)

as.numeric(z)

## Type casting can produce some unexpected results
(x <- sample(1:2, 10, TRUE))
as.logical(x)

(x <- rep(-2:2, each = 2))
as.logical(x)

## Not all type casting is possible
(x <- sample(letters[1:2], 10, TRUE))
class(x)

as.logical(x)
as.numeric(x)

### We often do some kind of type conversion to create factors from variables
### encoded as character or numeric

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


###-Tidyverse Data Transformation--------------------------------------------###

### The dplyr::mutate() function is the tidyverse function for computing new
### variables

## Use dplyr::mutate() to create the gender factor
bfi <- mutate(bfi, gender = factor(gender, labels = c("male", "female")))

## Mean center age
bfi <- mutate(bfi, age_mc = age - mean(age))
head(bfi)

## Standardize age and convert education into a factor
bfi <- mutate(bfi,
              age_std = scale(age)[ , 1],
              education = factor(education,
                                 labels = c("some high school",
                                            "high school graduate",
                                            "some college",
                                            "college graduate",
                                            "graduate degree")
                                 )
              )
str(bfi)

################################################################################
## PRACTICE PROBLEM 2.1
##
## Use dplyr::mutate() to add three new variables to the 'bfi' data
## 1. rootAge = The square root of 'age'
## 2. permEdu = A random permutation of the 'education' factor
## 3. compEdu = A logical vector that contains TRUE for every row where
##              'permEdu' = 'education' and FALSE otherwise
## 
## HINTS: 
## - You can use the sample() function to permute the contents of an R vector
## - You can use the tidyr::replace_na() function to replace missing values with
##   some non-missing value
##
################################################################################

### To project an operation across a range of variables, we need to use the
### across() function.

## Create mean scores for the five personality scales
tmp <- mutate(bfi,
              agree = rowMeans(across(A1:A5), na.rm = TRUE),
              consc = rowMeans(across(C1:C5), na.rm = TRUE),
              extra = rowMeans(across(E1:E5), na.rm = TRUE),
              neuro = rowMeans(across(N1:N5), na.rm = TRUE),
              open  = rowMeans(across(O1:O5), na.rm = TRUE)
              )
head(tmp)

### In these particular data, some of the items are reverse coded, so we need to
### flip their scaling before we compute scale scores. While we could do this
### ourselves (e.g., using the dplyr::recode() function), the 'psych' package
### has a very convenient scoreItems() function that we can use to make our
### lives much easier.

## The psych::scoreItems() function requires a list of item keys to describe the
## coding. Thankfully, the 'bfi' data ship with pre-defined keys
bfi.keys

## Let's give the scales some more succinct names
names(bfi.keys) <- c("agree", "consc", "extra", "neuro", "open")

## Now we can create the mean scores:
tmp <- scoreItems(keys = bfi.keys, items = bfi, impute = "none")

## The scoreItems() function returns a bunch of extra stuff
ls(tmp)

## When we print the returned object, we see some nice summary stats
tmp

## If we just want the scores with no fluff, we can use psych::scoreVeryFast()
scores <- scoreVeryFast(keys = bfi.keys, items = bfi)

head(scores)

## Add the scale scores onto our data
bfi <- data.frame(bfi, scores)

head(bfi)

## Create squarerooted versions of the agreeableness and extraversion items
bfi <- mutate(bfi,
              across(starts_with(c("A", "E"), ignore.case = FALSE),
                     sqrt,
                     .names = "{.col}_sqrt")
              )

head(bfi)

################################################################################
## PRACTICE PROBLEM 2.2
##
## Use the dplyr::mutate() function to create standardized versions of the five
## scales scores we just created.
##
################################################################################

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
bfi <- rename(bfi, sex = gender, edu = education, gm = gendered_maturity)
head(bfi)

## Convert all scale item names to lower case
bfi <- rename_with(bfi, .fn = tolower, .cols = matches("\\d$"))
head(bfi)

## Save the modified data for later use
bfi |> 
  select(-contains("_")) |>
  saveRDS(here::here(dataDir, "bfi.rds"))

################################################################################
## PRACTICE PROBLEM 2.3
##
## Use dplyr::mutate() and case_when() to create a new factor called 'col_grad'
## that satisfies the following logic
## - col_grad = "no" when 'edu' is "some high school", "high school graduate",
##   or "some college"
## - col_grad = "yes" when 'edu' is "college graduate" or "graduate degree"
## - col_grad = is missing, otherwise
##
################################################################################

################################################################################
## PRACTICE PROBLEM 2.4
##
## NOTE: The following problem statement uses these abbreviations
##       - O = Openness to Experience ('open')
##       - E = Extraversion ('extra')
##
## Use dplyr::mutate() and case_when() to create a new factor called 'type'
## that satisfies the following logic
## - type = "adventurous" when O is higher than the mean of O and E is higher
##          than the mean of E
## - type = "inquisitive" when O is higher than the mean of O and E is lower
##          than or equal to the mean of E
## - type = "quiet" when O is lower than or equal to the mean of O and E is
##          lower than or equal to the mean of E
## - type = "chatty" when O is lower than or equal to the mean of O and E is
##          higher than the mean of E
##
################################################################################


###-Apply Functions----------------------------------------------------------###

### Base R apply functions let us broadcast univariate operations across the
### elements of vector-like data objects

x <- bfi[1:10, 1:5]
l1 <- list(a = rnorm(10), b = runif(10), c = 1:10)

## The basic model: apply()
apply(x, 1, max)
apply(x, 2, mean)
apply(x, 1:2, sqrt)

## For lists: lapply()
lapply(l1, sd)
lapply(l1, range)
lapply(x, var)

## Simplified: sapply()
sapply(l1, sd)
sapply(l1, range)
sapply(x, var)

################################################################################
## PRACTICE PROBLEM 2.5
##
## Use an appropriate apply function to convert all the variable names in the
## 'bfi.keys' list to lower case.
## - Do all the conversions with a single command
## - Return the resulting object as a list
##
################################################################################

###-END----------------------------------------------------------------------###
