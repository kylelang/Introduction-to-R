### Title:    Introduction to R 4: Working with Data
### Author:   Kyle M. Lang
### Created:  2022-01-04
### Modified: 2023-01-23

rm(list = ls(all = TRUE))

dataDir <- "data/"

## Load necessary packages
library(dplyr)    # Data manipulation
library(magrittr) # Exposition pipe
library(psych)    # For creating scale scores

## Load the 'bfi' data from the 'psychTools' package
data(bfi, package = "psychTools")

## Save the original variable names for later use
varNames0 <- colnames(bfi)


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

################################################################################
## PRACTICE PROBLEM 4.1
##
## Use base R subsetting procedures to select the five neuroticism items for
## female minors out of the 'bfi' data.
##
################################################################################

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
tmp <- select(bfi, -10:-1)
head(tmp)

tmp <- select(bfi, -(1:10))
head(tmp)

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

################################################################################
## PRACTICE PROBLEM 4.2
##
## Use dplyr subsetting functions to select the same subset as in (4.1)
##
################################################################################


###-Sorting------------------------------------------------------------------###

## We can use the sort() function to order the elements of a vector
(x <- runif(6))
sort(x)
sort(x, decreasing = TRUE)

## To sort the rows of a matrix or data frame using base R functions, we can use
## the order() function
x
order(x)

(y <- data.frame(x1 = x,
                 x2 = rnorm(6),
                 x3 = rep(letters[1:2], 3)
                 )
)

y[order(y$x1), ]
y[order(y$x1, decreasing = TRUE), ]

## Sorting rows with base R order() can be confusing. The dplyr::arrange() 
## function offers a simpler, intuitive way to sort the rows of a data frame
arrange(y, x1)
arrange(y, -x1)

## We can also sort on multiple columns
arrange(y, x3, x1)
arrange(y, x3, -x2)

################################################################################
## PRACTICE PROBLEM 4.3
##
## Use the dplyr functions to sort the 'bfi' data on descending order of 'age'
## and ascending order of 'gender'.
## - Sort on 'age' before 'gender'
##
################################################################################


###-Transformation-----------------------------------------------------------###

### One common type of data transformation is converting numeric or character
### variables into factors.

### A quick-and-dirty solution uses the as.factor() function to cast the
### variable to a factor with default labels

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

################################################################################
## PRACTICE PROBLEM 4.4
##
## Modify the factor levels of the 'education' factor we just created. Replace
## all of the spaces with underscores, "_".
##
## HINT 1: The levels() function can also be used to re-assign factor levels.
## HINT 2: If you want to be fancy, check out the gsub() function.
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

## Notice that we get a lot of other stuff in addition to the scores
ls(tmp)

## If we just want the scores with no fluff, we can use psych::scoreVeryFast()
scores <- scoreVeryFast(keys = bfi.keys, items = bfi)

head(scores)

## Add the scale scores onto our data
bfi <- data.frame(bfi, scores)

head(bfi)

## Mean center all of the agreeableness and extraversion items
bfi <- mutate(bfi,
              across(starts_with(c("A", "E"), ignore.case = FALSE),
                     ~ .x - mean(.x, na.rm = TRUE),
                     .names = "{.col}_mc")
              )
head(bfi)

## Save a clean version of the modified 'bfi' data for later
varNames <- c(varNames0, colnames(tmp$scores))
saveRDS(bfi[varNames], paste0(dataDir, "bfi.rds"))

################################################################################
## PRACTICE PROBLEM 4.5
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
bfi <- rename(bfi, sex = gender, ed = education, gm = gendered_maturity)
head(bfi)

## Convert all scale item names to lower case
bfi <- rename_with(bfi, .fn = tolower, .cols = matches("\\d$"))
head(bfi)

################################################################################
## PRACTICE PROBLEM 4.6
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

################################################################################
## PRACTICE PROBLEM 4.7
##
## (a) Exclude the raw scale items from the modified 'bfi' data.
## (b) Save the dataset from (a) as an RDS file.
##
################################################################################


###-Pipes--------------------------------------------------------------------###

### The dplyr pipe operator, %>%, allows us to compose functions and chain
### together multiple data processing steps into single intuitive "pipelines"

### The pipe passes the output of one function as input to the next function in
### the pipeline. So pipes allow us to compose functions without drowning in
### nested parentheses.

### Say we want to compose the following series of functions
### 1. Select the five scale scores
### 2. Calculate the coveriance matrix of these scores
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
bfi %>%
    select(agree, consc, extra, neuro, open) %>%
    cov(use = "pairwise") %>%
    diag() %>%
    sqrt()

################################################################################
## PRACTICE PROBLEM 4.8
##
## Use a pipeline to calculate the square root of the mean of the agreeableness
## scale score for males in the 'bfi' data.
##
## TIP: You can use the unlist() function to covert a list to a vector.
##
################################################################################

### We can also use pipes to chain together multistep data processing workflows
### into meanginful computational units

### Consider the following workflow applied to the BFI data
### 1. Center the 'age' variable on 18
### 2. Create scale scores for extraversion and neuroticism
### 3. Select only adult participants
### 4. Select only the extraversion and neuroticism scale scores and the
###    demographic items
### 5. Sort the data on acending order of extraversion

## With the dplyr pipe, this sequence of operations could be achieved by
tmp1 <- bfi %>%
    mutate(age = age - 18,
           extra = rowMeans(across(matches("^e\\d$")), na.rm = TRUE),
           neuro = rowMeans(across(matches("^n\\d$")), na.rm = TRUE)
           ) %>%
    filter(age >= 0) %>%
    select(extra, neuro, age, sex, ed, gm) %>%
    arrange(extra)

head(tmp1, 50)

## With base R functions, and no pipes, the workflow will not be as elegant
tmp2 <- bfi

tmp2$age   <- tmp2$age - 18
tmp2$extra <- rowMeans(tmp2[grep("^e\\d$", colnames(tmp2))], na.rm = TRUE)
tmp2$neuro <- rowMeans(tmp2[grep("^n\\d$", colnames(tmp2))], na.rm = TRUE)

tmp2 <- tmp2[tmp2$age >= 0, c("extra", "neuro", "age", "sex", "ed", "gm")]
tmp2 <- tmp2[order(tmp2$extra), ]

head(tmp2, 50)

all.equal(tmp1, tmp2, check.attributes = FALSE)

### Pipes work by passing the output from the upstream function into the
### downstream function as the first argument. Any additional arguments can be
### specified directly in the downstream function.

## These two lines are equivalent
mean(bfi$age)
bfi$age %>% mean()

## As are these two
var(bfi$a1, na.rm = TRUE)
bfi$a1 %>% var(na.rm = TRUE)

## If the input data are not the first argument to the downstream function, we
## cannot directly apply the pipe
bfi %>% lm(extra ~ age)

## All is not lost, however. We can still use the pipe by using the '.'
## character as a placeholder for the data.
bfi %>% lm(extra ~ age, data = .)

### The exposition pipe, %$%, (provided by the magrittr package) offers anther
### way to pass information forward through a pipeline. The exposition pipe
### doesn't pass a dataset. Rather, it "exposes" the variable names of that
### dataset to the downstream function.

## We can use the exposition pipe to solve our previous dilema, as well
bfi %$% lm(extra ~ age)

################################################################################
## PRACTICE PROBLEM 4.9
##
## Use the pipe and exposition pipe to calculate the correlation between 'age'
## and 'agree' for adults in the 'bfi' data.
##
## HINT: You can use the cor() function to compute the correlation between two
##       variables.
##
################################################################################

### As of R verison 4.1, there is a base R pipe operator, |>, that you can use
### if you just need to create a simple pipeline and don't want to load dplyr
### and/or magrittr.

## We can also implement the function composition pipeline from above using the
## base R pipe
bfi[c("agree", "consc", "extra", "neuro", "open")] |>
    cov(use = "pairwise") |>
    diag() |>
    sqrt()


###-END----------------------------------------------------------------------###

