### Title:    Introduction to R: Suggested Solutions for Practice Problems
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2022-01-29

rm(list = ls(all = TRUE))


################################################################################
### 1: Basic Commands                                                        ###
################################################################################

###-1.1----------------------------------------------------------------------###

## (a) Create an object called 'age' that takes the value of your age in whole
##     years.

age <- 35

## (b) Use the 'age' object you created in (a) to create a second object called
##     'weeks' that takes the value of your age in whole weeks.
##     - Assume 52 weeks in each year
##     - Disregard partial years (i.e., assume every year counted in 'age'
##       contains 52 whole weeks).

weeks <- age * 52


###-1.2----------------------------------------------------------------------###

## Use a single line of code to generate a logical value (i.e., TRUE/FALSE)
## indicating if the value of the 'weeks' object you created in (1.1b) is
## evenly divisible by 5 or 7.

weeks %% 5 == 0 | weeks %% 7 == 0


###-1.3----------------------------------------------------------------------###

## Use the rm() function to remove the 'age' object that you created in (1.1a)
## from your environment.

rm(age)


###-1.4----------------------------------------------------------------------###

## Use the setwd() function to change your working directory to the directory in
## which this script is saved.

setwd("Your/Directory/Path/Here")


###-1.5----------------------------------------------------------------------###

## Use the install.packages() function to install the following packages in the
## default location (i.e., don't specify anything for the 'lib' argument).

install.packages(c("ggplot2", "dplyr", "haven"),
                 repos = "http://cloud.r-project.org")

###-1.6----------------------------------------------------------------------###

##  (a) Access the help file for the vector() function.

?vector

##  (b) How many arguments does the vector() function take?

## The vector function takes two arguments: "mode" and "length"


################################################################################
### 2: Data Objects                                                          ###
################################################################################

###-2.1----------------------------------------------------------------------###

## Create a numeric vector containing the five even integers between 2 and 10
## (inclusive).

seq(2, 10, 2)


###-2.2----------------------------------------------------------------------###

## (a) Create the object 'myVec' by uncommenting and running the preceding two
##     lines of code.

set.seed(235711)
myVec <- sample(1:5)

## (b) Programatically create a logical vector that indicates which elements of
##     myVec are less than 3.

myVec < 3


###-2.3----------------------------------------------------------------------###
##
## (a) Create a 5x3 numeric matrix called 'myMat' wherein each column is equal
##     to the vector 'myVec' that you created for Problem 2.2.

myMat <- matrix(myVec, 5, 3)

## (b) Multiply each entry in 'myMat' by pi (i.e., the numerical constant).
##
## HINT: The built-in R object 'pi' contains the value of pi.

pi * myMat

###-2.4----------------------------------------------------------------------###

## (a) Create a list to describe yourself. Include the following named elements
##     in your list:
##     (1) Your Name
##     (3) Your Eye Color
##     (4) Your Hair Color
##     (5) Your Favorite Color

me <- list(name      = "Kyle M. Lang",
           eyeColor  = "Brown",
           hairColor = "Brown",
           favColor  = "Green")

## (b) Using a single command, test if your eye color OR your hair color is also
##     your favorite color.

me$eyeColor == me$favColor | me$hairColor == me$favColor

## OR ##

with(me, eyeColor == favColor | hairColor == favColor)


###-2.5----------------------------------------------------------------------###

## (a) Create the vectors x, y, and z by uncommented and running the preceding
##     three lines of code.

x <- rep(c(TRUE, FALSE), 10)
y <- rep(1, 20)
z <- rep(2, 20)

## (b) Create a data frame called 'myDf' with 20 rows and 4 columns
##     - Make the first column the logical negation of 'x'
##     - Make the second and third columns 'y' and 'z', respectivly
##     - Make the fourth column equal y/z (i.e., 'y' divided by 'z')

myDf <- data.frame(!x, y, z, y/z)

## (b) Use the paste() function to name the columns var-1, var-2, var-3, var-4.

colnames(myDf) <- paste("var", 1:4, sep = "-")

## (c) Name the rows with the first twenty letters of the English alphabet.

rownames(myDf) <- letters[1:20]


###-2.6----------------------------------------------------------------------###

## (a) Create a length-20 factor with two levels = {"yes", "no"}.

f <- factor(rep(c("yes", "no"), 10))

## (b) Add the factor you created in (a) to the data frame you created in (2.5)
##     as a new column called "f".

myDf$f <- f


################################################################################
### 3: Data I/O                                                              ###
################################################################################

###-3.1----------------------------------------------------------------------###

## (a) Use the data() function to load the 'Cars93' dataset from the 'MASS'
##     package.

data(Cars93, package = "MASS")

## (b) Use the dim() function to check the dimensoins of the 'Cars93' data.
##     - How many rows?
##     - How many columns?

dim(Cars93)

## The Cars93 dataset has 93 rows and 27 columns.


###-3.2----------------------------------------------------------------------###

## (a) Load the dataset saved as '../data/diabetes.rds'.

diabetes <- readRDS("../data/diabetes.rds")


## (b) Use the str() function to compare the structure of the data you loaded in
##     (a) to the diabetes data loaded above using the read.table() function.
##     - Are there any differences between these two objects? If so, what are
##       the differences?

diabetes0 <- read.table("../data/diabetes.txt", header = TRUE, sep = "\t")

str(diabetes)
str(diabetes0)

## The 'sex' variable is a factor when reading the data from the RDS file, but
## it's a character vector when reading the data from the tab-delimited file.


###-3.3----------------------------------------------------------------------###

## (a) Use the haven::read_spss() function to load the SPSS dataset saved at
##     '../data/starwars.sav'

library(haven)
starwars1 <- read_spss("../data/starwars.sav")

## (b) Use the foreign::read.spss() function to load the same dataset as above
##     into a list with variable labels preserved.

library(foreign)
starwars2 <- read.spss("../data/starwars.sav")

## (c) Use the foreign::read.spss() function to load the same dataset as above
##     into a data frame without variable labels.

starwars3 <- read.spss("../data/starwars.sav",
                       to.data.frame    = TRUE,
                       use.value.labels = FALSE)


###-3.4----------------------------------------------------------------------###

## (a) Use the openxlsx::read.xlsx() function to load the first 100 rows (not
##     counting column names) of the first 4 columns from the 'diabetes' sheet
##     in the Excel workbook stored at '../data/example_data.xlsx'

library(openxlsx)
dat3.4a <- read.xlsx("../data/example_data.xlsx",
                     sheet = "diabetes",
                     rows  = 1:100,
                     cols  = 1:4)

## (b) Use the readxl::read_excel() function with an appropriate specification
##     for the 'range' argument to load the chunk of data beginning on Row 3 and
##     Column 2 and ending on Row 100 and Column 7 from the 'titanic' sheet in
##     '../data/example_data.xlsx'

library(readxl)
dat3.4b <- read_excel("../data/example_data.xlsx",
                      sheet = "titanic",
                      range = "B3:G100")

dat3.4b


################################################################################
### 4: Data Manipulation                                                     ###
################################################################################

###-4.1----------------------------------------------------------------------###

## Use base R subsetting procedures to select the five neuroticism items for
## female minors out of the 'bfi' data.

data(bfi, package = "psychTools")

filter <- with(bfi, gender == 2 & age < 18)
bfi[filter, paste0("N", 1:5)]


###-4.2----------------------------------------------------------------------###

## Use dplyr subsetting functions to select the same subset as in (4.1)

library(dplyr)

tmp <- filter(bfi, gender == 2 & age < 18)
select(tmp, starts_with("N"))

## OR (If you peek ahead to the pipes section ##

bfi %>% filter(gender == 2 & age < 18) %>% select(starts_with("N"))


###-4.3----------------------------------------------------------------------###

## Use base R functions to sort the 'bfi' data on ascending order of 'age'

bfi[order(bfi$age), ]


###-4.4----------------------------------------------------------------------###

## Use the dplyr functions to sort the 'bfi' data on descending order of 'age'
## and ascending order of 'gender'.
## - Sort on 'age' before 'gender'

arrange(bfi, gender, -age)


###-4.5----------------------------------------------------------------------###

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

## Modify the factor levels of the 'education' factor we just created. Replace
## all of the spaces with underscores, "_".
##
## HINT 1: The levels() function can also be used to reassign factor levels.
## HINT 2: If you want to be fancy, check out the gsub function.

levels(bfi$education) <- gsub(" ", "_", levels(bfi$education))


###-4.6----------------------------------------------------------------------###

library(psych)

names(bfi.keys) <- c("agree", "consc", "extra", "neuro", "open")
scores          <- scoreVeryFast(bfi.keys, bfi)
bfi             <- data.frame(bfi, scores)

## Use the dplyr::mutate() function to create standardized versions of the five
## scales scores we just created.

bfi <- mutate(bfi, across(agree:open, scale, .names = "std_{.col}"))


###-4.7----------------------------------------------------------------------###

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

mO <- mean(bfi$open)
mE <- mean(bfi$extra)

bfi <- mutate(bfi,
              type = case_when(
                  open > mO & extra > mE ~ "adventurous",
                  open > mO & extra <= mE ~ "inquisitive",
                  open <= mO & extra <= mE ~ "quiet",
                  open <= mO & extra > mE ~ "chatty")
              )


################################################################################
## PRACTICE PROBLEM 4.8
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
## PRACTICE PROBLEM 4.9
##
## Use a pipeline to caclulate the squareroot of the mean of the agreeableness
## scale scores for males in the 'bfi' data.
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
## PRACTICE PROBLEM 4.10
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
