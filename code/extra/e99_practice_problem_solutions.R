### Title:    Introduction to R: Suggested Solutions for Extra Practice Problems
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2025-01-18

rm(list = ls(all = TRUE))

dataDir <- "data/"

library(dplyr)
library(magrittr)


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

## Use the install.packages() function to install the following packages in the
## default location (i.e., don't specify anything for the 'lib' argument).

install.packages(c("ggplot2", "dplyr", "haven"))


###-1.5----------------------------------------------------------------------###

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

## (b) Programmatically create a logical vector that indicates which elements of
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
##     - Make the second and third columns 'y' and 'z', respectively
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

## Create a new RStudio project associated with the directory that you want to
## use as the working directory for these exercises.

### ANSWER: You have to do this with clicky-box options.


###-3.2----------------------------------------------------------------------###

## (a) Load the dataset saved as '../data/diabetes.rds'.

diabetes <- readRDS(paste0(dataDir, "diabetes.rds"))

## (b) Use the str() function to compare the structure of the data you loaded in
##     (a) to the 'diabetes2' dataset loaded above.
##     - Are there any differences between these two objects? If so, what are
##       the differences?

diabetes2 <- read.table(paste0(dataDir, "diabetes.txt"),
                        header = TRUE,
                        sep = "\t")

str(diabetes)
str(diabetes2)

## The 'sex' variable is a factor when reading the data from the RDS file, but
## it's a character vector when reading the data from the tab-delimited file.


###-3.3----------------------------------------------------------------------###

## (a) Use the haven::read_spss() function to load the SPSS dataset saved at
##     'data/starwars.sav'

starwars <- read_spss(paste0(dataDir, "starwars.sav"))


###-3.4----------------------------------------------------------------------###

## (a) Use the openxlsx::read.xlsx() function to load the first 100 rows (not
##     counting column names) of the first 4 columns from the 'diabetes' sheet
##     in the Excel workbook stored at '../data/example_data.xlsx'

dat3.4a <- read.xlsx(paste0(dataDir, "example_data.xlsx"),
                     sheet = "diabetes",
                     rows  = 1:100,
                     cols  = 1:4)

## (b) Use the readxl::read_excel() function with an appropriate specification
##     for the 'range' argument to load the chunk of data beginning on Row 3 and
##     Column 2 and ending on Row 100 and Column 7 from the 'titanic' sheet in
##     '../data/example_data.xlsx'

dat3.4b <- read_excel(paste0(dataDir, "example_data.xlsx"),
                      sheet = "titanic",
                      range = "B3:G100")

dat3.4b


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
