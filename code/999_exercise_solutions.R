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
## HINT 1: The levels() function can also be used to re-assign factor levels.
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


###-4.8----------------------------------------------------------------------###

## (a) Exclude the raw scale items from the modified 'bfi' data.

bfi <- select(bfi, -matches("^[aceno]\\d$")) %>% head()

## (b) Save the dataset from (a) as an RDS file.

saveRDS(bfi, "../data/bfi_pp_48.rds")


###-4.9----------------------------------------------------------------------###

## Use a pipeline to calculate the square root of the mean of the agreeableness
## scale score for males in the 'bfi' data.
##
## TIP: You can use the unlist() function to covert a list to a vector.

bfi %>% filter(gender == 1) %>% select(agree) %>% unlist() %>% mean() %>% sqrt()

## OR, if you peek ahead to exposition pipes ##

library(magrittr)

bfi %>% filter(gender == 1) %$% mean(agree) %>% sqrt()


###-4.10---------------------------------------------------------------------###
##
## Use the pipe and exposition pipe to calculate the correlation between 'age'
## and 'agree' for adults in the 'bfi' data.
##
## HINT: You can use the cor() function to compute the correlation between two
##       variables.

bfi %>% filter(age > 18) %$% cor(age, agree)


################################################################################
### 5: Data Analysis                                                         ###
################################################################################

###-5.1----------------------------------------------------------------------###

## Use dplyr functions to compute the mean, variance, and range of 'age' for
## females in the 'bfi' data.

bfi <- readRDS("../data/bfi.rds")

bfi %>%
    filter(gender == "female") %>%
    summarize(age_mean = mean(age), age_var = var(age), age_range = range(age))


###-5.2----------------------------------------------------------------------###

## Create a logical vector with one entry for every variable in the 'bfi' data.
## This vector should take the value TRUE when males have a higher proportion of
## missing data on that variable than females do.

male   <- bfi %>% filter(gender == "male") %>% is.na() %>% colMeans()
female <- bfi %>% filter(gender == "female") %>% is.na() %>% colMeans()

male > female


###-5.3----------------------------------------------------------------------###

## Use an appropriate apply function to create a vector containing the variances
## of all numeric variables in the 'bfi' data.

bfi %>% select(where(is.numeric)) %>% sapply(var, na.rm = TRUE)


###-5.4----------------------------------------------------------------------###

## Use the tapply() function to compute the average neuroticism value for minors
## and for adults.

bfi %$% tapply(neuro, age < 18, mean)


###-5.5----------------------------------------------------------------------###

## Use the aggregate function to compute SDs for 'extra', 'agree', and 'open'
## within education groups.

bfi %>%
    select(extra, agree, open) %>%
    aggregate(by = bfi["education"], FUN = sd)


###-5.6----------------------------------------------------------------------###

## Use dplyr functions to compute the means, medians, and variances of all
## numeric variables in the 'bfi' data.

bfi %>%
    select(where(is.numeric)) %>%
    summarize(
        across(.fns = list(mean = mean, med = median, var = var), na.rm = TRUE)
    )


###-5.7----------------------------------------------------------------------###

## Create a pipeline to compute the correlation matrix of all numeric variables
## in the 'bfi' dataset.
## - Use Spearman's rho for the correlations.
## - Use only those participants whose level of educational attainment includes,
##   at least, graduating from college.

bfi %>%
    filter(education %in% c("college graduate", "graduate degree")) %>%
    select(where(is.numeric)) %>%
    cor(use = "pairwise", method = "spearman")


###-5.8----------------------------------------------------------------------###

## Compute the internal consistency of the neuroticism scale for adult males.
## - Use the set.seed() function to set the random number seed to 314159.
## - Use 2000 bootstrap samples to estimate confidence intervals for the
##   internal consistency.
## - According to the bootstrap inference, is the internal consistency
##   significanlty different from 0.8?

set.seed(314159)

bfi %>%
    filter(age >= 18, gender == "male") %>%
    select(matches("^N\\d")) %>%
    alpha(n.iter = 2000, check.keys = TRUE)

## No. The bootstrapped CI for alpha includes 0.8, so we cannot infer a
## significant difference between alpha = 0.8 and the estimated alpha. 


###-5.9----------------------------------------------------------------------###

## Use an exposition pipe to replicate the above t.test

bfi %$% t.test(agree, extra, paired = TRUE)


###-5.10---------------------------------------------------------------------###

## Test for a positive correlation between agreeableness and openness in people
## younger than 30.

bfi %>% filter(age < 30) %$% cor.test(agree, open, alternative = "greater")


###-5.11---------------------------------------------------------------------###

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


###-5.12---------------------------------------------------------------------###

## Modify the model you fit in (5.11) to test if age moderates the effect of
## openness on agreeableness or the effect of extraversion on agreeableness,
## after controlling for educational attainment.
## - Does age moderate either of the focal effects?
## - If you find significant moderation, how does age affect the focal effects?
## - How much more variability in agreeableness have you explained by modifying
##   the model?
## - Is the additional explained variation significant?


fit2 <- update(fit1, ". ~ . + age * open + age * extra")
(s2 <- summary(fit2))

## No. Age does not moderate either focal effect.

s2$r.squared - s1$r.squared

anova(fit1, fit2)

## No. The modified  model does not explain significantly more variability.


###-5.13---------------------------------------------------------------------###
##
## (a) Use the rockchalk::plotSlopes() function to visualize the simple slopes
##     for one of the interactions you estimated in (5.12).
##     - Define the simple slopes at the mean of the moderator and one SD above
##       and below the mean of the moderator.

library(rockchalk)

psOut <- plotSlopes(fit2, plotx = "extra", modx = "age", modxVals = "std.dev")

## (b) Use the rockchalk::testSlopes() function to test the simple slopes you
##     estimated in (a) for significance.
##     - Are any of the simple slopes significant?
##     - Interpret any significant simple slopes.

testSlopes(psOut)$hypotests

## All three simple slopes are significant. Extraversion significantly predicts
## agreeableness for people with mean age and for people one SD above or below
## the mean of age.


###-5.14---------------------------------------------------------------------###

## Estimate a one-way ANCOVA to test if there is a difference in mean openness
## between adults with different levels of educational attainment after
## controlling for age, extraversion, and agreeableness
## - Is the hypothesis supported?
## - Do all predictors combined explain a significant proportion of variability
##   in openness?

bfi %>% filter(age >= 18) %$% lm(open ~ education + age + extra + agree) -> fit

summary.aov(fit)

## Yes. There are significant differences in the mean levels of openness between
## different educational groups after controlling for age, extraversion, and
## agreeableness.

summary(fit)

## Yes. The omnibus F-test is significant. So, we know that the R^2 for the
## model is significantly greater than zero.


###-5.15---------------------------------------------------------------------###

## Use Tukey's HSD to test all pairwise differences between the different levels
## of educational attaiment from the model you estimated in (5.14).
## - Are any of the groups significanlty different in their mean levels of
##   openness? If so, which?

library(multcomp)

glht(fit, linfct = mcp(education = "Tukey")) %>% summary()

## Yes. The following groups have significanlty different means of openness:
## - graduate degree and some high school
## - graduate degree and high school graduate
## - graduate degree and some college
## - college graduate and some college


################################################################################
### 6: Data Visualization                                                    ###
################################################################################
