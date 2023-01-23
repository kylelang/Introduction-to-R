### Title:    Introduction to R: Suggested Solutions for Practice Problems
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2023-01-23

rm(list = ls(all = TRUE))

dataDir <- "../data/"
figDir  <- "../figures/"

library(haven)
library(foreign)
library(openxlsx)
library(readxl)
library(dplyr)
library(magrittr)
library(psych)
library(ggplot2)
library(gridExtra)


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

install.packages(c("ggplot2", "dplyr", "haven"),
                 repos = "http://cloud.r-project.org")


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

## Use the setwd() function to change your working directory to the directory in
## which this script is saved.

setwd("Your/Directory/Path/Here")


###-3.2----------------------------------------------------------------------###

## Create a new RStudio project, and associate that project with the directory
## in which this script is saved.

### ANSWER: You have to do this with clicky-box options.


###-3.3----------------------------------------------------------------------###

## (a) Use the data() function to load the 'Cars93' dataset from the 'MASS'
##     package.

data(Cars93, package = "MASS")

## (b) Use the dim() function to check the dimensoins of the 'Cars93' data.
##     - How many rows?
##     - How many columns?

dim(Cars93)

## The Cars93 dataset has 93 rows and 27 columns.


###-3.4----------------------------------------------------------------------###

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


###-3.5----------------------------------------------------------------------###

## (a) Use the haven::read_spss() function to load the SPSS dataset saved at
##     '../data/starwars.sav'

starwars1 <- read_spss("../data/starwars.sav")

## (b) Use the foreign::read.spss() function to load the same dataset as above
##     into a list with variable labels preserved.

starwars2 <- read.spss("../data/starwars.sav")

## (c) Use the foreign::read.spss() function to load the same dataset as above
##     into a data frame without variable labels.

starwars3 <- read.spss("../data/starwars.sav",
                       to.data.frame    = TRUE,
                       use.value.labels = FALSE)


###-3.6----------------------------------------------------------------------###

## (a) Use the openxlsx::read.xlsx() function to load the first 100 rows (not
##     counting column names) of the first 4 columns from the 'diabetes' sheet
##     in the Excel workbook stored at '../data/example_data.xlsx'

dat3.6a <- read.xlsx("../data/example_data.xlsx",
                     sheet = "diabetes",
                     rows  = 1:100,
                     cols  = 1:4)

## (b) Use the readxl::read_excel() function with an appropriate specification
##     for the 'range' argument to load the chunk of data beginning on Row 3 and
##     Column 2 and ending on Row 100 and Column 7 from the 'titanic' sheet in
##     '../data/example_data.xlsx'

dat3.6b <- read_excel("../data/example_data.xlsx",
                      sheet = "titanic",
                      range = "B3:G100")

dat3.6b


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

tmp <- filter(bfi, gender == 2 & age < 18)
select(tmp, starts_with("N"))

## OR (If you peek ahead to the pipes section ##

bfi %>% filter(gender == 2 & age < 18) %>% select(starts_with("N"))


###-4.3----------------------------------------------------------------------###

## Use the dplyr functions to sort the 'bfi' data on descending order of 'age'
## and ascending order of 'gender'.
## - Sort on 'age' before 'gender'

arrange(bfi, gender, -age)


###-4.4----------------------------------------------------------------------###

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


###-4.5----------------------------------------------------------------------###

names(bfi.keys) <- c("agree", "consc", "extra", "neuro", "open")
scores          <- scoreVeryFast(bfi.keys, bfi)
bfi             <- data.frame(bfi, scores)

## Use the dplyr::mutate() function to create standardized versions of the five
## scales scores we just created.

bfi <- mutate(bfi, across(agree:open, scale, .names = "std_{.col}"))


###-4.6----------------------------------------------------------------------###

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


###-4.7----------------------------------------------------------------------###

## (a) Exclude the raw scale items from the modified 'bfi' data.

bfi <- select(bfi, -matches("^[aceno]\\d$")) %>% head()

## (b) Save the dataset from (a) as an RDS file.

saveRDS(bfi, "../data/practice_problem_4_8.rds")


###-4.8----------------------------------------------------------------------###

## Use a pipeline to calculate the square root of the mean of the agreeableness
## scale score for males in the 'bfi' data.
##
## TIP: You can use the unlist() function to covert a list to a vector.

bfi %>% filter(gender == 1) %>% select(agree) %>% unlist() %>% mean() %>% sqrt()

## OR, if you peek ahead to exposition pipes ##

bfi %>% filter(gender == 1) %$% mean(agree) %>% sqrt()


###-4.9---------------------------------------------------------------------###
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


################################################################################
### 6: Data Visualization                                                    ###
################################################################################

###-6.1----------------------------------------------------------------------###

## Use base R graphics and the 'titanic' data to create conditional boxplots,
## where plots of 'age' are conditioned on 'survived'.
## - What does this figure tell you about the ages of surivors ('survived' = 1)
##   vs. non-survivors ('survived' = 0)?

titanic <- readRDS(paste0(dataDir, "titanic.rds"))

boxplot(age ~ survived, data = titanic)

## There is not much difference in the age of survivors and non-survivors.


###-6.2----------------------------------------------------------------------###

## (a) Use the par() function to adjust the plotting canvas so you can draw two
##     plots in a 1x2 array.

par(mfrow = c(1, 2))

## (b) Using the diabetes data, create two plots. Both plots should begin with
##     a histogram of blood glucose level ('glu').
##     - In the first plot, overlay the kernel density plot for 'glu' as a blue
##       line.
##     - In the second plot, overlay the appropriate, theoretical normal density
##       as a red line.
##
## HINT: You can calculate the values for the normal density with the dnorm()
##       function. Don't forget to define the appropriate mean and SD.

diabetes <- readRDS(paste0(dataDir, "diabetes.rds"))

hist(diabetes$glu, freq = FALSE)
lines(density(diabetes$glu), col = "blue")

hist(diabetes$glu, freq = FALSE)
diabetes %>%
    arrange(glu) %>%
    mutate(density = dnorm(glu, mean(glu), sd(glu))) %$%
    lines(x = glu, y = density, col = "red")


###-6.3----------------------------------------------------------------------###

## Use GGPlot and the 'diabetes' data to create an empty plot of total
## cholesterol, 'tc', (on the y-axis) against 'age' (on the x-axis).
## - Don't add any geoms yet.
## - Assign the resulting plot object to a variable in your environment.

(p6.3 <- ggplot(diabetes, aes(age, tc)))


###-6.4----------------------------------------------------------------------###

## Augment the plot you created in (6.3) to create a scatterplot.
## - Map the size of the points to 'bmi'
## - Assign the resulting plot object to a variable in your environment.

(p6.4 <- p6.3 + geom_point(aes(size = bmi)))


###-6.5----------------------------------------------------------------------###

## Augment the plot you created in (6.4) by adding RUG lines to both the x-axis
## and y-axis.
## - Map the color of the RUG lines to 'glu'
## - Assign the resulting plot object to a variable in your environment.

(p6.5 <- p6.4 + geom_rug(aes(color = glu)))


###-6.6----------------------------------------------------------------------###

## Augment the plot you created in (6.5) by adding linear regression lines.
## - Add seperate lines for males and females.
## - Differentiate the regression lines by giving them different line types.
## - Do not include the SE bands.
## - Assign the resulting plot object to a variable in your environment.

(p6.6 <- p6.5 + geom_smooth(aes(linetype = sex), method = "lm", se = FALSE))


###-6.7--------------------------------------------------------------------=-###

## Modify the plot that you created in (6.6) by adjusting the theme.
## - Change the global theme to the "classic" theme.
## - Convert all text to 14-point, serif font.

(p6.7 <- p6.6 +
     theme_classic() +
     theme(text = element_text(family = "serif", size = 14))
)

###-6.8----------------------------------------------------------------------###

## Use the 'titanic' data, GGPlot, and facetting to create conditional
## histograms of 'age' conditioned on 'survived'.
## - Adjust the number of bins to optimize the clarity of the visualization.
## - Overlay kernel density plots on each histogram.
## - Do you think this figure is a more effective visualization than the
##   conditional boxplots you created in (6.1)? Why or why not?
##
## HINT: You can get ggplot to scale your histogram in proportions, rather than
##       counts, by specifying the argument "y = ..density.." for the y
##       aesthetic in an appropriate geom.

(p6.8 <- ggplot(titanic, aes(age)) +
     geom_histogram(aes(y = ..density..)) +
     geom_density() +
     facet_wrap(vars(survived))
)

(p6.8 <- ggplot(titanic, aes(age)) +
     geom_histogram(aes(y = ..density..), bins = 10) +
     geom_density() +
     facet_wrap(vars(survived))
)

(p6.8 <- ggplot(titanic, aes(age)) +
     geom_histogram(aes(y = ..density..), bins = 50) +
     geom_density() +
     facet_wrap(vars(survived))
)

(p6.8 <- ggplot(titanic, aes(age)) +
     geom_histogram(aes(y = ..density..), bins = 20) +
     geom_density() +
     facet_wrap(vars(survived))
)

## Yes. These histograms show a spike of very young survivors.


###-6.9----------------------------------------------------------------------###

## Use GGplot and grid.arrange() to recreate a version of the figure you created
## in (6.2).

p6.9 <- diabetes %>%
    mutate(density = dnorm(glu, mean(glu), sd(glu))) %>%
    ggplot(aes(glu)) +
    geom_histogram(aes(y = ..density..),
                   color = "black",
                   fill = "lightgray",
                   bins = 15) +
    theme_classic()

grid.arrange(
    p6.9 + geom_density(color = "blue"),
    p6.9 + geom_line(aes(glu, density), col = "red"),
    ncol = 2
)


###-6.10---------------------------------------------------------------------###

## Save the figure that you created in (6.8) as a JPEG
## - Adjust the size to 10cm X 10cm
## - Set the resolution to 800
## - Save the image to the "../figures/" directory

jpeg("../figures/practice_problem_6_10.jpg",
     width  = 10,
     height = 10,
     units  = "cm",
     res    = 800)

p6.8

dev.off()


###-6.11---------------------------------------------------------------------###

## (a) Save the five figures you created in (6.3 - 6.7) to a single PDF file.

pdf(paste0(figDir, "practice_problem_6_11a.pdf"))

p6.3
p6.4
p6.5
p6.6
p6.7

dev.off()

## (b) Save the five figures you created in (6.3 - 6.7) to a separate PNG files.
##     - Save both the PDF and the PNG files to the "../figures" directory

png(paste0(figDir, "practice_problem_6_11b-%d.png"))

p6.3
p6.4
p6.5
p6.6
p6.7

dev.off()


################################################################################
### 7: Programming                                                           ###
################################################################################

###-7.1----------------------------------------------------------------------###

## Write a 'hello world' function
##
## HINT: You can use the cat() function to echo a string to stdout

hello <- function() cat("Hello, World!\n")
hello()


###-7.2----------------------------------------------------------------------###

## Write a function with two arguments that takes a vector of data and a vector
## of weights (with length equal to the data vector) and returns the weighted
## sum of the elements in the data vector.

weightedSum <- function(data, weights) sum(data * weights)

x <- rnorm(100)
y <- rep(1:5, 20)

weightedSum(data = x, weights = y)


###-7.3----------------------------------------------------------------------###

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


###-7.4----------------------------------------------------------------------###

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
