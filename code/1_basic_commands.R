### Title:    Introduction to R 1: Basic Commands
### Author:   Kyle M. Lang
### Created:  2016-01-28
### Modified: 2023-01-23

rm(list = ls(all = TRUE))

###-Comments-----------------------------------------------------------------###

## The comment character in R is '#'
## Each commented line must be preceded by a '#' symbol
## There are no block comments in R (same system as Mplus)

## Comments are not evaluated when you run your code


###-Assignment---------------------------------------------------------------###

## To do anything useful, we need to create objects that hold data
## We 'assign' values to objects via the 'assignment' operator
y <- 7
x = 33.33
"Bob" -> z

## Evaluating an object name without assignment prints the value of that object
## to stdout
y
x
z

## The assigned values can also be named objects
w <- y
w

## Object names must begin with a letter
my1X <- pi
my1X

1X <- pi # Uh-oh :(


###-Mathematical Operators---------------------------------------------------###

## Arithmetic
y + x
y - x
y * x
y / x

## Powers
y^2
y^3

## Roots
sqrt(y)

## Or use fractional exponents
y^(1/3) # cube root of y
y^(1/4) # quartic root of y

### Logarithms and anti-logs
log(y)   # natural logarithm of y
log10(y) # log base 10 of y
log2(y)  # log base 2 of y

exp(x) # exponentiate x

## Other useful stuff
abs(y) # absolute value of y
x %% y # modulo operator := remainder after diving x by y

################################################################################
## PRACTICE PROBLEM 1.1
##
## (a) Create an object called 'age' that takes the value of your age in whole
##     years.
## (b) Use the 'age' object you created in (a) to create a second object called
##     'weeks' that takes the value of your age in whole weeks.
##     - Assume 52 weeks in each year
##     - Disregard partial years (i.e., assume every year counted in 'age'
##       contains 52 whole weeks).
##
################################################################################


###-Logical Comparisons------------------------------------------------------###

y <- 5
x <- 7
w <- 5

### Check equality
y == x
y == w

### Check relative size
y > x  # greater than
y >= x # greater than or equal to
y < x  # less than
y <= x # less than or equal to

y > w
y >= w
y < w
y <= w

## We can negate any logical condition by prepending the '!' character
y > x
!y > x

y == w
y != w

## We can create more complex logical conditions with the AND and OR operators:
## '&' and '|'
y == w & y < x
y == w & y > x
y == w | y > x

################################################################################
## PRACTICE PROBLEM 1.2
##
## Use a single line of code to generate a logical value (i.e., TRUE/FALSE)
## indicating if the value of the 'weeks' object you created in (1.1b) is
## evenly divisible by 5 or 7.
##
################################################################################


###-Order of Operations------------------------------------------------------###

### R will, mostly, follow the usual PEMDAS ordering for mathematic operations,
### but it's not psychic. When in doubt, use parentheses!

y^(1/2)
y^1/2

y^(1/2) == sqrt(y)
y^1/2 == y/2

y * 3 / (11 - x)
y * 3 / 11 - x

z <- y * 3 / 11
y * 3 / 11 - x == z - x

y + x / w
(y + x) / w


###-Interacting with the Environment-----------------------------------------###

### The 'environment' is a loosely organized set of all the objects that R
### currently has stored in working memory

## Check the contents of the current environment
ls()

## Remove an object from the environment
rm(x)
ls()

################################################################################
## PRACTICE PROBLEM 1.3
##
## Use the rm() function to remove the 'age' object that you created in (1.1a)
## from your environment.
##
################################################################################

## Completely clear the enviroment
rm(list = ls(all = TRUE))
ls()


###-Installing Packages------------------------------------------------------###

## Use the default CRAN mirror
install.packages("psych")

## Specify the CRAN mirror a priori
install.packages(c("lattice", "gridExtra"),
                 repos = "http://cloud.r-project.org")

## Specify a non-standard directory into which the packages will be installed
install.packages("mvtnorm",
                 repos = "http://cloud.r-project.org",
                 lib = "../software")

## Install from local source
install.packages("../software/magrittr_2.0.1.tar.gz",
                 repos = NULL,
                 type = "Source")

################################################################################
## PRACTICE PROBLEM 1.4
##
## Use the install.packages() function to install the following packages in the
## default location (i.e., don't specify anything for the 'lib' argument).
##
## PACKAGES: ggplot2, dplyr, haven
##
################################################################################


###-Getting Help-------------------------------------------------------------###

## Prepending the '?' character will access the help file for a function
?lm

## We can also use the 'help' function
help(lm)
help("lm")

## Non-letter characters need to be quoted
?/
help(/)
?"/"
help("/")

## If a package is not loaded, we need to specify the namespace
?quickpred
?mice::quickpred
help(quickpred, package = "mice")

## We can also open an interactive web-based help page
help.start()

################################################################################
## PRACTICE PROBLEM 1.5
##
##  (a) Access the help file for the vector() function.
##  (b) How many arguments does the vector() function take?
##
################################################################################


###-END----------------------------------------------------------------------###
