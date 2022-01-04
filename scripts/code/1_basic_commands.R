### Title:    Introduction to R 1: Basic R Commands
### Author:   Kyle M. Lang
### Created:  2016-01-28
### Modified: 2012-01-04


### Comments ###

## The comment character in R is '#'
## Each commented line must be preceded by a '#' symbol
## There are no block comments in R (same system as Mplus)

## Comments are not evaluted when you run your code

## There are two important uses of comments:
## 1) Documenting your syntax
## 2) Excluding probelmatic lines of code When debugging
## ---- When debugging, commenting out parts of your code,
## ---- rather than deleting them, will save you tons of time


### Assignment ###

## To do anything useful, we need to create objects that hold data
## We 'assign' values to objects via the 'assignment' operator
y <- 7
x = 33.33
"Bob" -> z

## Evaluating an object name without assignment
## prints the value of that object to stdout
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


### Mathematical Operators ###

### Addition
y + x

### Subtraction
y - x

### Multiplication
y * x

### Division
y / x

### Powers
## x^n will raise x to the nth power
y^2 # square y
y^3 # cube y

### Roots
sqrt(y) # square root of y

## for nth roots with n > 2, use fractional exponents
y^(1/3) # cube root of y
y^(1/4) # quartic root of y

### Logarithms and anti-logs
log(y)   # natural logarithm of y
log10(y) # base 10 log of y
log2(y)  # base 2 log of y

exp(x) # exponentiate x

### Other useful stuff
abs(y) # absolute value of y

x %% y # modulo operator := remainder after diving x by y


### Logical Comparisons ###

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

## We can negate any logical condition by prepending a '!' character
y > x
!y > x

y == w
y != w

## We can create more complex logical conditions with the
## AND and OR operators: '&' and '|'
y == w & y < x
y == w & y > x
y == w | y > x


### Order of Operations ###

## R will, mostly, follow the usual PEMDAS ordering for mathematic operations,
## but it's not psychic. So, when in doubt, use parentheses!

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


### Interacting with the Environment ###

## The 'environment' is a loosely organized set of all
## the objects that R currently has stored in working memory

## Check the contents of the current environment:
ls()

## Remove an object from the environment
rm(x)
ls()

## (Nearly) totally clear the enviroment
rm(list = ls(all = TRUE))
ls()

## Every R session is associated with a 'working directory'
## The working directory is the directory that R will use to
## read or write data objects to or from disk.

## Find the current working directory
getwd()

## Change the current working directory
setwd("data/")
getwd()


### Install Packages ###

## Prompted to choose CRAN mirror
install.packages("ICC")

## CRAN mirror specified a priori
install.packages(c("mice", "vcd"),
                 repos = "http://rweb.quant.ku.edu/cran")

## Specify a directory into which the packages will be installed
install.packages("mvtnorm",
                 repos = "http://rweb.quant.ku.edu/cran",
                 lib = "../../software")

## Install from local source
install.packages("../../software/quark_0.5.4.tar.gz",
                 repos = NULL,
                 type = "Source")

##### PRACTICE PROBLEM 1.1 #####

## TASK: Use the commands presented above to install the following
##       packages in the default location (i.e., don't specify
##       anything for the 'lib' argument)
## PACKAGE LIST: mvtnorm, lavaan, psych, mitools, multicomp

##### END PRACTICE PROBLEM 1.1 #####

### Getting help ###

## Prepending the '?' character will access the help file
## for a function
?lm

## We can also use the 'help' function
help(lm)
help("lm")

## Non-letter characters need to be quoted
?/
help(/)
?"/"
help("/")

## If a package is not loaded we need to specify the namespace
?quickpred
?mice::quickpred
help(quickpred, package = "mice")

## We can also open an interactive web-based help page
help.start()

##### PRACTICE PROBLEM 1.2 #####

## TASK: (a) Access the help file for the 'vector()' function.
##       (b) How many arguments does the 'vector()' function take?

##### END PRACTICE PROBLEM 1.2 #####
