### Title:    Introduction to R 7: Simple R Programming
### Author:   Kyle M. Lang
### Created:  2016-01-29
### Modified: 2023-01-23

rm(list = ls(all = TRUE))

library(dplyr)

data(mtcars)

###-Functions----------------------------------------------------------------###

### We can define our own functions using the function() function (how many
### times do you think I can use the word "function" in a single sentence?)

## Define a function
square <- function(x) {
    out <- x^2
    out
}

## Use the function
square(5)

## We don't need an explict return() statement. R functions will return any
## object that is 'printed' as the last command inside the function (e.g.,
## 'out' in the above example)

## One-line functions don't need braces
square <- function(x) x^2
square(5)

## Function arguments are not strictly typed. R will try to work with whatever
## you provide as input.
square(1:5)
square(pi)
square(TRUE)
square("bob") # But one can only try so hard

## Functions can take multiple arguments
mod <- function(x, y) x %% y
mod(10, 3)

## Sometimes it's useful to specify a list of arguments that we unpack inside
## the function
getLsBeta <- function(datList) {
    X <- datList$X
    y <- datList$y

    solve(crossprod(X)) %*% t(X) %*% y
}

X       <- matrix(runif(500), ncol = 5)
datList <- list(y = X %*% rep(0.5, 5), X = X)

getLsBeta(datList = datList)

################################################################################
## PRACTICE PROBLEM 7.1
##
## Write a 'hello world' function
##
## HINT: You can use the cat() function to echo a string to stdout
##
################################################################################

### Functions are first-class objects in R. We can treat them like any other R
### object.

## R views an initialized, but unevaluated, function as a special object with
## type "closure"
class(getLsBeta)
typeof(getLsBeta)


## After evaluation, functions are simply equivelent to the objects they return.
class(getLsBeta(datList))
typeof(getLsBeta(datList))

### We can use functions as arguments to other operations and functions.

fun1 <- function(x, y) x + y

## What will this command return?
fun1(1, fun1(1, 1))


## Why would we care?
s2 <- var(runif(100))
x  <- rnorm(100, 0, sqrt(s2))

X[1:10, ]

c(1, 3, 6:9, 12)

rnorm(100) %>% square() %>% mean()

################################################################################
## PRACTICE PROBLEM 7.2
##
## Write a function with two arguments that takes a vector of data and a vector
## of weights (with length equal to the data vector) and returns the weighted
## sum of the elements in the data vector.
##
################################################################################


###-Loops--------------------------------------------------------------------###

## There are three types of loops in R: for, while, and until, but, you'll
## rarely use anything but the for loop. So, we're not going to discuss while or
## until loops

### A for loop is defined as follows
### for(INDEX in RANGE) {
###     Stuff To Do with the Current INDEX Value
### }

## Sum the numbers from 1 to 100
val <- 0
for(i in 1:100) {
    val <- val + i
}

val

## Compute the mean of every column in the 'mtcars' data
means <- rep(0, ncol(mtcars))
for(j in 1 : ncol(mtcars)) {
    means[j] <- mean(mtcars[ , j])
}

means

## Regress each variable in the 'mtcars' dataset onto all others
fits <- list()
for(v in 1 : ncol(mtcars)) {
    fits[[v]] <- lm(mtcars[ , v] ~ as.matrix(mtcars[ , -v]))
}

fits

## Summarize the previous results
summaries <- list()
for(bob in 1 : length(fits))
    summaries[[bob]] <- summary(fits[[bob]])

summaries

## A more direct version of the above
for(f in fits) print(summary(f))

### Loops are often one of the least efficient solutions in R

n <- 1e8

t0 <- system.time({
    val0 <- 0
    for(i in 1:n) val0 <- val0 + i
})

t1 <- system.time(
    val1 <- sum(1:n)
)

val0 - val1 # Same answer
t0
t1     # But many times slower

### There is often a built in routine for what you are trying to accomplish with
### the loop.

## The appropriate way to get variable means:
colMeans(mtcars)

### In R, we're usually working with lists and data frames, not vectors and
### matrices. So, some flavor of apply statement is often preferred to a loop.

## A more elegant way to summarize the list of fits from above
lapply(fits, summary)

################################################################################
## PRACTICE PROBLEM 7.3
##
## (a) Write a for loop that iterates over the rows of the mtcars dataset. For
##     each row, do the following:
##     - Compute the mean of all variables.
##     - Check if this mean is larger than 20.
##     Save the results of this loop in a logical vector.
##
## (b) Write a single line of code that accomplishes the same as (a) using only
##     vectorized operations (i.e., no looping, no apply statements).
##
################################################################################


###-Conditional Logic--------------------------------------------------------###

### We can build conditional logic into our scripts by using if/else statements.
### In R, these work pretty much the same way as they do in all software.

## Seperate cars with automatic and manual transmissions
auto <- manual <- NULL
for(i in 1:nrow(mtcars)) {
    if(mtcars[i, "am"] == 0)
        auto <- rbind.data.frame(auto, mtcars[i, ])
    else
        manual <- rbind.data.frame(manual, mtcars[i, ])
}

auto
manual

### By include 'else if' options, We can accomodate more conditions.

## Count the number of cars with 4, 6, and 8 cylinders
counts <- rep(0, 3)
for(i in 1:nrow(mtcars)) {
    if(mtcars[i, "cyl"] == 4)      counts[1] <- counts[1] + 1
    else if(mtcars[i, "cyl"] == 6) counts[2] <- counts[2] + 1
    else                           counts[3] <- counts[3] + 1
}

counts

## As with for loops, if/else statements are often not the preferred method to
## incorporate conditional logic when doing applied data analysis. We can often
## get much more elegent solutions with logical indexing.

## A better way to do the file splitting shown above
(auto1   <- mtcars[mtcars$am == 0, ])
(manual1 <- mtcars[mtcars$am == 1, ])

## Or, using dplyr functions
(auto2   <- mtcars %>% filter(am == 0))
(manual2 <- mtcars %>% filter(am == 1))

all.equal(auto, auto1)
all.equal(auto, auto2)

all.equal(manual, manual1)
all.equal(manual, manual2)

## Likewise, for the second example
table(mtcars$cyl)

################################################################################
## PRACTICE PROBLEM 7.4
##
## Write a for loop that iterates over the rows of the mtcars dataset. For each
## row, use an if/else statement to do the following:
## - Print the string "Yay!", if the car has a manual transmission.
## - Print the string "Boo!", if the car has an automatic transmission.
##
################################################################################


###-END----------------------------------------------------------------------###
