### Title:    Examples 3: Simple R Programming
### Author:   Kyle M. Lang
### Created:  2016-JAN-29
### Modified: 2016-JAN-29

### Functions

squareX <- function(x) {
    out <- x^2
    out
}
squareX(5)

## We don't need an explict return() statement
## R functions will return any object that is 'printed' as
## the last command inside the function (e.g., 'out' in the above example)

## One-line functions don't need brackets
squareX2 <- function(x) x^2
squareX2(5)
squareX(c(1 : 5))
squareX("bob")

## We can specify multiple arguments
getMod <- function(x, y) x %% y
getMod(10, 3)

## We can specify list arguments that we unpack inside the function
getLsBeta <- function(datList) {
    X <- datList$X
    y <- datList$y

    solve(crossprod(X)) %*% t(X) %*% y
}

datList <- list()
datList$X <- matrix(runif(500), ncol = 5)
datList$y <- X %*% rep(0.5, 5)

getLsBeta(datList = datList)


##### PRACTICE PROBLEM 3.1 #####

## TASK: Write a 'hello world' function

##### END PRACTICE PROBLEM 3.1 #####


## Functions are just objects just like anything else we've been working with
class(getLsBeta)
typeof(getLsBeta)

## R views an unevaluated function as a special object with type "closure"
## but evaluated functions are simply equivelent to return objects.
## So, we can acutally use evaluated functions as arguments to
## other operators / functions

fun1 <- function(x, y) {
    x + y
}

typeof(fun1)
typeof(fun1(1, 1))

fun1(1, fun1(1, 1))# What will this command return?

## Why would we care?
myVar <- var(runif(100))
x <- rnorm(100, 0, sqrt(myVar))

## Let's get back to that Lexical Scoping business I mentioned yesterday

rm(list = ls(all = TRUE))

## What do you think the following function will do?
fun2 <- function(x) {
    fun1 <- function(y) {
        y - x
    }
    fun1(y = x)
}

fun2(x = 10)


fun1 <- function(y) {
    y - x
}

fun1(y = x)# Uh-oh :(


## When encapsulated in the comforting embrace of fun2(),
## fun1() can use "lexical scoping" to find the missing
## variable that it needs (i.e., x), but fun1() can't
## survive in the outside world becuase we're neglectful
## creators that didn't give it all of the tools needed
## for its job.


##### PRACTICE PROBLEM 3.2 ######

## TASK: Write a function with two arguments that takes a vector of
##       data and a vector of weights (with length equal to the data vector)
##       and returns the weighted mean of the elements in the data vector.

##### END PRACTICE PROBLEM 3.2 #####


### Loops

## There are three types of loops in R: for, while, until
## In most situtations, the for loop is the only version you'll
## need, so we're not going to discuss while and until loops

## Sum the numbers from 1 to 100:
val <- 0
for(i in 1 : 100) {
    val <- val + i
}
val

## Compute every variable mean of a dataset:
dat1 <- matrix(rnorm(1000), ncol = 10)
meanVec <- rep(0, ncol(dat1))
for(j in 1 : ncol(dat1)) {
    meanVec[i] <- mean(dat1[ , j])
}

## Regress each variable in a dataset onto all others:
outList <- list()
for(v in 1 : ncol(dat1)) {
    outList[[v]] <- lm(dat1[ , v] ~ dat1[ , -v])
}

## Summarize the previous results:
sumList <- list()
for(bob in 1 : length(outList)) sumList[[bob]] <- summary(outList[[bob]])
sumList

## More direct versin of the above:
for(ol in outList) print(summary(ol))

## Loops are often one of the least efficient solutions in R
n <- 1e7
startTime <- proc.time()
val <- 0
for(i in 1 : n) {
    val <- val + i
}
badTime <- proc.time() - startTime

goodTime <- system.time(
    val2 <- sum(as.numeric(c(1 : n)))
    )

val2 - val# Same answer
badTime / goodTime# But many times slower

## There is often a builtin routine for what your trying to accomplish
## with the loop. A quick google search will often find these routines

## The appropriate way to get variable means:
colMeans(dat1)


##### PRACTICE PROBLEM 3.3 #####

n <- 100
v <- 10
dat1 <- matrix(sample(c(1 : 7), n*v, replace = TRUE), ncol = v)
## TASK: Write a for loop that loops over the rows of dat1, counts the
##       number of 1s in each row and stores the nth row's count of 1s
##       in the nth element of an integer vector.
## HINT: Remember what happens when you sum a logical vector.

##### END PRACTICE PROBLEM 3.3 #####


### Apply Functions

## Loops are often not used by knowledgable R programmers because
## 'apply' functions are the preferred method of applying the same
## procedure to each row, column, cell, or list element

## If we're using a for loop, we're often just applying the same
## operation to each column or row of a data set. The 'apply' family
## of functions are more efficient ways to do this.

## Two flavors of apply are most useful (I think): apply and lapply
?apply
?lapply

## Compute order statistics for every variable in the dataset
n <- 100
v <- 10
dat1 <- matrix(rnorm(n*v), ncol = v)
orderStatMat <- apply(dat1, 2, quantile, probs = c(0.25, 0.5, 0.75))

## A better way to summarize the lm output from above:
sumList2 <- lapply(outList, summary)
all.equal(sumList2, sumList)# same results but much more elegent


##### PRACTICE PROBLEM 3.4 #####

## TASK: (a) Use an lapply statement to find the type of each element
##       in the list you created for PP 2.3.
##       (b) Run the lapply statement you created for part (a) on the
##       data frame you created for PP 2.4. What do you notice?

##### END PRACTICE PROBLEM 3.4 #####


### If Else Statements

## We can build conditional logic into our scripts by using if/else statements
## These work pretty much the same way as they do in all software, but there
## are a few quirks of the R version

## Seperate male and female data:
dat1 <- data.frame(gender = sample(c("male", "female"), 100, replace = TRUE),
                   x = rnorm(100),
                   y = runif(100),
                   z = rpois(100, 1)
                   )

maleData <- femaleData <- NULL
for(i in 1 : nrow(dat1)) {
    if(dat1[i, "gender"] == "male") {
        maleData <- rbind.data.frame(maleData, dat1[i, ])
    } else {
        femaleData <- rbind.data.frame(femaleData, dat1[i, ])
    }
}

maleData
femaleData

## We can also have more conditions:

## Count the number of dogs, cats, and mongeese
vec1 <- sample(c("dog", "dog", "dog",
                 "cat", "cat",
                 "mongoose"),
               500, replace = TRUE)

dogCount <- catCount <- mongooseCount <- 0
for(i in 1 : length(vec1)) {
    if(vec1[i] == "dog") dogCount <- dogCount + 1
    else if(vec1[i] == "cat") catCount <- catCount + 1
    else mongooseCount <- mongooseCount + 1
}

dogCount
catCount
mongooseCount

## As with for loops, if/else statements are often not the preferred
## method to incorporate conditional logic when doing applied data analysis.
## We can often get much more elegent solutions with logical indexing.

## A better way to do the file splitting shown above

maleData2 <- dat1[dat1$gender == "male", ]
femaleData2 <- dat1[dat1$gender == "female", ]

all.equal(maleData, maleData2)
all.equal(femaleData, femaleData2)# Same answers but so much more elegent!

## Likewise, for the second example
table(vec1)
