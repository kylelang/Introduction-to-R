### Title:    Examples 2: R Data Objects
### Author:   Kyle M. Lang
### Created:  2016-JAN-28
### Modified: 2016-JAN-28

### Basic Data Structures ###

### Vector

## We can make a vector with type equal to any
## of the six basic vector types
v1 <- vector("numeric", 3)
v1

v2 <- vector("integer", 3)
v2

v3 <- vector("double", 3)
v3

v4 <- vector("complex", 3)
v4

v5 <- vector("logical", 3)
v5

v6 <- vector("character", 3)
v6

v7 <- vector("raw", 3)
v7

## Default mode for 'vectors' is logical
x <- vector(length = 5)
x

## The concatination function c() returns a vector populated by its arguments
y <- c(1, 2, 3)
y

y2 <- c("bob", "suzy", "danny")
y2

y3 <- c(TRUE, FALSE, TRUE, TRUE)
y3

## We can check the length of a vector with 'length()'
length(y)

## We can check the storage mode of an object with 'mode()'
mode(y)
mode(y2)
mode(y3)

## We can check the type of an object with 'typeof()'
typeof(y)# numeric data is assumed double
y4 <- c(1L, 2L, 3L)
y4
typeof(y4)

## Clashes of integer and double-precision division result in a double
y5 <- y4 / y
typeof(y5)

## Arithmetic with vectors works element-wise
y <- c(1, 2, 3, 4)
x <- rep(2, 4)

y + x
y - x
y / x
y * x

y - 1
y * 3
y / 3

## Elements are recycled (silently) to make the vectors' lengths match
z <- c(1, 2)
y - z

## We can do check logical conditions for each element in the vector
## by applying the logical test to the vector's name
y == 4
y == 4 | y == 2
y == c(4, 2)# Hmm...that's weird. What's going on here?

## There are many ways to create vectors:
y1 <- c(1, 2, 3)
y1

y2 <- c(1 : 5)
y2

y3 <- rep(33, 4)
y3

y4 <- seq(0, 1, 0.25)
y4

## We can access and re-assign values to the vector
## entries with the '[]' operator
y4[1] <- 77
y4

y4[3]

y4[c(2, 4)] <- -22
y4

y4[2, 4] <- 33# Oops :(

##### PRACTICE PROBLEM 2.1 #####

set.seed(235711)
myVec <- sample(c(1 : 5))

## TASK: Programatically create a logical vector that
## indicates which elements of myVec are less than 3.

##### END PRACTICE PROBLEM 2.1 #####

### Matrices

## Matrices are very similar to vectors but they have two dimensions

m1 <- matrix(1, 3, 3)
m1

## We can assign values with the '[]' operator here too
m2 <- matrix(NA, 3, 3)
m2[1, 2] <- 33
m2[1, 3] <- 44
m2

## Matrices are populated in column-major order, by default
m3 <- matrix(c(1 : 9), 3, 3)
m3

## The 'byrow' option allows us to swith the above to 'row-major' order
m4 <- matrix(c(1 : 9), 3, 3, byrow = TRUE)
m4

## length() of a matrix counts its elements
length(m4)

## We use 'dim()' to get a more sensible measure of dimension
dim(m4)

## Arthmetic is still assumed element-wise
m4 + m3
m4 - m3
m4 / m3
m4 * m3

## "Matrix Multiplaction" has a special operator '%*%'
m4 %*% m3

## All of the other basic matrix operations are avaiable, too
m5 <- matrix(sample(c(1 : 100), size = 9), ncol = 3)
m5
t(m5)# Transpose
solve(m5)# Inverse
svd(m5)# Singular Value Decomposition
eigen(m5)# Eigen Decomposition

## The behavior of R vectors in matrix algebra can be tricky
y <- rnorm(4)
y
x <- matrix(rnorm(16), ncol = 4)
x

y %*% x# y acts like a row-vector
x %*% y# y acts like a column-vector

y
t(y)
t(t(y))

is.matrix(y)
is.matrix(t(y))

## We can get a true column vector with matrix
matrix(y)
t(matrix(y))

##### PRACTICE PROBLEM 2.2 #####

library(mvtnorm)
set.seed(235711)
nObs <- 10000
rSquared <- 0.5
beta <- c(0.25, 0.5, 0.75)
X <- rmvnorm(nObs, rep(0, 3), diag(3))
eta <- X %*% beta
sigma2 <- (var(eta) / rSquared) - var(eta)
y <- eta + rnorm(nObs, 0, sqrt(sigma2))

## TASK: Find the least squares solution for the coefficients
## of the regression of y onto X by hand (see board for hint).

##### END PRACTICE PROBLEM 2.2 #####

## We can select a subset of a matrix with the '[]' operator
m4[1 : 2, 1 : 3]
m6 <- m4[c(1, 3), 2]

## Arithmetic with matrices will also use recycling
m4 + m6


### Lists

l1 <- list(1, 2, 3)
l1

l2 <- list("bob", TRUE, 33, 42+3i)
l2

## List elements have no defualt names, but we can have them
l3 <- list(name = "bob",
           alive = TRUE,
           age = 33,
           relationshipStatus = 42+3i)
l3

l4 <- list()
l4$grass = "green"
l4$money = 0
l4$logical = FALSE
l4$trivial = function(x){x + 77 - 7 * 11}
l4

## The elements inside a list don't really know that
## they live in a list, they'll behave as normal (mostly)
l4$trivial(32)

## We can also assign post hoc names via the 'names()' function
names(l1) <- c("first", "second", "third")
l1

## We can access and modify the elements of a list via
## the '$', '[]', and '[[]]' operators
l3$name
l3[2]
l3["name"]
l3[[2]]
l3[["age"]]

l3[["age"]] <- 57
l3

l3[["name"]] <- TRUE
l3

l3[[1]] <- "suzy"
l3

## What's the difference between [] and [[]]?
class(l3[1])
class(l3[[1]])


##### PRACTICE PROBLEM 2.3 #####

## TASK: (a) Create a list to describe yourself. Include the following
##       named elements in your list:
##       (1) Your Name
##       (3) Your Eye Color
##       (4) Your Hair Color
##       (5) Your Favorite Color
##
##       (b) Using a single command, test if your eyes OR your hair
##       are your favorite color.

##### END PRACTICE PROBLEM 2.3 #####


### Data Frames

d1 <- data.frame(c(1 : 100),
                 c(-100, -1),
                 seq(1, 200, 2)
                 )
d2 <- data.frame(x = c(1 : 100),
                 y = c(-100, -1),
                 z = seq(1, 200, 2)
                 )
d3 <- data.frame(a = sample(c(TRUE, FALSE), 100, replace = TRUE),
                 b = sample(c("foo", "bar"), 100, replace = TRUE),
                 c = runif(100)
                 )
d4 <- data.frame(matrix(NA, 100, 3))

d1
d2
d3
d4

## We can access columns of a data frame with the '$' operator
d3$b

## Data frames are actually lists of vectors (representing the columns)
is.data.frame(d3)
is.list(d3)

## Although they look like rectangular "matrices," from
## R's perspective a data frame IS NOT a matrix
is.matrix(d3)

## Therefore, we cannot do matrix algebra with data frames
d1 %*% t(d2)
as.matrix(d1) %*% t(as.matrix(d2))

## Transposition "works" but with some unanticipated side-effects
d1
t(d1)

class(d1)
class(t(d1))

d3
t(d3)

class(d3)
class(t(d3))

### Assigning Dimnames

## We can assign names to vector and list elements using the 'names()' function
## We can assign row and columns names to matrices and data frames using the
## 'rownames()' and 'colnames()' functions, respectively.

v1 <- c(1 : 3)
names(v1) <- c("n1", "n2", "n3")
v1

m1 <- matrix(rnorm(6), 3, 2)
colnames(m1) <- c("suzy", "timmy")
m1
rownames(m1) <- c("r1", "r2", "r3")
m1

d1 <- data.frame(matrix(rchisq(6, df = 1), 3, 2))
colnames(d1) <- c("cat", "dog")
d1
rownames(d1) <- c("r1", "r2", "r3")
d1

##### PRACTICE PROBLEM 2.4 #####

x <- rep(c(TRUE, FALSE), 25)
y <- rep(1, 50)
z <- rep(2, 50)

## TASK: Create a data frame with 50 rows and 4 columns
##       Make the first column the logical negation of x
##       Make the second and third columns y and z, respectivly
##       Make the fourth column equal y/z (i.e., y divided by z)
##       Give the fourth column a name of your choosing

##### END PRACTICE PROBLEMS 2.4 #####
