### Title:    Introduction to R 2: Data Objects
### Author:   Kyle M. Lang
### Created:  2016-01-28
### Modified: 2023-01-23

rm(list = ls(all = TRUE))

###-Vectors------------------------------------------------------------------###

### Vectors are the simplest kind of object in R. Vectors come in one of six
### "atomic modes":
### 1. double/numeric
### 2. logical
### 3. character
### 4. integer
### 5. complex
### 6  raw

## We can make a vector with any of the six basic vector modes
v1 <- vector("numeric", 3)
v1

v1 <- vector("double", 3)
v1

v2 <- vector("logical", 3)
v2

v3 <- vector("character", 3)
v3

v4 <- vector("integer", 3)
v4

v5 <- vector("complex", 3)
v5

v6 <- vector("raw", 3)
v6

## There are many different ways to generate vectors
(y1 <- c(1, 2, 3))
(y2 <- c(TRUE, FALSE, TRUE, TRUE))
(y3 <- c("bob", "suzy", "danny"))

1:5
1.2:5.3

rep(33, 4)
rep(1:3, 3)
rep(y3, each = 2)

seq(0, 1, 0.25)
seq(-1, 1, length.out = 10)

### NOTE: The outer parantheses above allow me to assign an object and print its
###       value in a single command

################################################################################
## PRACTICE PROBLEM 2.1
##
## Create a numeric vector containing the five even integers between 2 and 10
## (inclusive).
##
################################################################################

## We can check the length of a vector with length()
length(y1)

## Arithmetic with vectors works element-wise
(y <- 1:4)
(x <- rep(2, 4))

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

## We can check logical conditions for each element in the vector by applying
## the logical test to the vector's name
y == 4
y == 4 | y == 2
y == c(4, 2)# Hmm...that's weird. What's going on here?

## We can access and assign values to vector entries with the '[]' operator
y

y[3]

y[1] <- 77
y

## To select multiple elements, we subset with another vector
y[c(2, 4)] <- -22
y

y[2, 4] <- 33# Oops :(

################################################################################
## PRACTICE PROBLEM 2.2
##
## set.seed(235711)
## myVec <- sample(1:5)
##
## (a) Create the object 'myVec' by uncommenting and running the preceding two
##     lines of code.
## (b) Programatically create a logical vector that indicates which elements of
##     myVec are less than 3.
##
################################################################################


###-Matrices-----------------------------------------------------------------###

### Matrices are just vectors with a dimension attribute

## We create matrices using the matrix() function
(m1 <- matrix(data = 1, nrow = 3, ncol = 3))

attributes(y1)
attributes(m1)

## Matrices are populated in column-major order, by default
(m2 <- matrix(1:9, 3, 3))

## The 'byrow' option allows us to fill by row-major order
(m3 <- matrix(1:9, 3, 3, byrow = TRUE))

## length() of a matrix counts its elements
length(m3)

## We use dim() to get a more sensible measure of dimension
dim(m3)

## Arthmetic is still assumed element-wise
m3 + m2
m3 - m2
m3 / m2
m3 * m2

## We can access and assign values with the '[]' operator
m3[1:2, 1:3]
(m4 <- m3[c(1, 3), 2])

m1[1, 2] <- 33
m1[2:3, c(1, 3)] <- 44
m1

## Arithmetic with matrices will also use recycling
m2
m4
m2 + m4

################################################################################
## PRACTICE PROBLEM 2.3
##
## (a) Create a 5x3 numeric matrix called 'myMat' wherein each column is equal
##     to the vector 'myVec' that you created for Problem 2.2.
## (b) Multiply each entry in 'myMat' by pi (i.e., the numerical constant).
##
## HINT: The built-in R object 'pi' contains the value of pi.
##
################################################################################


###-Lists--------------------------------------------------------------------###

### Lists are the workhorse of R data objects. An R list can hold an arbitrary
### set of other R objects.

## We create lists using the list() function
(l1 <- list(1, 2, 3))
(l2 <- list("bob", TRUE, 33, 42+3i))

## List elements have no defualt names, but we can define our own
(l3 <- list(name = "bob",
            alive = TRUE,
            age = 33,
            relationshipStatus = 42+3i)
)

## We can also assign post hoc names via the 'names()' function
names(l1) <- c("first", "second", "third")
l1

## We can access and modify the elements of a list via the '$', '[]', and
## '[[]]' operators
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
l3[1]
l3[[1]]

class(l3[1])
class(l3[[1]])

## We can append new elements onto an existing list:
(l4 <- list())
l4$grass <- "green"
l4$money <- 0
l4$logical <- FALSE
l4$addOne <- function(x){x + 1}
l4

## The elements inside a list don't really know that they live in a list;
## they'll pretty much behave as normal
l4$addOne(32)

################################################################################
## PRACTICE PROBLEM 2.4
##
## (a) Create a list to describe yourself. Include the following named elements
##     in your list:
##     (1) Your Name
##     (3) Your Eye Color
##     (4) Your Hair Color
##     (5) Your Favorite Color
##
## (b) Using a single command, test if your eye color OR your hair color is also
##     your favorite color.
##
################################################################################


###-Data Frames--------------------------------------------------------------###

### Data frames are R's way of storing rectangular data sets. Each column of a
### data frame is a vector; each of these vectors can have a different type.

## We create data frames using the data.frame() function
(d1 <- data.frame(1:10, c(-1, 1), seq(0.1, 1, 0.1)))

(d2 <- data.frame(x = 1:10, y = c(-1, 1), z = seq(0.1, 1, 0.1)))

(d3 <- data.frame(a = sample(c(TRUE, FALSE), 10, replace = TRUE),
                  b = sample(c("foo", "bar"), 10, replace = TRUE),
                  c = runif(10)
                  )
)

(d4 <- data.frame(matrix(NA, 10, 3)))

## Data frames are actually lists of vectors (representing the columns)
is.data.frame(d3)
is.list(d3)

## Although they look like rectangular "matrices", from R's perspective a data
## frame IS NOT a matrix
is.matrix(d3)

## We cannot treat a data frame like a matrix. E.g., matrix algebra doesn't work
## with data frames
d1 %*% t(d2)
as.matrix(d1) %*% t(as.matrix(d2))

## We can access columns of a data frame using any method we would use to access
## the elements of a list
d3$b
d3["b"]
d3[["b"]]

d3[2]
d3[[2]]

## We can also access the elements of a data frame using matrix-style subsetting
d3[1, 2]
d3[ , 2]
d3[1, ]


###-Dimnames-----------------------------------------------------------------###

## We can assign names to vector and list elements using the names() function
(v1 <- c(1 : 3))
names(v1) <- c("n1", "n2", "n3")
v1

attributes(v1)

(l1 <- list("foo", TRUE, pi))
names(l1) <- c("bar", "not_false", "tasty")
l1

## We can assign row and column names to matrices and data frames using the
## rownames() and colnames() functions, respectively.
(m1 <- matrix(rnorm(6), 3, 2))
colnames(m1) <- c("suzy", "timmy")
m1
rownames(m1) <- paste("row", 1:3, sep = "_")
m1

(d1 <- data.frame(matrix(runif(6), 3, 2)))
colnames(d1) <- c("cat", "dog")
d1
rownames(d1) <- paste0("case", 1:3)
d1

## Since data frames are also lists, we can also use the names() function to
## name the columns of a data frame
names(d1) <- paste("Column", LETTERS[1:2], sep = ":")
d1

################################################################################
## PRACTICE PROBLEM 2.5
##
## x <- rep(c(TRUE, FALSE), 10)
## y <- rep(1, 20)
## z <- rep(2, 20)
##
## (a) Create the vectors x, y, and z by uncommented and running the preceding
##     three lines of code.
## (b) Create a data frame called 'myDf' with 20 rows and 4 columns
##     - Make the first column the logical negation of 'x'
##     - Make the second and third columns 'y' and 'z', respectivly
##     - Make the fourth column equal y/z (i.e., 'y' divided by 'z')
## (b) Use the paste() function to name the columns var-1, var-2, var-3, var-4.
## (c) Name the rows with the first twenty letters of the English alphabet.
##
################################################################################


###-Factors------------------------------------------------------------------###

### We use factors to represent categorical variables (i.e., grouping factors).

## We can create a factor using the factor() function
(f1 <- factor(sample(1:3, 20, TRUE), labels = c("red", "yellow", "blue")))

## Factors are stored as integer vectors with a levels attribute and a special
## 'factor' class
typeof(f1)
attributes(f1)

## The levels are just group labels
levels(f1)

## Even though a factor's data are represented by an integer vector, R does not
## consider factors to be integer/numeric data
is.numeric(f1)
is.integer(f1)

## Since factors represent nominal variables, we cannot do math with factors
f1 + 1
mean(f1)

################################################################################
## PRACTICE PROBLEM 2.6
##
## (a) Create a length-20 factor with two levels = {"yes", "no"}.
## (b) Add the factor you created in (a) to the data frame you created in (2.5)
##     as a new column called "f".
##
################################################################################


###-END----------------------------------------------------------------------###
