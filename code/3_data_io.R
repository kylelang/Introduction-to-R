### Title:    Introduction to R 3: Reading and Writing External Data
### Author:   Kyle M. Lang
### Created:  2022-01-04
### Modified: 2023-01-23

rm(list = ls(all = TRUE))

## Load necessary packages
library(foreign)  # Data I/O from other stats packages
library(haven)    # Data I/O from other stats packages
library(openxlsx) # Powerful data I/O for Excel files
library(readxl)   # Read Excel files
library(labelled) # Working with labelled vectors

## Define the directory holding our data
dataDir <- "data/"


###-Working Directories & RStudio Projects-----------------------------------###

### Every R session is associated with a 'working directory'. The working
### directory is the directory wherein R will root its navigation when reading
### or writing data objects to or from disk.

## Find the current working directory
getwd()

## Change the current working directory
setwd("../")
getwd()

################################################################################
## PRACTICE PROBLEM 3.1
##
## Use the setwd() function to change your working directory to the directory in
## which this script is saved.
##
################################################################################

### We can use RStudio projects to simplify this process and gain additional 
### project-management benefits.

### More information: https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects

################################################################################
## PRACTICE PROBLEM 3.2
##
## Create a new RStudio project, and associate that project with the directory
## in which this script is saved.
##
################################################################################


###-Loading Simple Data------------------------------------------------------###

### We have many ways to read data into R

## Load the built-in 'bfi' data from the 'psychTools' package
data(bfi, package = "psychTools")

## Access the documentation for the 'bfi' data
?psychTools::bfi

################################################################################
## PRACTICE PROBLEM 3.3
##
## (a) Use the data() function to load the 'Cars93' dataset from the 'MASS'
##     package.
## (b) Use the dim() function to check the dimensoins of the 'Cars93' data.
##     - How many rows?
##     - How many columns?
##
################################################################################

## Load the 'boys' data stored in the R workspace '../data/boys.RData'
load(paste0(dataDir, "boys.RData"))

## Load the 'diabetes' data stored in tab-delimited file '../data/diabetes.txt'
diabetes <- read.table(paste0(dataDir, "diabetes.txt"),
                       header = TRUE,
                       sep = "\t")

## Load the 2017 UTMB data from the comma-seperated file '../data/utmb_2017.csv'
utmb1 <- read.csv(paste0(dataDir, "utmb_2017.csv"))

### NOTE: For EU-formatted CSV files, use read.csv2()

## Load the 'titanic' data stored in R data set '../data/titanic.rds'
titanic <- readRDS(paste0(dataDir, "titanic.rds"))

################################################################################
## PRACTICE PROBLEM 3.4
##
## (a) Load the dataset saved as '../data/diabetes.rds'.
## (b) Use the str() function to compare the structure of the data you loaded in
##     (a) to the diabetes data loaded above using the read.table() function.
##     - Are there any differences between these two objects? If so, what are
##       the differences?
##
################################################################################


###-Loading SPSS Data--------------------------------------------------------###

### Reading data in from other stats packages can be a bit tricky. If we want to
### read SAV files, the two most popular options are foreign::read.spss() and
### haven::read_spss().

## Use foreign::read.spss() to read '../data/mtcars.sav' into a list
(mtcars1 <- read.spss(paste0(dataDir, "mtcars.sav")))

attributes(mtcars1)

## Read '../data/mtcars.sav' as a data frame
(mtcars2 <- read.spss(paste0(dataDir, "mtcars.sav"), to.data.frame = TRUE))

attributes(mtcars2)

## Read '../data/mtcars.sav' without value labels
(mtcars3 <- read.spss(paste0(dataDir, "mtcars.sav"),
                      to.data.frame = TRUE,
                      use.value.labels = FALSE)
)

attributes(mtcars3)

## Use haven::read_spss() to read '../data/mtcars.sav' into a tibble
(mtcars4 <- read_spss(paste0(dataDir, "mtcars.sav")))

attributes(mtcars4)

## haven::read_spss() converts SPSS variables with labels into labelled vectors
mtcars4$am
attributes(mtcars4$am)

## Use the labelled::unlabelled() functio to remove the value labels (but not
## the variable labels)
(mtcars5 <- unlabelled(mtcars4))

val_labels(mtcars4)
val_labels(mtcars5)

var_label(mtcars5)

### NOTE: The 'labelled' package provides a bunch of utilities to manipulate the
###       variable and value labels for data coming from, or going to, SPSS.

################################################################################
## PRACTICE PROBLEM 3.5
##
## (a) Use the haven::read_spss() function to load the SPSS dataset saved at
##     '../data/starwars.sav'
## (b) Use the foreign::read.spss() function to load the same dataset as above
##     into a list with variable labels preserved.
## (c) Use the foreign::read.spss() function to load the same dataset as above
##     into a data frame without variable labels.
##
################################################################################


###-Loading Excel Data-------------------------------------------------------###

## Use the readxl::read_excel() function to read the data from the 'titanic'
## sheet of the Excel workbook stored at '../data/example_data.xlsx'
titanic2 <- read_excel(paste0(dataDir, "example_data.xlsx"), sheet = "titanic")

## Use the openxlsx::read.xlsx() function to read the data from the 'titanic'
## sheet of the Excel workbook stored at '../data/example_data.xlsx'
titanic3 <- read.xlsx(paste0(dataDir, "example_data.xlsx"), sheet = "titanic")

str(titanic2)
str(titanic3)

all.equal(as.data.frame(titanic2), titanic3)

################################################################################
## PRACTICE PROBLEM 3.6
##
## (a) Use the openxlsx::read.xlsx() function to load the first 100 rows (not
##     counting column names) of the first 4 columns from the 'diabetes' sheet
##     in the Excel workbook stored at '../data/example_data.xlsx'
## (b) Use the readxl::read_excel() function with an appropriate specification
##     for the 'range' argument to load the chunk of data beginning on Row 3 and
##     Column 2 and ending on Row 100 and Column 7 from the 'titanic' sheet in
##     '../data/example_data.xlsx'
##
################################################################################


###-Writing Simple Data------------------------------------------------------###

### All of the data reading functions we saw earlier have complementary data
### writing versions.

## The save() function writes an R workspace to disk
save(boys, file = paste0(dataDir, "tmp.RData"))

## For delimited text files and RDS data, the write.table(), write.csv(), and
## saveRDS() function do what you'd expect
write.table(boys,
            paste0(dataDir, "boys.txt"),
            row.names = FALSE,
            sep = "\t",
            na = "-999")
write.csv2(boys, paste0(dataDir, "boys.csv"), row.names = FALSE, na = "")
saveRDS(boys, paste0(dataDir, "boys.rds"))


###-Writing SPSS Data--------------------------------------------------------###

## To write SPSS data, the best option is the haven::write_sav() function.
write_sav(mtcars2, paste0(dataDir, "mctars2.sav"))

## write_sav() will preserve label information provided by factor variables and
## the 'haven_labelled' class, but not by attributes
write_sav(mtcars4, paste0(dataDir, "mctars4.sav"))
write_sav(mtcars5, paste0(dataDir, "mctars5.sav"))


###-Writing Excel Data-------------------------------------------------------###

### The 'openxlsx' package provides a powerful toolkit for programmatically
### building Excel workbooks in R and saving the results. Of course, it also
##  works for simple data writing tasks.

## Use the openxlsx::write.xlsx() function to write the 'diabetes' data to an
## XLSX workbook
write.xlsx(diabetes, paste0(dataDir, "diabetes.xlsx"), overwrite = TRUE)

## Use the openxlsx::write.xlsx() function to write each data frame in a list to
## a seperate sheet of an XLSX workbook
write.xlsx(list(titanic = titanic, diabetes = diabetes, mtcars = mtcars),
           paste0(dataDir, "example_data.xlsx"),
           overwrite = TRUE)

