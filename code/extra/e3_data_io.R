### Title:    Introduction to R Extra 3: Reading and Writing External Data
### Author:   Kyle M. Lang
### Created:  2022-01-04
### Modified: 2026-01-16

rm(list = ls(all = TRUE))

## Load necessary packages
library(readr)    # Data I/O for delimited files
library(haven)    # Data I/O from other stats packages
library(labelled) # Working with labelled vectors
library(readxl)   # Read Excel files
library(openxlsx) # Powerful data I/O for Excel files

## Define the directory holding our data
dataDir <- "data"


###-Working Directories & RStudio Projects-----------------------------------###

### Every R session is associated with a 'working directory'. The working
### directory is the directory wherein R will root its navigation when reading
### or writing data objects to or from disk.

## Find the current working directory
getwd()

## Change the current working directory
setwd("../")
getwd()

### You should usually avoid manually changing the working directory.
### We're better off using RStudio projects to simplify this process and gain
### additional project-management benefits.

### More information: https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects

################################################################################
## PRACTICE PROBLEM 3.1
##
## Create a new RStudio project associated with the directory that you want to
## use as the working directory for these exercises.
##
################################################################################


###-Loading Delimited Data---------------------------------------------------###

### The readr package provides several powerful functions for reading delimited
### data files.

## Use readr::read_delim() to load the 'boys' data stored in space-delimited
## file '/data/boys.dat'
boys <- read_delim(here::here(dataDir, "boys.dat"))
boys

## Specify the missing data code
boys <- read_delim(here::here(dataDir, "boys.dat"), na = "-999")
boys

## Use readr::read_delim() to load the 'diabetes' data stored in tab-delimited
## file '/data/diabetes.txt'
diabetes <- read_delim(here::here(dataDir, "diabetes.txt"), delim = "\t")
diabetes

## Use readr::read_tsv() to do the same as above
diabetes <- read_tsv(here::here(dataDir, "diabetes.txt"))
diabetes

## Use readr::read_csv() to load the 2017 UTMB data from the comma-separated
## file '/data/utmb_2017.csv'
utmb <- read_csv(here::here(dataDir, "utmb_2017.csv"))
utmb

## Drop the first column
utmb <- read_csv(here::here(dataDir, "utmb_2017.csv"), col_select = -1)
utmb
str(utmb)
spec(utmb)

## Specify some columns types
utmb <- read_csv(
  file       = here::here(dataDir, "utmb_2017.csv"),
  col_select = -1,
  col_types  = cols(
    bib         = col_character(),
    category    = col_factor(),
    rank        = col_integer(),
    nationality = col_factor()
  )
)
utmb
str(utmb)

## Use readr::read_csv2() to load an EU-formatted CSV file:
boys <- read_csv2(here::here(dataDir, "boys_eu.csv"))
boys

## Specify column types:
boys <- read_csv2(here::here(dataDir, "boys_eu.csv"), col_types = "dddddffdf")
boys
str(boys)

### Base R includes analogous functions, but they're not as powerful.

diabetes2 <- read.table(here::here(dataDir, "diabetes.txt"),
                        header = TRUE,
                        sep = "\t")
utmb2 <- read.csv(here::here(dataDir, "utmb_2017.csv"))
boys2 <- read.csv2(here::here(dataDir, "boys_eu.csv"))


###-Loading R Data-----------------------------------------------------------###

### R has two native data types.

## Load the 'boys' data stored in the R workspace '/data/boys.RData'
load(here::here(dataDir, "boys.RData"))

## Load the 'titanic' data stored in R data set '/data/titanic.rds'
titanic <- readRDS(here::here(dataDir, "titanic.rds"))

################################################################################
## PRACTICE PROBLEM 3.2
##
## (a) Load the dataset saved as '/data/diabetes.rds'.
## (b) Use the str() function to compare the structure of the data you loaded in
##     (a) to the 'diabetes2' dataset loaded above.
##     - Are there any differences between these two objects? If so, what are
##       the differences?
##
################################################################################


###-Loading SPSS Data--------------------------------------------------------###

## Use haven::read_spss() to read '/data/mtcars.sav' into a tibble
(mtcars1 <- read_spss(here::here(dataDir, "mtcars.sav")))

attributes(mtcars1)

## haven::read_spss() converts SPSS variables with labels into labelled vectors
mtcars1$am
attributes(mtcars1$am)

## Use the labelled::unlabelled() function to remove the value labels (but not
## the variable labels)
(mtcars2 <- unlabelled(mtcars1))

val_labels(mtcars1)
val_labels(mtcars2)

var_label(mtcars2)

### NOTE: The 'labelled' package provides a bunch of utilities to manipulate the
###       variable and value labels for data coming from, or going to, SPSS.

################################################################################
## PRACTICE PROBLEM 3.3
##
## Use the haven::read_spss() function to load the SPSS dataset saved at
## '/data/starwars.sav'
##
################################################################################


###-Loading Excel Data-------------------------------------------------------###

## Use the readxl::read_excel() function to read the data from the 'titanic'
## sheet of the Excel workbook stored at '/data/example_data.xlsx'
titanic2 <- read_excel(here::here(dataDir, "example_data.xlsx"), sheet = "titanic")

## Use the openxlsx::read.xlsx() function to read the data from the 'titanic'
## sheet of the Excel workbook stored at '/data/example_data.xlsx'
titanic3 <- read.xlsx(here::here(dataDir, "example_data.xlsx"), sheet = "titanic")

str(titanic2)
str(titanic3)

################################################################################
## PRACTICE PROBLEM 3.4
##
## (a) Use the openxlsx::read.xlsx() function to load the first 100 rows (not
##     counting column names) of the first 4 columns from the 'diabetes' sheet
##     in the Excel workbook stored at '/data/example_data.xlsx'
## (b) Use the readxl::read_excel() function with an appropriate specification
##     for the 'range' argument to load the chunk of data beginning on Row 3 and
##     Column 2 and ending on Row 100 and Column 7 from the 'titanic' sheet in
##     '/data/example_data.xlsx'
##
################################################################################


###-Writing Delimited Data---------------------------------------------------###

### For delimited text files, each read_*() function in readr has an analogous
### write_*() function

## Save 'boys' as a space-delimited file
write_delim(boys,
            here::here(dataDir, "boys.dat"),
            na = "-999")

## Save 'boys' as a tab-delimited file
write_delim(boys,
            here::here(dataDir, "boys.txt"),
            delim = "\t",
            na = "-999")

## Same as above
write_tsv(boys, here::here(dataDir, "boys.txt"), na = "")

## Save 'boys' as two different flavors of comma-separated values files
write_csv(boys, here::here(dataDir, "boys_us.csv"), na = "")
write_csv2(boys, here::here(dataDir, "boys_eu.csv"), na = "")


###-Writing R Data-----------------------------------------------------------###

## The save() function writes an R workspace to disk
save(boys, file = here::here(dataDir, "tmp.RData"))

## The saveRDS() function does what you'd expect
saveRDS(boys, here::here(dataDir, "boys.rds"))


###-Writing SPSS Data--------------------------------------------------------###

## To write SPSS data, the best option is the haven::write_sav() function.
write_sav(mtcars1, here::here(dataDir, "mctars1.sav"))

## write_sav() will preserve label information provided by factor variables and
## the 'haven_labelled' class, but not by attributes
write_sav(mtcars2, here::here(dataDir, "mctars2.sav"))


###-Writing Excel Data-------------------------------------------------------###

### The 'openxlsx' package provides a powerful toolkit for programmatically
### building Excel workbooks in R and saving the results. Of course, it also
### works for simple data writing tasks.

## Use the openxlsx::write.xlsx() function to write the 'diabetes' data to an
## XLSX workbook
write.xlsx(diabetes, here::here(dataDir, "diabetes.xlsx"), overwrite = TRUE)

## Use the openxlsx::write.xlsx() function to write each data frame in a list to
## a separate sheet of an XLSX workbook
write.xlsx(list(titanic = titanic, diabetes = diabetes, mtcars = mtcars),
           here::here(dataDir, "example_data.xlsx"),
           overwrite = TRUE)


###-END----------------------------------------------------------------------###
