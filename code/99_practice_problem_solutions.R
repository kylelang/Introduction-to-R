### Title:    Introduction to R: Suggested Solutions for Practice Problems
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2025-01-18

rm(list = ls(all = TRUE))

dataDir <- "data"
figDir  <- "figures"

library(haven)
library(openxlsx)
library(readxl)
library(dplyr)
library(magrittr)
library(tidyr)
library(psych)
library(ggplot2)
library(gridExtra)

################################################################################
### 1: Data Manipulation                                                     ###
################################################################################

###-1.1----------------------------------------------------------------------###

## Use base R subsetting procedures to select the five neuroticism items for
## female minors out of the 'bfi' data.

data(bfi, package = "psychTools")

filter <- with(bfi, gender == 2 & age < 18)
bfi[filter, paste0("N", 1:5)]


###-1.2----------------------------------------------------------------------###

## 1. Check the documentation for tidyselect::contains()

?tidyselect::contains

## 2. Use the contains() function to select only the third item from each of the
##    subscales in the 'bfi' data.

bfi1.2 <- select(bfi, contains("3"))


###-1.3----------------------------------------------------------------------###

## Use dplyr subsetting functions to select the same subset as in (4.1)

tmp <- filter(bfi, gender == 2 & age < 18)
select(tmp, starts_with("N"))

## OR (If you peek ahead to the pipes section) ##

bfi %>% filter(gender == 2 & age < 18) %>% select(starts_with("N"))


###-1.4----------------------------------------------------------------------###

## Use the dplyr functions to sort the 'bfi' data on descending order of 'age'
## and ascending order of 'gender'.
## - Sort 'age' within levels of 'gender'

arrange(bfi, gender, -age)


################################################################################
### 2: Data Transformation                                                   ###
################################################################################

###-2.1----------------------------------------------------------------------###

bfi <- mutate(bfi,
              age_std = scale(age)[ , 1],
              education = factor(education,
                                 labels = c("some high school",
                                            "high school graduate",
                                            "some college",
                                            "college graduate",
                                            "graduate degree")
                                 )
              )

## Use dplyr::mutate() to add three new variables to the 'bfi' data
## 1. rootAge = The square root of 'age'
## 2. permEdu = A random permutation of the 'education' factor
## 3. compEdu = A logical vector that contains TRUE for every row where
##              'permEdu' = 'education' and FALSE otherwise

## HINTS: 
## - You can use the sample() function to permute the contents of an R vector
## - You can use the tidyr::replace_na() function to replace missing values with
##   some non-missing value

bfi2.1 <- mutate(bfi,
                 rootAge = sqrt(age),
                 permEdu = sample(education),
                 compEdu = replace_na(permEdu == education, FALSE)
                 )


###-2.2----------------------------------------------------------------------###

names(bfi.keys) <- c("agree", "consc", "extra", "neuro", "open")
scores          <- scoreVeryFast(bfi.keys, bfi)
bfi             <- data.frame(bfi, scores)

## Use the dplyr::mutate() function to create standardized versions of the five
## scales scores we just created.

bfi2.2 <- mutate(bfi, across(agree:open, scale, .names = "std_{.col}"))


###-2.3----------------------------------------------------------------------###

## Use dplyr::mutate() and case_when() to create a new factor called 'col_grad'
## that satisfies the following logic
## - col_grad = "no" when 'edu' is "some high school", "high school graduate",
##   or "some college"
## - col_grad = "yes" when 'edu' is "college graduate" or "graduate degree"
## - col_grad = is missing, otherwise

bfi <- readRDS(here::here(dataDir, "bfi.rds"))

## Tell mutate() to create a factor variable directly:
bfi2.3 <-
    mutate(bfi,
           col_grad = case_when(
               edu %in% c("some high school", "high school graduate", "some college") ~ factor("no"),
               edu %in% c("college graduate", "graduate degree") ~ factor("yes"),
               TRUE ~ NA
           )
    )

## Create a character vector and convert it to a factor in a separate step:
bfi2.3 <-
    mutate(bfi,
           col_grad = case_when(
               edu %in% c("some high school", "high school graduate", "some college") ~ "no",
               edu %in% c("college graduate", "graduate degree") ~ "yes",
               TRUE ~ NA
           ),
           col_grad = factor(col_grad)
    )

## Create a factor by converting a character vector to a factor with a pipeline:
bfi2.3 <-
    mutate(bfi,
           col_grad = case_when(
               edu %in% c("some high school", "high school graduate", "some college") ~ "no",
               edu %in% c("college graduate", "graduate degree") ~ "yes",
               TRUE ~ NA
           ) |> factor()
    )


###-2.4----------------------------------------------------------------------###

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

bfi2.4 <- mutate(bfi,
                 type = case_when(
                     open > mO & extra > mE ~ "adventurous",
                     open > mO & extra <= mE ~ "inquisitive",
                     open <= mO & extra <= mE ~ "quiet",
                     open <= mO & extra > mE ~ "chatty")
                 )


###-2.5----------------------------------------------------------------------###

## Use an appropriate apply function to convert all the variable names in the
## 'bfi.keys' list to lower case.
## - Do all the conversions with a single command
## - Return the resulting object as a list

bfiKeys <- lapply(bfi.keys, tolower)


################################################################################
### 3: Pipes                                                                 ###
################################################################################

###-3.1----------------------------------------------------------------------###

## Use a pipeline to calculate the square root of the mean of the agreeableness
## scale score for males in the 'bfi' data.
##
## TIP: You can use the unlist() function to covert a list to a vector.

bfi %>% filter(gender == 1) %>% select(agree) %>% unlist() %>% mean() %>% sqrt()

## OR, if you peek ahead to exposition pipes ##

bfi %>% filter(gender == 1) %$% mean(agree) %>% sqrt()


###-3.2----------------------------------------------------------------------###

## Create a single pipeline that completes the following data processing steps
## on the 'bfi' data.

## 1. Create a new variable, 'open4', by centering the 'open' variable on 4
## 2. Create a new variable, 'oaProd', that contains the product of 'open4' and
##    'agree'
## 3. Select only the rows for which 'oaProd' > 0
## 4. Select only the following items:
##    - The five agreeableness items
##    - The five openness items
##    - 'agree', 'open', and 'open4'

bfi3.2 <- bfi |>
    mutate(open4 = open - 4,
           oaProd = open4 * agree) |>
    filter(oaProd > 0) |> 
    select(a1:a5, o1:o5, agree, open, open4)


###-3.3----------------------------------------------------------------------###

## Use the pipe and exposition pipe to calculate the correlation between 'age'
## and 'agree' for adults in the 'bfi' data.

## - Your pipeline should return only a single correlation, not an entire
##   correlation matrix

## HINT: You can use the cor() function to compute the correlation between two
##       variables.

bfi %>% filter(age > 18) %$% cor(age, agree)


################################################################################
### 4: Data Visualization                                                    ###
################################################################################

###-4.1----------------------------------------------------------------------###

## Use base R graphics and the 'titanic' data to create conditional boxplots,
## where plots of 'age' are conditioned on 'survived'.
## - What does this figure tell you about the ages of survivors ('survived' = 1)
##   vs. non-survivors ('survived' = 0)?

titanic <- readRDS(here::here(dataDir, "titanic.rds"))

boxplot(age ~ survived, data = titanic)

## There is not much difference in the age of survivors and non-survivors.


###-4.2----------------------------------------------------------------------###

## Use GGPlot and the 'diabetes' data to create an empty plot of total
## cholesterol, 'tc', (on the y-axis) against 'age' (on the x-axis).
## - Don't add any geoms yet.
## - Assign the resulting plot object to a variable in your environment.

diabetes <- readRDS(here::here(dataDir, "diabetes.rds"))

(p4.2 <- ggplot(diabetes, aes(age, tc)))


###-4.3----------------------------------------------------------------------###

## Augment the plot you created in 4.2 to create a scatterplot.
## - Map the size of the points to 'bmi'
## - Assign the resulting plot object to a variable in your environment.

(p4.3 <- p4.2 + geom_point(aes(size = bmi)))


###-4.4----------------------------------------------------------------------###

## Augment the plot you created in 4.3 by adding RUG lines to both the x-axis
## and y-axis.
## - Map the color of the RUG lines to 'glu'
## - Assign the resulting plot object to a variable in your environment.

(p4.4 <- p4.3 + geom_rug(aes(color = glu)))


###-4.5----------------------------------------------------------------------###

## Augment the plot you created in 4.4 by adding linear regression lines.
## - Add separate lines for males and females.
## - Differentiate the regression lines by giving them different line types.
## - Do not include the SE bands.
## - Assign the resulting plot object to a variable in your environment.

(p4.5 <- p4.4 + geom_smooth(aes(linetype = sex), method = "lm", se = FALSE))


###-4.6--------------------------------------------------------------------=-###

## Modify the plot that you created in 4.5 by adjusting the theme.
## - Change the global theme to the "classic" theme.
## - Convert all text to 14-point, serif font.

(p4.6 <- p4.5 +
     theme_classic() +
     theme(text = element_text(family = "serif", size = 14))
)

###-4.7----------------------------------------------------------------------###

## Use the 'titanic' data, GGPlot, and faceting to create conditional
## histograms of 'age' conditioned on 'survived'.
## - Adjust the number of bins to optimize the clarity of the visualization.
## - Overlay kernel density plots on each histogram.
## - Do you think this figure is a more effective visualization than the
##   conditional boxplots you created in 4.1? Why or why not?
##
## HINT: You can get ggplot to scale your histogram in proportions, rather than
##       counts, by specifying the argument "y = after_stat(density)" for the y
##       aesthetic in an appropriate geom.

(p4.7 <- ggplot(titanic, aes(age)) +
     geom_histogram(aes(y = after_stat(density))) +
     geom_density() +
     facet_wrap(vars(survived))
)

(p4.7 <- ggplot(titanic, aes(age)) +
     geom_histogram(aes(y = after_stat(density)), bins = 10) +
     geom_density() +
     facet_wrap(vars(survived))
)

(p4.7 <- ggplot(titanic, aes(age)) +
     geom_histogram(aes(y = after_stat(density)), bins = 50) +
     geom_density() +
     facet_wrap(vars(survived))
)

(p4.7 <- ggplot(titanic, aes(age)) +
     geom_histogram(aes(y = after_stat(density)), bins = 20) +
     geom_density() +
     facet_wrap(vars(survived))
)

## Yes. These histograms show a spike of very young survivors.


###-4.8----------------------------------------------------------------------###
##
## Use ggplot() and grid.arrange() to create the two plots described below and
## organize the plots into a 1x2 array (i.e., 1 row and 2 columns).
##
## Using the diabetes data, create two plots. Both plots should begin with a 
## histogram of blood glucose level ('glu').
## - In the first plot, overlay the kernel density plot for 'glu' as a blue line.
## - In the second plot, overlay the theoretical normal density as a red line.
##
## HINTS:
## - You can calculate the values for the normal density with the dnorm()
##   function.
## - Don't forget to define the appropriate mean and SD.

p4.8 <- diabetes %>%
    mutate(density = dnorm(glu, mean(glu), sd(glu))) %>%
    ggplot(aes(glu)) +
    geom_histogram(aes(y = after_stat(density)),
                   color = "black",
                   fill = "lightgray",
                   bins = 15) +
    theme_classic()

grid.arrange(
    p4.8 + geom_density(color = "blue"),
    p4.8 + geom_line(aes(glu, density), col = "red"),
    ncol = 2
)


###-4.9----------------------------------------------------------------------###

## Save the figure that you created in 4.7 as a JPEG
## - Adjust the size to 10cm X 10cm
## - Set the resolution to 800
## - Save the image to the "/figures/" directory

jpeg(here::here(figDir, "practice_problem_4_9.jpg"),
     width  = 10,
     height = 10,
     units  = "cm",
     res    = 800)

p4.7

dev.off()


###-4.10---------------------------------------------------------------------###

## (a) Save the five figures you created in (4.2 - 4.6) to a single PDF file.

pdf(here::here(figDir, "practice_problem_4_10a.pdf"))

p4.2
p4.3
p4.4
p4.5
p4.6

dev.off()

## (b) Save the five figures you created in (4.2 - 4.6) to a separate PNG files.
##     - Save both the PDF and the PNG files to the "figures" directory

png(here::here(figDir, "practice_problem_4_10b-%d.png"))

p4.2
p4.3
p4.4
p4.5
p4.6

dev.off()


###-END----------------------------------------------------------------------###
