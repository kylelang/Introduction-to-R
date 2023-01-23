### Title:    Introduction to R 6: Data Visualization
### Author:   Kyle M. Lang
### Created:  2022-01-04
### Modified: 2023-01-23

rm(list = ls(all = TRUE))

library(ggplot2)
library(dplyr)
library(magrittr)
library(gridExtra)

dataDir  <- "data/"
figDir   <- "figures/"

diabetes <- readRDS(paste0(dataDir, "diabetes.rds"))
titanic  <- readRDS(paste0(dataDir, "titanic.rds"))
bfi      <- readRDS(paste0(dataDir, "bfi.rds"))

## Convert survival indicator to a numeric dummy code:
titanic <- titanic %>% mutate(survived = as.numeric(survived) - 1)


###-Base R Graphics----------------------------------------------------------###

### Base R already comes with many plotting capabilities

## Basic scatter plot using the plot() function
diabetes %$% plot(y = tc, x = bmi)

diabetes %$% plot(y = tc,
                  x = bmi,
                  ylab = "Total Cholesterol",
                  xlab = "Body Mass Index",
                  main = "Relation between BMI and Cholesterol",
                  ylim = c(0, 350),
                  xlim = c(0, 50)
                  )

## Simple histogram with the hist() function
hist(diabetes$glu)

hist(diabetes$glu, breaks = 5)
hist(diabetes$glu, breaks = 50)

hist(diabetes$glu, col = "pink", border = "blue", probability = TRUE)

## Simple boxplots via the boxplot() function
boxplot(diabetes$progress)

boxplot(diabetes$progress,
        horizontal = TRUE,
        range = 3,
        xlab = "Disease Progression")

boxplot(progress ~ sex, data = diabetes, col = "violet")

################################################################################
## PRACTICE PROBLEM 6.1
##
## Use base R graphics and the 'titanic' data to create conditional boxplots,
## where plots of 'age' are conditioned on 'survived'.
## - What does this figure tell you about the ages of surivors ('survived' = 1)
##   vs. non-survivors ('survived' = 0)?
##
################################################################################

## If we apply the plot() function to a dataframe, we get a scatterplot matrix
diabetes %>% select(age, bmi, tc, glu, progress) %>% plot()

## The density() function will estimate the density of a continuous variable
d <- density(diabetes$bmi)

## If we plot a density object, we get a kernel density plot
plot(d)

ls(d)
d %$% plot(y = y, x = x, type = "l")

### Base R graphics work by building up graphics from layers.

## Start with a simple scatter plot
diabetes %$% plot(y = tc, x = bmi, pch = 20, xlab = "", ylab = "")

## Use the abline() function to add lines representing the means of x and y
abline(h = mean(diabetes$tc), v = mean(diabetes$bmi), lty = 2)

## Add the best fit line from a linear regression of 'tc' onto 'bmi'
fit <- lm(tc ~ bmi, data = diabetes)

abline(coef = coef(fit), col = "blue", lwd = 2)

## Add titles
title(main = "Total Cholesterol by Body Mass Index",
      ylab = "Total Cholesterol",
      xlab = "Body Mass Index")

## Add a kernel density plot on top of a histogram
hist(diabetes$age,
     probability = TRUE,
     xlab = "Age",
     main = "Distribution of Age")
lines(density(diabetes$age), col = "red", lwd = 2)

### We can adjust the global plotting parameters using the par() function

## Include multiple plots on the same canvas
par(mfrow = c(1, 3))

boxplot(age ~ sex, data = diabetes)
hist(diabetes$progress)
plot(rstudent(fit))

################################################################################
## PRACTICE PROBLEM 6.2
##
## (a) Use the par() function to adjust the plotting canvas so you can draw two
##     plots in a 1x2 array.
## (b) Using the diabetes data, create two plots. Both plots should begin with
##     a histogram of blood glucose level ('glu').
##     - In the first plot, overlay the kernel density plot for 'glu' as a blue
##       line.
##     - In the second plot, overlay the appropriate, theoretical normal density
##       as a red line.
##
## HINT: You can calculate the values for the normal density with the dnorm()
##       function. Don't forget to define the appropriate mean and SD.
##
################################################################################


###-GGPlot-------------------------------------------------------------------###

### Base R graphics are fine for quick-and-dirty visualizations (e.g., for
### checking assumptions), but for publication quality graphics, we probably
### want to use GGPlot.

### GGPlot uses the "grammar of graphics" and "tidy data" to build up a figure
### from modular components

## We start by calling the ggplot() function. We must define a dataset and some
## aesthetic via the aes() function.
(p1 <- ggplot(data = diabetes, mapping = aes(x = bmi, y = glu)))

## We need to define some geometry via an appropriate "geom" to actually plot
## the data
p1 + geom_point()
p1 + geom_line()
p1 + geom_rug()

## We can also combine different geoms into a single figure
p1 + geom_point() + geom_line() + geom_rug()

################################################################################
## PRACTICE PROBLEM 6.3
##
## Use GGPlot and the 'diabetes' data to create an empty plot of total
## cholesterol, 'tc', (on the y-axis) against 'age' (on the x-axis).
## - Don't add any geoms yet.
## - Assign the resulting plot object to a variable in your environment.
##
################################################################################

## We can use different flavors of geom for different types of data
p2 <- ggplot(diabetes, aes(tc))

p2 + geom_histogram()
p2 + geom_density()
p2 + geom_boxplot()

p3 <- ggplot(diabetes, aes(sex, bmi))

p3 + geom_boxplot()
p3 + geom_violin()

p4 <- ggplot(bfi, aes(education, age))

p4 + geom_point()
p4 + geom_jitter()

## We can also add statistical summaries of the data
p1 + geom_point() + geom_smooth()
p1 + geom_point() + geom_smooth(method = "lm")

### We can modify the styling of a plot in two distinct ways.

## Changing style options outside of the aes() function applies the styling to
## the entire plot
p5 <- ggplot(titanic, aes(age, survived))
p5 + geom_jitter(color = "blue", size = 3, height = 0.1)

## We can also apply styles as a function of variables by defining the style
## within the aes() function.
p6.1 <- ggplot(titanic, aes(age, survived, color = sex))
p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()

p6.2 <- ggplot(titanic, aes(age, survived))

p6.2 + geom_jitter(aes(color = sex), size = 3, height = 0.1) +
    geom_smooth()

p6.2 + geom_jitter(size = 3, height = 0.1) +
    geom_smooth(aes(color = sex))

p6.2 + geom_jitter(aes(color = class), size = 3, height = 0.1) +
    geom_smooth(aes(color = sex))

p6.2 + geom_jitter(aes(shape = class), size = 3, height = 0.1) +
    geom_smooth(aes(color = sex))

p6.1 + geom_jitter(aes(shape = class), size = 3, height = 0.1) +
    geom_smooth()

################################################################################
## PRACTICE PROBLEM 6.4
##
## Augment the plot you created in (6.3) to create a scatterplot.
## - Map the size of the points to 'bmi'
## - Assign the resulting plot object to a variable in your environment.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 6.5
##
## Augment the plot you created in (6.4) by adding RUG lines to both the x-axis
## and y-axis.
## - Map the color of the RUG lines to 'glu'
## - Assign the resulting plot object to a variable in your environment.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 6.6
##
## Augment the plot you created in (6.5) by adding linear regression lines.
## - Add seperate lines for males and females.
## - Differentiate the regression lines by giving them different line types.
## - Do not include the SE bands.
## - Assign the resulting plot object to a variable in your environment.
##
################################################################################

### The theme defines all of the non-data ink in a plot

## We can apply several pre-baked themes to adjust a plot's overall appearance
(p1.1 <- p1 + geom_point())
p1.1 + theme_classic()
p1.1 + theme_minimal()
p1.1 + theme_bw()

## We can also moodifying individual theme elements
p1.1 + theme_classic() +
    theme(axis.title = element_text(size = 16,
                                    family = "serif",
                                    face = "bold",
                                    color = "blue"),
          aspect.ratio = 1)

################################################################################
## PRACTICE PROBLEM 6.7
##
## Modify the plot that you created in (6.6) by adjusting the theme.
## - Change the global theme to the "classic" theme.
## - Convert all text to 14-point, serif font.
##
################################################################################

### Facetting allow us to make arrays of conditional plots

## Use facet_wrap() to condition plots on 'sex'
(p7 <- ggplot(titanic, aes(age, survived, color = class)) +
     geom_jitter(height = 0.05) +
     geom_smooth(method = "glm",
                 method.args = list(family = "binomial"),
                 se = FALSE)
)

p7 + facet_wrap(vars(sex))

## Use facet_grid() to condition plots on both 'sex' and 'class'
(p8 <- ggplot(titanic, aes(age, survived)) +
     geom_jitter(height = 0.05) +
     geom_smooth(method = "glm",
                 method.args = list(family = "binomial"),
                 se = FALSE)
)

p8 + facet_grid(vars(sex), vars(class))

################################################################################
## PRACTICE PROBLEM 6.8
##
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
##
################################################################################

### If we want to paste several different plots into a single figure (without
### facetting), we can use the utilities in the 'gridExtra' package.

grid.arrange(p1 + geom_point(),
             p3 + geom_boxplot(),
             p4 + geom_jitter(),
             p8 + facet_grid(vars(sex), vars(class)),
             ncol = 2)

################################################################################
## PRACTICE PROBLEM 6.9
##
## Use GGplot and grid.arrange() to recreate a version of the figure you created
## in (6.2).
##
################################################################################


###-Saving Graphics----------------------------------------------------------###

### To save a graphic that we've created in R, we simply redirect the graphical
### output to a file using an appropriate function.

## Save as PDF
pdf(paste0(figDir, "example_plot.pdf"))

p7 + facet_wrap(vars(sex))

dev.off()

## Save as JPEG
jpeg(paste0(figDir, "example_plot.jpg"))

p7 + facet_wrap(vars(sex))

dev.off()

## Save as PNG
png(paste0(figDir, "example_plot.png"))

p7 + facet_wrap(vars(sex))

dev.off()

## With PDF documents we can save multiple figures to a single file.
pdf(paste0(figDir, "example_plot2.pdf"))

p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()
p7 + facet_wrap(vars(sex))
p8 + facet_grid(vars(sex), vars(class))

dev.off()

################################################################################
## PRACTICE PROBLEM 6.10
##
## Save the figure that you created in (6.8) as a JPEG
## - Adjust the size to 10cm X 10cm
## - Set the resolution to 800
## - Save the image to the "../figures/" directory
##
################################################################################

### With any format, we can save multiple figures, but we need to add the '%d'
### token to the file name, so the file names will have an index.

## For PDF files, we also need to set 'onefile = FALSE'
pdf(paste0(figDir, "example_plot2_%d.pdf"), onefile = FALSE)

p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()
p7 + facet_wrap(vars(sex))
p8 + facet_grid(vars(sex), vars(class))

dev.off()

## Multiple PNG files
png(paste0(figDir, "example_plot2_%d.png"))

p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()
p7 + facet_wrap(vars(sex))
p8 + facet_grid(vars(sex), vars(class))

dev.off()

## Multiple JPEG files
jpeg(paste0(figDir, "example_plot2_%d.jpg"))

p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()
p7 + facet_wrap(vars(sex))
p8 + facet_grid(vars(sex), vars(class))

dev.off()

################################################################################
## PRACTICE PROBLEM 6.11
##
## (a) Save the five figures you created in (6.3 - 6.7) to a single PDF file.
## (b) Save the five figures you created in (6.3 - 6.7) to a separate PNG files.
##     - Save both the PDF and the PNG files to the "../figures" directory
##
################################################################################


###-END----------------------------------------------------------------------###
