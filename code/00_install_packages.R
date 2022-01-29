### Title:    Introduction to R: Package Installation Script
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2022-01-29
   
install.packages(c("haven",
                   "foreign",
                   "openxlsx",
                   "readxl",
                   "dplyr",
                   "magrittr",
                   "psych",
                   "rockchalk",
                   "multcomp",
                   "ggplot2",
                   "gridExtra"),
                 repos = "http://cloud.r-project.org",
                 dependencies = TRUE)
