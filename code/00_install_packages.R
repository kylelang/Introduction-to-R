### Title:    Introduction to R: Package Installation Script
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2025-01-18

install.packages(c("readr",
                   "haven",
                   "openxlsx",
                   "readxl",
                   "dplyr",
                   "magrittr",
                   "psych",
                   "ggplot2",
                   "gridExtra"),
                 repos = "http://cloud.r-project.org",
                 dependencies = TRUE)
