### Title:    Introduction to R: Package Installation Script
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2026-01-16

install.packages(c("readr",
                   "haven",
                   "openxlsx",
                   "readxl",
                   "dplyr",
                   "magrittr",
                   "psych",
                   "ggplot2",
                   "gridExtra",
                   "tidyr"),
                 repos = "http://cloud.r-project.org",
                 dependencies = TRUE)
