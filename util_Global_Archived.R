# Author: Ankur Shringi (ankurshringi@iisc.ac.in)
# title: util_Global_Archived.R
# subtitle: Archieved functions, which no longer being used.
# abstract: ...
# Project: util_Global
# Date created: 2020-May-14 15:04:58 Thursday
# Enter following command to render the code as html
# `r2html()`
# Initialization ----------------------------------------------------------
# Loading custom made utility functions
source("util_Global.R")
# Deleting R-Environment Variables (Except utility functions)
clr()
# Loading required packages
Packages <-     c("tidyverse")
install(Packages); rm(Packages)
