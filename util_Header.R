#' ---
#' title: util_Header.R
#' subtitle: |
#'
#'author:
#'  - name: Ankur Shringi
#'    email: ankurshringi@iisc.ac.in
#'    affiliation: Centre for Ecological Sciences, Indian Institute of Science, Bangalore, India
#'abstract: |
#'
#'output:
#'  html_document:
#'    self_contained: true
#'    toc: true
#'    toc_depth: 2
#'    number_sections: true
#'    toc_type: float
#'    toc_float: true
#'    highlight: kate
#'    toc_collapsed: false
#'---
#'*****
#' Project: util_Global <br><br> Enter Following command to execute it. <br>
#' `export2html('util_Header.R', folder = 'Output-R-Html', suppress_warnings = TRUE, browse = TRUE)` <br><br>
#' Date created: <br> 2019-Jan-09 20:49:01 Wednesday <br><br>
#+ echo = FALSE
# ================================================================ --------
#'*****
#' # Initialization
#+ echo = FALSE
# Loading utility function used by Ankur ----------------------------------
#' ## Loading utility function used by Ankur
source("C:/Users/Ankur/hubiC/Work/Projects/Utilities/util_Global/util_Global.R")
#+ echo = FALSE
# Deleting R-Environment Variables (Except utility functions) -------------
#' ## Deleting R-Environment Variables (Except utility functions)
clr()
#+ echo = FALSE
# Loading required packages -----------------------------------------------
#'## Loading required packages
Packages <-     c("tidyverse","roxygen2Comment")
install(Packages); rm(Packages)
#+ echo = FALSE
# Start of the real coding ------------------------------------------------
#'*****
#' # Start of the real coding
#+ echo = FALSE
# ---------------------------------------------------------------- --------

#+ echo = FALSE
# ---------------------------------------------------------------- --------
#+ echo = FALSE
# End of the coding -------------------------------------------------------
#'*****
#' # Termination
#+ echo = FALSE
# ================================================================ --------
# Session Info ------------------------------------------------------------
#' ## Session Info
# runInfo()
