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
# Header to append to any new R file -------------------------------------------
# `header()`
header <- function() {
  file.show("C:/Users/Ankur/hubiC/Work/Projects/Utilities/util_Global/R-Header.txt")
}
# HTML Function -----------------------------------------------------------
# Various HTML Functions
# `hr(0)`
install("rstudioapi")
install("devtools")
if ("roxygen2Comment" %in% rownames(installed.packages())) {
  if ("roxygen2Comment" %!in% (.packages())) {
    suppressPackageStartupMessages(do.call("library", list("roxygen2Comment")))}
} else {
  devtools::install_github("csgillespie/roxygen2Comment")
}

hr <- function(x = 0){
  if (x == 1) {
    cat('<hr style="height:1px;border:none;color:#333;background-color:#333;" />')
  } else if (x == 0) {
    cat("<hr />")
  }
}
# `gototop()`
gototop <- function(x = 1){
  if (x == 0) {
    cat("<a href='#top'> Go back to the Table of Contents </a> ")
  } else if (x == 1) {
    cat('<table width="100%"> <td><hr style="height:1px;border:none;color:#333;background-color:#333;"  /></td> <td style="width:1px; padding: 0 10px; white-space: nowrap;"><a href="#top"> Go back to the Table of Contents </a></td> <td><hr style="height:1px;border:none;color:#333;background-color:#333;" /></td> </table>')
  }

}
# rename.col.variable.to ------
# Renaming variable while melting the data
rename.col.variable.to <- function(df,
                                   old.var.name = "variable",
                                   new.var.name = "variable") {
  names(df)[names(df) == old.var.name] <- new.var.name
  return(df)
}
# Insert Code/Text as Text in Script File Automatically -------------------
install("rstudioapi")
i <- function(insert = c("hr","chunk","gototop","section","date")) {
  insert <- match.arg(insert)
  if (insert == "hr") {
    rstudioapi::insertText(text = "hr() \n")
  } else if (insert == "chunk") {
    rstudioapi::insertText(text = "#+ results= 'asis', echo = FALSE \n")
  } else if (insert == "gototop") {
    rstudioapi::insertText(text = "gototop() \n")
  } else if (insert == "section") {
    rstudioapi::insertText(text = paste0("#+ echo = FALSE \n", "# 01 ------ \n", "###   \n \n", "#+ results= 'asis', echo = FALSE \n", "hr() \n"))
  } else if (insert == "date") {
    rstudioapi::insertText(text = format(Sys.time(), format = "%Y-%b-%d %H:%M:%S " %+% weekdays(as.Date(Sys.Date(),'%d-%m-%Y'))))
  }
}
