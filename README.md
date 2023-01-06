Util_Global.R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

![GitHub top
language](https://img.shields.io/github/languages/top/shringi/util_Global)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/shringi/util_Global)
![GitHub](https://img.shields.io/github/license/shringi/util_Global)

![GitHub commit
activity](https://img.shields.io/github/commit-activity/y/shringi/util_Global)
![GitHub last
commit](https://img.shields.io/github/last-commit/shringi/util_Global)

![GitHub
issues](https://img.shields.io/github/issues/shringi/util_Global)

# Motivation

R and its package repository are great. I could find almost all kinds of
functions, various tricks and tips online. However, as I worked on R and
R studio long enough. I happen to create custom functions which I find
it convenient to use for my purpose. I slowly began to realize that I
should share some of them with others in case they find it helpful.

# Functions

The following functions are implemented (broad category wise)

### Calculations

- `interpolate`
- `max.adv`
- `min.adv`
- `sum.adv`
- `mov.avg`
- `mov.sd`
- `rm.na`

### Graphics

### Info

### Management

#### Objects

- `clr` : Remove variables from the environment
- `reset`

### Manipulation

### Operators

### Print

- `catn` : cat() function with new line already attached in the end.
- `hr`
- `print.warn`
- `str.list` \### Productivity \### Summary
- `get.empty.columns`: Get empty columns (NA or blank) in a data.frame
- `stat.summary`
- `summary.adv`
- `summary.non.num`
- `summary.num`
- `summaryLM`

# Installation

Just download the **util_Global.R** file.

``` r
source("util_Global.R")
install("tidyverse")
## [1] "tidyverse not loaded."
## [1] "tidyverse is already installed!"
## [1] "tidyverse has been loaded now."
```

Source it locally and you are good to go.

# Usage

### clr()

clr() is motivated from Matlab function `clr` Its an advance and short
way of writing `rm(list = ls())` \#### syntax

    clr(mode = "notall", except = NULL)

#### usage

``` r
# To delete all the variables except functions!
clr()
# To delete all the variables except functions!
temp = 1
clr( except = "temp")
```

``` r
# To clean everything except function clr(), and clc()
clr("all")
```

### drop_unfit_cols(), drop_unfit_rows()

Select out columns/rows based on unfit values \#### syntax

``` r
drop_unfit_cols(data, ..., contains = "", na.rm = TRUE)
```

#### Usage

``` r
df <- tibble(c1 = c("a", "", "c", "d", NA, "f", "g", "h"),
             c2 = c(NA, NA, 3, 4, NA, 6, 7, 8),
             c3 = c("A", "", "C", "D", NA, "#Value!", "G", "H"),
             c4 = c(NA, NA, NA, NA, NA, NA, NA, NA),
             c5 = c(0.001, NA, -99, 1, NA, NA, 1000, NA),
             c6 = c("", "", "", "", "", "", "", NA),
             c7 = c("#Value!", "", "", "#Value!", NA, "#Value!", "", "#Value!"), 
             c8 = c(-99, NA, -99, NA, NA, NA, -99, NA))
df
## # A tibble: 8 × 8
##   c1       c2 c3        c4          c5 c6    c7           c8
##   <chr> <dbl> <chr>     <lgl>    <dbl> <chr> <chr>     <dbl>
## 1 "a"      NA "A"       NA       0.001 ""    "#Value!"   -99
## 2 ""       NA ""        NA      NA     ""    ""           NA
## 3 "c"       3 "C"       NA     -99     ""    ""          -99
## 4 "d"       4 "D"       NA       1     ""    "#Value!"    NA
## 5 <NA>     NA  <NA>     NA      NA     ""     <NA>        NA
## 6 "f"       6 "#Value!" NA      NA     ""    "#Value!"    NA
## 7 "g"       7 "G"       NA    1000     ""    ""          -99
## 8 "h"       8 "H"       NA      NA     <NA>  "#Value!"    NA

# Filterning blank or NA filled columns
df %>% drop_unfit_cols()
## # A tibble: 8 × 6
##   c1       c2 c3              c5 c7           c8
##   <chr> <dbl> <chr>        <dbl> <chr>     <dbl>
## 1 "a"      NA "A"          0.001 "#Value!"   -99
## 2 ""       NA ""          NA     ""           NA
## 3 "c"       3 "C"        -99     ""          -99
## 4 "d"       4 "D"          1     "#Value!"    NA
## 5 <NA>     NA  <NA>       NA      <NA>        NA
## 6 "f"       6 "#Value!"   NA     "#Value!"    NA
## 7 "g"       7 "G"       1000     ""          -99
## 8 "h"       8 "H"         NA     "#Value!"    NA

# Filterning blank or NA filled rows
df %>% drop_unfit_rows()
## # A tibble: 6 × 8
##   c1       c2 c3      c4          c5 c6    c7           c8
##   <chr> <dbl> <chr>   <lgl>    <dbl> <chr> <chr>     <dbl>
## 1 a        NA A       NA       0.001 ""    "#Value!"   -99
## 2 c         3 C       NA     -99     ""    ""          -99
## 3 d         4 D       NA       1     ""    "#Value!"    NA
## 4 f         6 #Value! NA      NA     ""    "#Value!"    NA
## 5 g         7 G       NA    1000     ""    ""          -99
## 6 h         8 H       NA      NA     <NA>  "#Value!"    NA

# Filterning blank or NA filled columns from a column range
df %>% drop_unfit_cols(c3:c8)
## # A tibble: 8 × 6
##   c1       c2 c3              c5 c7           c8
##   <chr> <dbl> <chr>        <dbl> <chr>     <dbl>
## 1 "a"      NA "A"          0.001 "#Value!"   -99
## 2 ""       NA ""          NA     ""           NA
## 3 "c"       3 "C"        -99     ""          -99
## 4 "d"       4 "D"          1     "#Value!"    NA
## 5 <NA>     NA  <NA>       NA      <NA>        NA
## 6 "f"       6 "#Value!"   NA     "#Value!"    NA
## 7 "g"       7 "G"       1000     ""          -99
## 8 "h"       8 "H"         NA     "#Value!"    NA

# Filterning blank or NA filled rows from a column range
df %>% drop_unfit_rows(c3:c8)
## # A tibble: 6 × 8
##   c1       c2 c3      c4          c5 c6    c7           c8
##   <chr> <dbl> <chr>   <lgl>    <dbl> <chr> <chr>     <dbl>
## 1 a        NA A       NA       0.001 ""    "#Value!"   -99
## 2 c         3 C       NA     -99     ""    ""          -99
## 3 d         4 D       NA       1     ""    "#Value!"    NA
## 4 f         6 #Value! NA      NA     ""    "#Value!"    NA
## 5 g         7 G       NA    1000     ""    ""          -99
## 6 h         8 H       NA      NA     <NA>  "#Value!"    NA

# Filtering selected or NA filled columns from a column range
df %>% drop_unfit_cols(c3:c8, contains = list("", "#Value!", -99))
## # A tibble: 8 × 4
##   c1       c2 c3              c5
##   <chr> <dbl> <chr>        <dbl>
## 1 "a"      NA "A"          0.001
## 2 ""       NA ""          NA    
## 3 "c"       3 "C"        -99    
## 4 "d"       4 "D"          1    
## 5 <NA>     NA  <NA>       NA    
## 6 "f"       6 "#Value!"   NA    
## 7 "g"       7 "G"       1000    
## 8 "h"       8 "H"         NA

# Filtering selected or NA filled rows from a column range
df %>% drop_unfit_rows(c3:c8, contains = list("", "#Value!", -99))
## # A tibble: 5 × 8
##   c1       c2 c3    c4          c5 c6    c7           c8
##   <chr> <dbl> <chr> <lgl>    <dbl> <chr> <chr>     <dbl>
## 1 a        NA A     NA       0.001 ""    "#Value!"   -99
## 2 c         3 C     NA     -99     ""    ""          -99
## 3 d         4 D     NA       1     ""    "#Value!"    NA
## 4 g         7 G     NA    1000     ""    ""          -99
## 5 h         8 H     NA      NA     <NA>  "#Value!"    NA
```

### install()

loading packages every now or then you require a package to run before
you can use some R code you found online. Its typically starts with
loading a `library()` or `require()`. For example

If “package_name” is already install then you are good to go, however if
it is not installed (which is the case sometimes), then you will get an
error as following-

``` pseudocode
Error in library("package_name") : 
there is no package called ‘package_name’
```

Then one needs install it manually either by using RStudio GUI or
writing a command such as

``` r
install.packages("package_name")
```

#### syntax

``` r
install("tidyverse")
```

### catn()

cat() command is used to display a message on console or in a file.
catn() adds a new line in the end for convenience. Also this function
adds the functionality of adding the color and an argument to when or
when not to print. \#### syntax

    catn(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE, console = TRUE, color = NULL)

#### usage

``` r
# Color will only work on console!
catn("A message in blue!", color = "blue")
## [1] "crayon not loaded."
## [1] "crayon is already installed!"
## [1] "crayon has been loaded now."
## A message in blue!
```

### get.empty.columns()

Get empty columns (NA or blank) in a data.frame In multiple data set
sometimes it happens that a critical column is there in the header name
but misses any of the data. Or We might be intrested that which columns
are completely empty once we group it. This functions provides a visual
summary of such sort. (In the outputs `X` represents an empty column)

#### Usage

``` r
df
## # A tibble: 8 × 8
##   c1       c2 c3        c4          c5 c6    c7           c8
##   <chr> <dbl> <chr>     <lgl>    <dbl> <chr> <chr>     <dbl>
## 1 "a"      NA "A"       NA       0.001 ""    "#Value!"   -99
## 2 ""       NA ""        NA      NA     ""    ""           NA
## 3 "c"       3 "C"       NA     -99     ""    ""          -99
## 4 "d"       4 "D"       NA       1     ""    "#Value!"    NA
## 5 <NA>     NA  <NA>     NA      NA     ""     <NA>        NA
## 6 "f"       6 "#Value!" NA      NA     ""    "#Value!"    NA
## 7 "g"       7 "G"       NA    1000     ""    ""          -99
## 8 "h"       8 "H"       NA      NA     <NA>  "#Value!"    NA

df %>% get.empty.columns()
## [1] "knitr not loaded."
## [1] "knitr is already installed!"
## [1] "knitr has been loaded now."
## 
## 
## ===  ===  ===  ===  ===  ===  ===  ===
## c1   c2   c3   c4   c5   c6   c7   c8 
## ===  ===  ===  ===  ===  ===  ===  ===
##                X         X            
## ===  ===  ===  ===  ===  ===  ===  ===

rbind(df, df) %>% get.empty.columns(data = ., group.cols = c1)
## 
## 
## ===  ===  ===  ===  ===  ===  ===  ===
## c1   c2   c3   c4   c5   c6   c7   c8 
## ===  ===  ===  ===  ===  ===  ===  ===
##      X    X    X    X    X    X    X  
## a    X         X         X            
## c              X         X    X       
## d              X         X         X  
## f              X    X    X         X  
## g              X         X    X       
## h              X    X    X         X  
## NA   X    X    X    X    X    X    X  
## ===  ===  ===  ===  ===  ===  ===  ===
```

### str.list()

str.list() command is used to display an internal structure of a list in
a visual tree form on the console.

#### syntax

    str.list(X, prefix1 = "", prefix2 = "", prefix3 = "", prefix4 = "", a =1)

#### usage

``` r
l = list()
df <- data.frame(a = 1:10, b = 11:20)
l$data$df1 <- df
l$data$df2 <- df
l$plots$figure <- ggplot(df, aes(a, b)) + geom_point()
str.list(l)
## l
## ├─data
## │ ├─df1
## │ │ ├─a
## │ │ └─b
## │ └─df2
## │   ├─a
## │   └─b
## └─plots
##   └─figure
##     ├─data
##     │ ├─a
##     │ └─b
##     ├─layers
##     │ └─
##     ├─scales
##     ├─mapping
##     │ ├─x
##     │ └─y
##     ├─theme
##     ├─coordinates
##     ├─facet
##     ├─plot_env
##     └─labels
##       ├─x
##       └─y
```

### store.table()

Command to save csv file and .Rdata with semi-automatic name in a
predefined folder.

#### syntax

    store.table(filename, data, lt, check = T, subfolder = "03-Tables", console = FALSE, fun_family = "csv", envir = rlang::caller_env(), ...)

#### usage

``` r
l = list()
df <- data.frame(a = 1:10, b = 11:20)
l$data$df1 <- df
l$data$df2 <- df
store.table(filename = "test", data = df, lt = l)
## [1] "rlang not loaded."
## [1] "rlang is already installed!"
## [1] "rlang has been loaded now."

# Following files will be stored in the folder named as `03-Folder`
# `01_test_[R].csv` `util_Global.Rdata`
```

# Scripts

## **util_Global.R**

This is the main scripts which hosts all the custom functions. A list of
function and their usage are mentioned below.

## util_Global_Archived.R

All the functions, which I no longer use, or found a way around are
shifted here.

## Snippets.txt

A file containing plain snippets. Copy these snippets, modify and add in
your RStudio settings (Tools \> Global Options \> Code \> Editing \>
Snippets \> Edit Snippets button) for handy use. You can find more
information about RStudio Snippets from
[here](https://support.rstudio.com/hc/en-us/articles/204463668-Code-Snippets).

# Feedback

- Please feel free to raise a issue on github if you are find a bug,
  typo or suggest a feature.
- If you come across functions which supersedes any of functions
  presented here then please let me know by raising an issue.
