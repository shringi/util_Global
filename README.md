# Motivation
R and its package repository are great. I could find almost all kinds of functions, various tricks and tips online. However, as I worked on R  and R studio long enough. I happen to create custom functions which I find it convenient to use for my purpose. I slowly began to realize that I should share some of them with others in case they find it helpful.

For example, every now or then you require a package to run before you can use some R code you found online. Its typically starts with loading a `library()` or `require()`. For example

```R
library("package_name")
```

If "package_name" is already install then you are good to go, however if it is not installed (which is the case sometimes), then you will get an error as following-

```pseudocode
Error in library("package_name") : 
  there is no package called ‘package_name’
```

 Then one needs install it manually either by using RStudio GUI or writing a command such as 

```R
install.packages("package_name")
```

I thought, wouldn't it be better that I write one command `install("package_name")` and it detects automatically whether the package is available or not. If it not available then goes ahead and downloads it automatically behind the scenes using `install.packages()`, else loads it using `library()` command. So created one such command `install()` for my personal use and many others such tiny tweaked functions and though of sharing on the github.

# Scripts

## **util_Global.R**

This is the main scripts which hosts all the custom functions. A list of function and their usage are mentioned below.

## util_Global_Archived.R

All the functions, which I no longer use, or found a way around are shifted here.

## Snippets.txt

A file containing plain snippets. Copy these snippets, modify and add in your RStudio settings (Tools > Global Options > Code > Editing > Snippets > Edit Snippets button) for handy use. You can find more information about RStudio Snippets from [here](https://support.rstudio.com/hc/en-us/articles/204463668-Code-Snippets).

# Getting started

Just download the **util_Global.R** file. 

Source it locally and you are good to go.

# Feedback

- Please feel free to raise a issue on github if you are find a bug or suggest a feature.
- If you come across any  functions which surpasses any of the functions or then please let me know in the issues as well.