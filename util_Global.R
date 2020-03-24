# Author: Ankur Shringi (ankurshringi@iisc.ac.in)
# title: util_Global.R
# subtitle: ...
# abstract: ...
# Project: util_Global
# Date created: 2019-Nov-18 12:03:40 Monday
# Enter following command to render the code as html
# `r2html()`

# Installing and Loading Packages ------------------------------------
# `install("dplyr")`
install <- function(package1, ...)
{
  # convert arguments to vector
  packages <- c(package1, ...)

  # check if loaded and installed
  loaded        <- packages %in% (.packages())
  names(loaded) <- packages

  installed        <- packages %in% rownames(installed.packages())
  names(installed) <- packages

  # start loop to determine if each package is installed
  load_it <- function(p, loaded, installed)
  {
    if (loaded[p])
    {
      #print(paste(p, "is already loaded!"))
    }else{
      print(paste(p, "not loaded."))

      if (installed[p])
      {
        print(paste(p, "is already installed!"))
        suppressPackageStartupMessages(do.call("library", list(p)))
        print(paste(p, "has been loaded now."))
      }else{
        print(paste(p, "is not installed!"))
        print(paste(p, "is being installed."))
        install.packages(p, dependencies = TRUE)
        print(paste(p, "has been installed."))
        suppressPackageStartupMessages(do.call("library", list(p)))
        print(paste(p, "has been loaded now."))
      }
    }
  }
  invisible(lapply(packages, load_it, loaded, installed));
}

# Plotting two varibales on different y axis -----------------------------------
# `plotyy(x, y1, y2, ...)`
plotyy <- function(x,y1,y2, # Required Arguments
                   y1.lim = NULL, y2.lim = NULL, # Axis limits
                   y1.lab = "", y2.lab = "", # All the labels
                   y1.legend = "", y2.legend ="", # Legends
                   y1.col = "red", y2.col = "blue", # Colors of the plots
                   y1.pch = 19, y2.pch = 22, # Pch
                   las=1,
                   ...){
  par(mar = c(5,4,4,5) + .1, las = 1)

  # Plot 1
  plot(x,y1,
       ylab = y1.lab,
       ylim = y1.lim,
       pch = y1.pch, col = y1.col,
       ...)
  axis(2,col = y1.col,col.axis = y1.col)

  # Plot 2
  par(new = TRUE)
  plot(x, y2,
       ylim = y2.lim,
       col = y2.col, pch = y2.pch,
       xaxt = "n", yaxt = "n", ylab = "",...)

  axis(4)
  axis(4,col = y2.col, col.axis = y2.col)
  mtext(y2.lab, side = 4, line = 3, col = y2.col)
  legend("topright",col = c(y1.col, y2.col), lty = 1, bty = 'n',
         pch = c(y1.pch, y2.pch),
         legend = c(y1.legend, y2.legend), text.col = c(y1.col, y2.col))
}
# Reseting parametes (Required when you messed up with plot parameters)---------
# `resetPar()`
resetPar <- function() {
  dev.new();
  par(no.readonly = TRUE);
  dev.off();
}
# Plotting error bars in a basic R plot ----------------------------------------
# `plotCI(x, y = NULL, uiw, liw = uiw, ...)`
# Plotting errorbars
plotCI <- function(x, y = NULL, uiw, liw = uiw, ylim=NULL, sfrac = 0.01, add=FALSE,
                   col=par("col"), lwd=par("lwd"), slty=par("lty"),
                   xlab=deparse(substitute(x)), ylab=deparse(substitute(y)), ...)  {
  # from Bill Venables, R-list
  if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (is.null(y)) {
    if (is.null(x))
      stop("both x and y NULL")
    y <- as.numeric(x)
    x <- seq(along = x)
  }
  ui <- y + uiw
  li <- y - liw
  if (is.null(ylim)) ylim <- range(c(y, ui, li), na.rm = TRUE)
  if (add) {
    points(x, y, col = col, lwd = lwd, ...)
  } else {
    plot(x, y, ylim = ylim, col = col, lwd = lwd, xlab = xlab, ylab = ylab, ...)
  }
  smidge <- diff(par("usr")[1:2]) * sfrac
  segments(x, li, x, ui, col = col, lwd = lwd, lty = slty)
  x2 <- c(x, x)
  ul <- c(li, ui)
  segments(x2 - smidge, ul, x2 + smidge, ul, col = col, lwd = lwd)
  invisible(list(x = x, y = y))
}
# Generating reg equation for a regression -------------------------------------
# `eq.reg(reg)`
# `lm_eqn(reg)`
eq.reg <- function(reg){
  rmse <- round(sqrt(mean(resid(reg)^2)), 2)
  coefs <- summary(reg)$coefficients
  if (dim(coefs)[1] == 2) {
    b0 <- round(coefs[1],2)
    b1 <- round(coefs[2],2)
    r2 <- round(summary(reg)$r.squared, 2)
    ar2  <- round(summary(reg)$adj.r.squared, 2)
    Pvalue.slp <- round(coefs[8],3)
    Pvalue.int <- round(coefs[7],3)

    eqn <- bquote(italic(bold(Y)) == .(b0) ["("*p == .(Pvalue.int)*")"]  +
                    .(b1)*italic(bold(X))["("*p == .(Pvalue.slp)*")"] * "," ~~
                    r^2 == .(r2) * "," ~~
                    adj_r^2 == .(ar2) * "," ~~ RMSE == .(rmse))
  }else if (dim(coefs)[1] == 3) {
    c0 <- round(coefs[1,1],2)
    c1 <- round(coefs[2,1],2)
    c2 <- round(coefs[3,1],2)
    cr2 <- round(summary(reg)$r.squared, 2)
    car2  <- round(summary(reg)$adj.r.squared, 2)
    Pvalue.c0 <- round(coefs[1,4],3)
    Pvalue.c1 <- round(coefs[2,4],3)
    Pvalue.c2 <- round(coefs[3,4],3)

    eqn <- bquote(italic(bold(Y)) == .(c0) ["("*p == .(Pvalue.c0)*")"]  +
                    .(c1)*italic(bold(X))["("*p == .(Pvalue.c1)*")"] +
                    .(c2)*italic(bold(X^2))["("*p == .(Pvalue.c2)*")"]* "," ~~
                    r^2 == .(cr2) * "," ~~
                    adj_r^2 == .(car2) * "," ~~ RMSE == .(rmse))
  }
}

lm_eqn <- function(reg){
  coefs <- summary(reg)$coefficients
  if (dim(coefs)[1] == 2) {
    eq <- substitute(y == a + b %.% x*","~~italic(r)^2~"="~r2,
                     list(a = format(coef(reg)[[1]], digits = 2),
                          b = format(coef(reg)[[2]], digits = 2),
                          r2 = format(summary(reg)$r.squared, digits = 3)))
  }else if (dim(coefs)[1] == 3) {
    eq <- substitute(y == a + b %.% x+ c %.% x^2*","~~r^2~"="~r2,
                     list(a = format(coef(reg)[[1]], digits = 2),
                          b = format(coef(reg)[[2]], digits = 2),
                          c = format(coef(reg)[[3]], digits = 2),
                          r2 = format(summary(reg)$r.squared, digits = 3)))
  }
  #as.character(as.expression(eq));
  return(eq)
}
# Plotting grey confidence bars in a base R plot -------------------------------
# `conf.bar(x, reg, alpha, varname = "")`

# Plotting grey confidence bars
# Don't use subset inside lm
# Keep same variable names in varname as in fitting dataframe
conf.bar <- function(x, reg, alpha, varname = ""){
  coefs <- summary(reg)$coefficients
  newx <- seq(min(x), max(x), length.out = 12)
  newdata <- data.frame(newx)
  if (identical(varname,"") == 1)
  {
    names(newdata) <- names(reg$coefficients)[2]
  }else{
    names(newdata) <- varname
  }
  #        print(newdata)
  #       print(reg)
  preds <- predict(reg, I(newdata),interval = 'confidence')
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,alpha), border = NA)
  # model
  if (dim(coefs)[1] == 2) {
    abline(reg)
  } else if (dim(coefs)[1] > 2) {
    x.temp = seq(min(x),max(x),length.out = 100)
    new = list()
    new[[names(reg$coefficients)[2]]] = x.temp
    lines(x.temp,predict(reg.T, newdata = new))
  }
  # intervals
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
  lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
}

# Export plots to pdf or png-----------------------------------------
# `export(filename = "test.pdf)`

export <- function(fname){
  dev.copy(png,filename = paste0(fname,".png"),width = 2000, height = 1000,
           antialias = "cleartype", pointsize = 16)
  dev.off()
  dev.copy(x11)
  dev.copy2pdf(file = paste0(fname,".pdf"),width = 14, height = 9,paper = "a4r",
               out.type = "pdf")
  dev.off()
}

# Opne and closing of pdf device ------------------------------------------
# `openPdf(pdfname = "test")`
# `closePdf(pdfname = "test")`

openPdf <- function(pdfname = "test", width = 14, height = 9, subfolder = "Output-Graphics", path = getwd()){
  closeFigs()
  # Opening "test.pdf"
  if (!file.exists(subfolder)) {
    dir.create(file.path(path, subfolder))
  }
  pdfname <- subfolder %/% pdfname
  {pdf(paste0(pdfname, " [R].pdf"), width = width, height = height, paper = "a4r",
       onefile = T, bg = "white");
    dev.control("enable")}
}
# closePdf(pdfname = "test")
closePdf <- function(pdfname = "test", subfolder = "Output-Graphics"){
  # Closing Images
  dev.off()
  # opening pdf file
  system(paste0("open ", shQuote(subfolder %/% pdfname %+% " [R].pdf")))
}
# Clear data or screen -------------------------------------------
# `clr()`
# `clr("all")`
clr <- function(mode="notall"){
  ENV <- globalenv()
  ll <- ls(envir = ENV)
  if ((mode == "all") || (mode == "All")) {
    ll <- ll[!ll %in% c("clr","clc")]
    rm(list = ll, envir = ENV)
  }else if (mode == "notall") {
    rm(list = setdiff(ls(envir = ENV), lsf.str(envir = ENV)), envir = ENV)}
}
# clc()
clc <- function(){cat("\014")}

# Close all open figures ------
# `closeFigs()`
closeFigs <- function(){
  graphics.off()
}

# Convert any object to a string -----------------------------------------------
# `to.chr(string = objName)`
to.chr <- function(string){
  as.character(substitute(string))
}

# Advance function for converting columns to numeric ---------------------------
# `as.numeric.adv(x)`
as.numeric.adv <- function(x) {
  if (class(x) == "factor") {
    as.numeric(levels(x))[x]
  }else{
    as.numeric(x)
  }
}

# Converting a dataframe column to numeric ------
col2num <- function(df,column, use.names = FALSE){
  unlist(df[column], use.names = use.names)
}

# Converting classes of columns of dataframe -----------------------------------
# `convertClass(obj = df, types = to.chr(fnin - fnic))`
convertClass <- function(obj, types, date.origin = "1970-01-01"){
  # Input data is pure dataframe
  # Check you give the classes in lowerclass
  # It works for all columns
  if (class(obj)[1] != "data.frame") {
    # in case obj is tbl_df or tbl
    obj <- as.data.frame(obj)
  }
  if (is.character(types)) {
    types <- types %>%
      gsub(.,pattern = ",", replacement = "") %>%
      gsub(.,pattern = " ", replacement = "") %>%
      gsub(.,pattern = "-", replacement = "") %>%
      strsplit(.,"") %>% unlist(.)
  }
  origin <- date.origin
  as.date <- function(x){
    if (class(x) == "factor") {
      as.Date(x, origin = origin)
    }else{
      as.Date(as.numeric.adv(x), origin = origin)
    }}
  out <- lapply(1:length(obj),
                FUN = function(i){FUN1 <- switch(tolower(types[i]),
                                                 character = as.character,
                                                 c = as.character,
                                                 numeric = as.numeric.adv,
                                                 n = as.numeric.adv,
                                                 integer = as.integer,
                                                 i = as.integer,
                                                 factor = as.factor,
                                                 f = as.factor,
                                                 date = as.date,
                                                 d = as.date,
                                                 logical = as.logical,
                                                 l = as.logical); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  return(as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE))
}

# Generating unique file names -------------------------------------------------
# `uniqueFilename(filename = "test.pdf", default = FALSE)`
#tempfile
uniqueFilename <- function(filename, default = FALSE){
  sysTime <- format(Sys.time(), format = "%y-%b-%d-%H%M%S")
  if (default == TRUE) {
    return(sysTime %+% '-' %+% filename)
  }else{
    return(filename %+% '-' %+% sysTime)
  }
}

# Sandeep: Borrowed Functions
# From Sandeep
# Extracting different summary of linear model ---------------------------------
# `summaryLM(reg)`

summaryLM <- function(reg){
  out = {}
  out$Slope <- coef(reg)[2]
  out$Intercept <- coef(reg)[1]
  out$Slope25 <- confint(reg)[2,"2.5 %"]
  out$Slope95 <- confint(reg)[2,"97.5 %"]
  out$Intercept25 <- confint(reg)[1,"2.5 %"]
  out$Intercept95 <- confint(reg)[1,"97.5 %"]
  out$SlopePval <-  summary(reg)$coefficients[2,"Pr(>|t|)"]
  out$InterceptPval <- summary(reg)$coefficients[1,"Pr(>|t|)"]
  out$RSquared <- summary(reg)$r.squared
  out$AdjRSquared <- summary(reg)$adj.r.squared
  return(out)
}

# Count occurance of a character in a string -----------------------------
# `char.count (string = "string", Char = "i")`
char.count <- function(string, Char ="."){
  return(length(unlist(strsplit(string, Char, fixed = TRUE))) - 1)
}

# Add Path to filename ----------------------------------------------------
# `getwd()`
"%/%" = function(Path.Folder.Data,File.Name){
  # Cheking whether / was already added to path
  if (substr(Path.Folder.Data,nchar(Path.Folder.Data),nchar(Path.Folder.Data)) == "/") {
    File.Name <- paste0(Path.Folder.Data, File.Name)
  }else{
    File.Name <- paste0(Path.Folder.Data, "/", File.Name)
  }
  return(File.Name)
}

# Not in the list ----------------------------------------------------
# `getwd()`
"%!in%" = function(x,y)!('%in%'(x,y))

# Save data as csv file (no need to worry about path and extension) ------------
# `save.csv(data, file.name, path = getwd(), subfolder = "Output-R-Tables" )`
save.csv <- function(data,file.name,path=getwd(), row.Names = FALSE,
                     subfolder="Output-Tables"){
  # In case my path has forward slash
  if (substr(path,nchar(path),nchar(path)) != "/") {
    path <- paste0(path,"/")
  }
  # if subfolder doesn't exist then create it.
  if (!file.exists(subfolder)) {
    dir.create(file.path(path, subfolder))
  }
  path <- path %/% subfolder
  char = "."
  # In case my file name already have .csv in it
  # we don't wan't to add another .csv
  if (char.count(file.name) == 0) {
    file.name <- path %/% file.name %+% ".csv"
  }else if (char.count(file.name) == 1) {
    file.name <- path %/% file.name
  }else if (char.count(file.name) == 2) { # Checking whether we have >1 "."
    last.ext <- rev(unlist(strsplit(file.name,char,fixed = TRUE)))[1]
    second.last.ext <- rev(unlist(strsplit(file.name,char,fixed = TRUE)))[2]
    if (last.ext == second.last.ext) {
      file.name <-  paste(unlist(strsplit(file.name,char,fixed = TRUE))[1:2],
                          collapse = ".")
    }else{
      warning(paste0("File name <", file.name,
                     "> has muliple '.' other than extention"))
    }
  }else{
    if (unique(rev(unlist(strsplit(file.name,char,fixed = TRUE)))[1:3]) == "csv") {
      stop(paste0("File name <", file.name, "> has muliple '.' other than extention"))
    }
  }
  write.csv(data,file.name,row.names = row.Names)
}

# Saving session info to a txt file --------------------------------------------
# `runInfo()`
runInfo <- function(time=FALSE){
  closeAllConnections()
  fileName <- file(basename(getwd()) %+% "-Session Info.txt")
  sink(fileName)

  if (time == TRUE) {
    msg <- "Ran by " %+% getUser() %+% " on " %+%
      as.character(Sys.time()) %+% "\n"
  } else {
    msg <- "Ran by " %+% getUser() %+% " on " %+%
      as.character(Sys.Date()) %+% "\n"
  }
  catn(msg)
  catn(capture.output(sessionInfo(),split = TRUE))
  sink()
  closeAllConnections()
}

# Get User Name -----------------------------------------------------------
# `getUser()`
getUser <- function(){
  env <- if (.Platform$OS.type == "windows") "USERNAME" else "USER"
  unname(Sys.getenv(env))
}

# Header to append to any new R file -------------------------------------------
# `header()`
header <- function() {
  file.show("C:/Users/Ankur/hubiC/Work/Projects/Utilities/util_Global/R-Header.txt")
}

# Publication Theme for ggplot2 -------------------------------------------
# `theme_publication(base_size=14, base_family="")`
# `scale_fill_Publication()`
# `scale_colour_Publication`
install(c("ggthemes","grid"))
theme_Publication <- function(base_size=14, base_family="") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size = base_size, base_family = base_family) +
      theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle = 90, vjust = 2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line.x = element_line(colour = "black",size = 1),
            axis.line.y = element_line(colour = "black",size = 1),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour = "#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face = "bold"),
            plot.margin = unit(c(10,5,5,5),"mm"),
            strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
            strip.text = element_text(face = "bold")
      ))
}
# scale_fill_Publication()
scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}
# scale_colour_Publication()
scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

# Reset Everything (Like a new R session) --------------------------------------
# `reset()`
reset <- function(){
  clr()
  clc()
  closeFigs()
  invisible(resetPar())
}

# Concenate object as Text -------------------------------------------
# `"a" %+% "b"`
"%+%" = function(obj_1, obj_2) {
  paste(obj_1, obj_2, sep = "")
}
# cat() that appends a newline ---------------------------------------
# `catn(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)`
catn = function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
                append = FALSE) {
  cat(..., "\n", file = file, sep = sep, fill = fill, labels = labels,
      append = append)
}

# List objects available in the environment-------------------------------------
# `list.objects(env = .GlobalEnv)`
list.objects <- function(env = .GlobalEnv){
  if (!is.environment(env)) {
    env <- deparse(substitute(env))
    stop(sprintf('"%s" must be an environment', env))
  }
  obj.type <- function(x) class(get(x, envir = env))
  foo <- sapply(ls(envir = env), obj.type)
  object.name <- names(foo)
  names(foo) <- seq(length(foo))
  dd <- data.frame(CLASS = foo, OBJECT = object.name,
                   stringsAsFactors = FALSE)
  dd[order(dd$CLASS),]
}

# Check whether all the elements are same or identical -------------------------
# `all.same(vector)`
# `all.identical(x, warn = FALSE)`
all.same <- function(vector){
  # Give a vector of values and it will tell whether all the values are same or not
  # Returns boolean results in terms of True or False
  if (length(unique(vector)) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# all.identical(x, warn = FALSE)
all.identical <- function(x, warn = FALSE) {
  if (length(x) == 1L) {
    if (warn) {
      warning("'x' has a length of only 1")
    }
    return(TRUE)
  } else if (length(x) == 0L) {
    warning("'x' has a length of 0")
    return(logical(0))
  } else {
    TF <- vapply(1:(length(x) - 1),
                 function(n) identical(x[[n]], x[[n + 1]]),
                 logical(1))
    if (all(TF)) TRUE else FALSE
  }
}

# Stich (Collapse a vector of strings) ------------------------------------
# `stich(vec, collapse = " ")`
stich <- function(vec, collapse = " "){
  paste0(as.character(vec), collapse = collapse)
}

# Remove na from a vector -------------------------------------------------
# `rm.na(vec)`
rm.na <- function(vec){
  return(vec[!is.na(vec)])
}

# Merging two dataframe like mean +- sd -----------------------------------
# `fuse(df.prfx, df.sufx, merged.Cols , link = "±")`
fuse <- function(df.prfx, df.sufx, merged.Cols , link = "±"){
  # Checking no. of cols are identical
  if (dim(df.prfx)[2] != dim(df.sufx)[2]) {
    stop("Number of columns are not matching, please recheck")
  }
  if (dim(df.prfx)[1] != dim(df.sufx)[1]) {
    print.warn("Please note that number of rows are not matching!")
  }
  if (!identical(sort(names(df.prfx)),sort(names(df.sufx)))) {
    stop("Columns names are not matching, please fix")
  }
  # Merging matrix
  merged <- merge(df.prfx, df.sufx, by = merged.Cols, all = TRUE,
                  suffixes = c(".prfx",".sufx"))
  out <-  as.data.frame(matrix(NA, nrow = dim(merged)[1], ncol = dim(df.prfx)[2] ))
  names(out) <- names(df.prfx)
  out[, merged.Cols] <- merged[ , merged.Cols]
  to.fuse.cols <- setdiff(names(df.prfx),merged.Cols)

  for (cols in to.fuse.cols) {
    prfx.rows.blank <- merged[,cols %+% ".prfx"] %in% c(""," ")
    prfx.rows.Na <-  is.na(merged[,cols %+% ".prfx"])
    prfx.rows.valid <- (!prfx.rows.blank & !prfx.rows.Na)

    sufx.rows.blank <- merged[,cols %+% ".sufx"] %in% c(""," ")
    sufx.rows.Na <-  is.na(merged[,cols %+% ".sufx"])
    sufx.rows.valid <- (!sufx.rows.blank & !sufx.rows.Na)

    # Completely valid rows
    rows.valid <-  prfx.rows.valid & sufx.rows.valid
    out[rows.valid, cols] <- paste(merged[rows.valid ,cols %+% ".prfx"],
                                   merged[rows.valid ,cols %+% ".sufx"],
                                   sep = " " %+% link %+% " ")

    # Only prefix valid rows
    only.prfx.rows.valid <-  prfx.rows.valid & !sufx.rows.valid
    out[only.prfx.rows.valid, cols] <- merged[only.prfx.rows.valid, cols %+% ".prfx"]

    # Only sufix valid rows
    only.sufx.rows.valid <-  sufx.rows.valid & !prfx.rows.valid
    if (sum(only.sufx.rows.valid) > 0) {
      out[only.sufx.rows.valid, cols] <- merged[only.sufx.rows.valid, cols %+% ".sufx"]
      print.warn("Watch out suffix df have valid rows while prefix df not!!")
    }

    rows.rest <-  !(prfx.rows.valid | sufx.rows.valid)
    out[rows.rest, cols] <- ""
  }
  return(out)
}

# Moving Average & Standard Deviations ------------------------------------
# `mov.avg(x, width, align = "center", partial = FALSE, na.rm = FALSE )`
mov.avg <- function(x, width, align = "center", partial = FALSE, na.rm = FALSE ) {
  # Installing zoo
  install("zoo")
  out <- rollapply(data = x, width = width, FUN = mean, na.rm = na.rm, align = align, fill = NA, partial = partial)
  return(out)
}

# `mov.sd(x, width, align = "center", partial = FALSE, na.rm = FALSE )`
mov.sd <- function(x, width, align = "center", partial = FALSE) {
  install("zoo")
  out <- rollapply(data = x, width = width, FUN = sd, na.rm = na.rm, align = align, fill = NA, partial = partial)
  return(out)
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
# export to html
# 'export2html(".R")'
export2html <- function(filename, folder = 'Output-html', suppress_warnings = TRUE, browse = TRUE, output_file = NULL) {
  if (suppress_warnings) {
    suppressWarnings(rmarkdown::render(filename, output_dir = folder, clean = TRUE, quiet = TRUE,
                                       output_file = output_file))
  } else {
    rmarkdown::render(filename, output_dir = folder, clean = TRUE, quiet = TRUE, output_file = output_file)
  }
  if (is.null(output_file)) {
    output_file = folder %/% substr(filename, start = 1, nchar(filename) - 2)
  }
  if (browse) {
    file_wt_ext <- folder %/% output_file
    if (file.exists(file_wt_ext %+% ".html")) {
      browseURL(file_wt_ext %+% ".html", getOption("browser"))
    } else if (file.exists(file_wt_ext %+% ".nb.html")) {
      browseURL(file_wt_ext %+% ".nb.html", getOption("browser"))
    } else {
      stop("Corresponding html file doesnt exists!!")
    }
  }
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
# Open Current Directory directly from R ---------------------------------------
# 'openwd()'
openwd <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows") {
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))}
}

# List objects by their size ----------------------------------------------
# `lsos()`
.ls.objects <- function(pos = 1, pattern, order.by,
                        decreasing = FALSE, head = FALSE, n = 5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

lsos <- function(..., n=10) {
  .ls.objects(..., order.by = "Size", decreasing = TRUE, head = TRUE, n = n)
}

# function to see logs/ diary of work and project progress ------
diary <- function(){
  #Installing necessary plugins
  install(c("dplyr","DT","readxl","htmlwidgets"))
  #Name and path for the file
  dir <- "c:/Users/Ankur/hubiC/Work/Projects/"
  name <- "Diary&Tasks[Ankur]"
  table <- read_excel(dir %/% name %+% ".xlsm",sheet = "Work_Diary")
  #` Converting classes
  table$`#` <- as.integer(table$`#`)

  table$`Project` <- as.factor(table$`Project`)
  table$Section <- as.factor(table$Section)
  table$`Context File` <- as.factor(table$`Context Files`)
  table$`Task Type` <- as.factor(table$`Task_Type`)

  #  Creating html table
  widget <- datatable(table, class = 'display', filter = list(position = 'top', clear = TRUE, plain = FALSE),
                      extensions = 'FixedHeader', escape = TRUE,
                      options = list(initComplete = JS("function(settings, json) {",
                                                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                       "}"),
                                     paging = TRUE, searchHighlight = TRUE,search = list(smart = TRUE),pageLength = 400,
                                     autoWidth = TRUE, fixedHeader = TRUE,
                                     columnDefs = list(list(width = '100%', targets = "_all",
                                                            className = 'compact')))) %>%
    formatStyle('Project',target = "row", backgroundColor = styleEqual(c("Growth"), c('yellow'))
    )
  #  Saving html table in the same folder as html file
  DT::saveWidget(widget, dir %/% name %+% '.html',selfcontained = TRUE)

  #  opening the file
  browseURL(dir %/% name %+% '.html')
  #https://datatables.net/manual/styling/classes
}

# blank as NA ------
#Usage: data %>% mutate_each(funs(blank2NA))
blank2NA <- function(x){
  install("dplyr")
  if ("factor" %in% class(x)) x <- as.character(x) # since ifelse wont work with factors
  ifelse(as.character(x) != "", x, NA)
}

# as.attrib.same ----------------------------------------------------------
# 'Usage: 'is.attrib.same(df = ha_11, by_col = "tag", attrib_cols = c("plot_id","year","qdt","s.qdt","gx","gy","spp","date4"))'

# print.warn ------
print.warn = function(msg) {
  # Taken from sandeep's util.r file
  # Signals a test failure.
  # msg - Message to be printed.
  catn(msg)
  warning(msg, call. = F)
}

# fix.levels------
fix.levels = function(x, level_map) {
  # Taken from sandeep's util.r file
  # "Re-levels" factors in 'x' to correspond to the levels in 'level_map'.
  # x - Vector of factors, possibly with fewer levels than 'level_map.'
  # level_map - Vector of named unique levels.
  # return - Releveled 'x.'
  return(factor(level_map[levels(x)[x]], level_map, names(level_map)))
}

# rename.col.variable.to ------
# Renaming variable while melting the data
rename.col.variable.to <- function(df,
                                   old.var.name = "variable",
                                   new.var.name = "variable") {
  names(df)[names(df) == old.var.name] <- new.var.name
  return(df)
}

# sum.adv ------
# Modified sum function to handle values like all NA
sum.adv <- function(x){
  if (sum(!is.na(x)) == 0) {
    out <- NA
  } else {
    out <- sum(x, na.rm = TRUE)
  }
  return(out)
}

# max.adv ------
# Modified max function to handle values like all NA
max.adv <- function(x){
  if (sum(!is.na(x)) == 0) {
    out <- NA
  } else {
    out <- max(x, na.rm = TRUE)
  }
  return(out)
}

# max.adv ------
# Modified min function to handle values like all NA
min.adv <- function(x){
  if (sum(!is.na(x)) == 0) {
    out <- NA
  } else {
    out <- min(x, na.rm = TRUE)
  }
  return(out)
}

# stat.summary------
# stat.summary
# given a data frame, a table of list of variables and its filter, and list of grouping variables it provides a mean, standard deviation and other relavant summaries.
stat.summary <- function(colTable, group_var, df_in){
  df_out <- list()
  for (f in (1:dim(colTable)[1])) {
    var <-  colTable[f, 1]
    filter.var <- colTable[f, 2]

    if (is.na(filter.var) ==  TRUE) {
      filter.var = TRUE
    }

    var.mean <- var %+% ".mean"
    var.sd <- var %+% ".sd"
    var.cv <- var %+% ".cv"
    var.max <- var %+% ".max"
    var.min <- var %+% ".min"
    var.n <- var %+% ".n"
    var.se <- var %+% ".se"

    rm(var)
    var <- colTable[f, 1] %+% "[" %+% filter.var %+% " == TRUE]"

    formula.mean <- "mean(" %+% var %+% ", na.rm = TRUE)"
    formula.sd <- "sd(" %+% var %+% ", na.rm = TRUE)"
    formula.cv <- var.sd %+% "*100/" %/% var.mean
    formula.max <- "max.adv(" %+% var %+% ")"
    formula.min <- "min.adv(" %+% var %+% ")"
    formula.n <- "sum.adv(!is.na(" %+% var %+% "))"
    formula.se <- var.sd %+% "/sqrt(" %+% var.n %+% ")"

    df_out[[f]] <- df_in %>% group_by_at(vars(group_var)) %>%
      summarise(!!var.mean := eval(parse(text = formula.mean)),
                !!var.sd := eval(parse(text = formula.sd)),
                !!var.cv := eval(parse(text = formula.cv)),
                !!var.min := eval(parse(text = formula.min)),
                !!var.max := eval(parse(text = formula.max)),
                !!var.n := eval(parse(text = formula.n)),
                !!var.se := eval(parse(text = formula.se))
      )
  }
  return(df_out %>% reduce(left_join, by = group_var) %>% as.data.frame())
}

# pad.00------
# Pads a number or string with zeros
pad.00 <- function(string, width = 2, side = "left"){
  install("stringr")
  str_pad(string = string, width = width, side = side, pad = "0")
}

# is.attrib.same ---------------
# Crosscheck whether two data sets are same in some columns
# Checks the internal consistency of the data by a particular column
is.attrib.same <- function(df, by_col, attrib_cols){
  # Check that by_col and attrib_cols are mutually exclusive
  if (length(intersect(by_col,attrib_cols)) != 0) {
    stop(sprintf("by_col and attrib_cols have operlapping column: %s",intersect(by_col,attrib_cols)))
  } else {
    # Filtering rows where there are conflicting values for a tag
    # Removing columns which are consistent!
    # First term generates the string of TRUE by numbers of by_col
    # Second term checks whether all the entries in the attrib_cols columns are TRUE or not. If it is not then it filters it.
    df_check <- df %>% select(by_col, attrib_cols) %>% group_by_(by_col) %>% summarise_all(all.same) %>%
      filter_at(vars(attrib_cols), any_vars(. == FALSE )) %>%
      select_if(c(rep(TRUE, length(by_col)), sapply(.[attrib_cols], all) == FALSE))
    return(df_check)
  }
}

# r2html -------------------------------------------------------------------------------------------------
# Funtion which converts normal R code into a rmarkdown code. There already exists some implementation, for example see <https://rmarkdown.rstudio.com/articles_report_from_r_script.html> for more details.
# This function however does an additional things.
# 1. Converts R-section to R markdown sections
# 2. Converts normal comments to roxygen comments automatically
# 3. Appends detailed output format
r2html <- function(){
  invisible(install("dplyr"))
  file <- rstudioapi::getSourceEditorContext()$path
  flIn  <- readLines(file)
  # open the file and read in all the lines
  head <- unlist(strsplit(flIn[2:4], split = '\n'))
  render <- unlist(strsplit(flIn[5:8], split = '\n'))
  time_now <- "# Last Rendered: " %+%
    format(Sys.time(), format = "%Y-%b-%d %H:%M:%S " %+% weekdays(as.Date(Sys.Date(),'%d-%m-%Y')))
  flIn <- flIn[-c(1:8)]
  block <- "#'author:
#'  - name: Ankur Shringi
#'    email: ankurshringi@iisc.ac.in
#'    affiliation: Centre for Ecological Sciences, Indian Institute of Science, Bangalore, India
#'output:
#'  html_notebook:
#'    self_contained: true
#'    toc: true
#'    toc_depth: 2
#'    number_sections: true
#'    toc_float: true
#'    highlight: kate
#'---
#'*****"
  text_block <- unlist(strsplit(block, split = '\n'))
  # concatenate the old file with the new text
  flIn <- c("#'---", head, text_block, render[1:2], time_now, render[3:4], "#'", "#'*****", flIn)
  secStrt <- which(grepl(flIn, pattern = "# ", perl = TRUE))
  secEnd <- which(grepl(flIn, pattern = "----", perl = TRUE))
  comLines <- which(grepl(flIn, pattern = "^+# "))
  secLines <- intersect(secStrt, secEnd)
  sketchLines <- which(grepl(flIn, pattern = "sketch\\(", perl = TRUE))
  for (i in 1:length(flIn)) {
    if (i %in% secLines) {
      flIn[i] <-  flIn[i] %>%
        gsub(pattern = "[-]+$", replacement = "", x = .) %>%
        gsub(pattern = "^+# ", replacement =  "#' ## ", x = .)
    } else if (i %in% comLines) {
      flIn[i] <-  flIn[i] %>%
        gsub(pattern = "^+# ", replacement = "#' ", x = .) %+% "<br>"
    }
    if (i %in% sketchLines) {
      flIn[i] <-  flIn[i] %>%
        gsub(pattern = ')$', replacement = ', export = FALSE)', x = .)
    }
  }
  filename = basename(file)
  fn = substr(filename, start = 1, nchar(filename) - 2)
  writeLines(flIn, con = "temp_rmd.R")
  export2html("temp_rmd.R", folder = 'Output-html', suppress_warnings = TRUE, browse = TRUE, output_file = fn)
  if (file.exists("temp_rmd.R"))
    #Delete file if it exists
    file.remove("temp_rmd.R")
}

# opendir -----------------------------------------------------------------------------------------------
# Function which opens the current working directory directly from the console
# Adapted from the stackoverflow user Dason <https://stackoverflow.com/users/1003565/dason>
# Stack overflow link <https://stackoverflow.com/questions/12135732/how-to-open-working-directory-directly-from-r-console>
opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows") {
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

# summary.adv -------------------------------------------------------------------------------------------
# Function to convert summary into a nice dataframe
summary.adv = function(data){
  out <- data.frame(unclass(summary(data)), check.names = FALSE, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  return(out)
}

# summary.num -------------------------------------------------------------------------------------------
# Detailed summary of the numeric columns only
summary.num = function(data){
  install("summarytools")
  out <- as.data.frame(summarytools::descr(data, transpose = TRUE, round.digits = 4))
  rows <- rownames(out)
  out <- out %>% mutate(SE = Std.Dev/sqrt(N.Valid)) %>% as.data.frame()
  out$names <- rows
  out <- out[,c(17,15,14,3,1,7,17,2,8,9,16,10,17,4,5,6,11,12,13)]
  return(out)
}

# summary.non.num -------------------------------------------------------------------------------------------
# Function to convert summary into a nice dataframe
summary.non.num = function(data){
  a <- sapply(data,class)
  cols <- names(a[!(a %in% c("numeric", "integer")) ])
  out <- data.frame(unclass(summary(data %>% dplyr::select(cols))), check.names = FALSE, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  return(out)
}

# sketch.pptx -------------------------------------------------------------------------------------------
# Function to export the plot as editable powerpoint file
sketch.pptx <- function(figObj, prefix, figname, figObjName, figList = ""){
  install(c("officer"))
  subfolder = "Output-Graphics"
  path = subfolder %/% prefix %+% "-" %+% figname %+% " [R]" %+% ".pptx"
  if (!file.exists(path)) {
    out <- read_pptx()
  } else {
    out <- read_pptx(path)
  }
  footer_text <- prefix %+% "-<...>.R: Figure: " %+% figList %+% "$" %+% figObjName
  out %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = figObj, location = ph_location_type(type = "body"),
            bg = "transparent" ) %>%
    ph_with(value = ".", location = ph_location_type(type = "title")) %>%
    ph_with(value = paste0(format(Sys.time(), format = "%Y-%b-%d %H:%M:%S "), weekdays(as.Date(Sys.Date(), '%d-%m-%Y'))), location = ph_location_type(type = "dt")) %>%
    ph_with(value = footer_text, location = ph_location_type(type = "ftr")) %>%
    ph_hyperlink(ph_label = slide_summary(.) %>% dplyr::filter(type == "ftr") %>% dplyr::select(ph_label), type = "ftr", href = figname %+% " [R]" %+% ".pdf") %>%
    print(target = path)
}

# print.figure ----------------------------------------------------------
# Printing a singleton figure by an object
print.figure <- function(figobj, prefix, figname, figObjName, figList = "", pdf = TRUE, pptx = FALSE){
  if (pdf) {
    print(figobj)
  }
  if (pptx) {
    sketch.pptx(figObj = figobj, prefix = prefix, figname = figname, figObjName = figObjName, figList = figList)
  }
}
# sketch ------------------------------------------------------------------------------------------------
# Function to save a singleton plot as pdf as well as ppt
sketch <- function(figObj, prefix="99ZZ-99z-99", figname = "temp", ppt = FALSE, export = TRUE){
  subfolder = "Output-Graphics"
  path = getwd()
  if (!file.exists(subfolder)) {
    dir.create(file.path(path, subfolder))
  }
  figname <- prefix %+% "-" %+% figname
  print(figObj)
  if (export == TRUE) {
    ggsave(plot = figObj, path = subfolder, figname %+% "[R].pdf", height = 210, width = 297, units = "mm")
    system(paste0("open ", shQuote(subfolder %/% figname), "[R].pdf"))
    if (ppt == TRUE) {
      sketch.pptx(figObj = figObj, figname = figname)
    }
  }
}


# export.list.of.figures --------------------------------------------------
# Saves a list of figures into a joint pdf or pptx file.
export.list.of.figures <- function(figList, prefix = "99zz-99z-99", figname, pdf = TRUE, pptx = FALSE, ...){
  prefix.figname = prefix %+% "-" %+% figname
  if(pdf){
    openPdf(pdfname = prefix.figname, ...)
  }
  figListName = substitute(figList)
  walk2(figList, names(figList), ~print.figure(figobj = .x, prefix = prefix, figname = figname, figObjName = .y, figList = figListName, pptx = pptx))
  if(pdf){
    closePdf(pdfname = prefix.figname, ...)
  }
}
# interpolate -------------------------------------------------------------------------------------------
# Function to interpolate the values by fitting a smooth spline
interpolate <- function(x, y, df, y_per, graph = FALSE){
  temp <- data.frame(x = x, y = y) %>%
    filter(complete.cases(.)) %>%
    group_by(x) %>%
    summarise(y = mean(y, na.rm = TRUE)) %>%
    arrange(x)
  if (dim(temp)[1] < 4) {
    return(list(NA,NA))
  } else {
    x = temp$x
    y = temp$y
    df = floor(length(unique(x[!is.na(x)]))*.9)
    rm(temp)
    reg <- smooth.spline(x = x, y = y, df = df)
    xval <- approx(x = reg$y, y = reg$x, xout = y_per)$y
    if (graph) {
      plot(x,y)
      lines(reg)
      abline(h = y_per)
      abline(v = xval)
    }
    return(list(reg, xval))
  }
}
