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
install <- function(package1, load = TRUE, ...) {
  # convert arguments to vector
  packages <- c(package1, ...)

  # check if loaded and installed
  loaded <- packages %in% (.packages())
  names(loaded) <- packages

  installed <- packages %in% rownames(installed.packages())
  names(installed) <- packages

  # start loop to determine if each package is installed
  load_it <- function(p, loaded, installed) {
    if (loaded[p]) {
      #print(paste(p, "is already loaded!"))
    } else {
      print(paste(p, "not loaded."))
      if (installed[p]) {
        print(paste(p, "is already installed!"))
      } else {
        print(paste(p, "is not installed!"))
        print(paste(p, "is being installed."))
        install.packages(p, dependencies = TRUE)
        print(paste(p, "has been installed."))
      }
      if (load) {
        suppressPackageStartupMessages(do.call("library", list(p)))
        print(paste(p, "has been loaded now."))
      } else {
        print(paste(p, "is not loaded!"))
      }
    }
  }
  invisible(lapply(packages, load_it, loaded, installed))
}

# Plotting two variables on different y axis -----------------------------------
# `plotyy(x, y1, y2, ...)`
plotyy <- function(
  x,
  y1,
  y2, # Required Arguments
  y1.lim = NULL,
  y2.lim = NULL, # Axis limits
  y1.lab = "",
  y2.lab = "", # All the labels
  y1.legend = "",
  y2.legend = "", # Legends
  y1.col = "red",
  y2.col = "blue", # Colors of the plots
  y1.pch = 19,
  y2.pch = 22, # Pch
  las = 1,
  ...
) {
  par(mar = c(5, 4, 4, 5) + .1, las = 1)

  # Plot 1
  plot(x, y1, ylab = y1.lab, ylim = y1.lim, pch = y1.pch, col = y1.col, ...)
  axis(2, col = y1.col, col.axis = y1.col)

  # Plot 2
  par(new = TRUE)
  plot(
    x,
    y2,
    ylim = y2.lim,
    col = y2.col,
    pch = y2.pch,
    xaxt = "n",
    yaxt = "n",
    ylab = "",
    ...
  )

  axis(4)
  axis(4, col = y2.col, col.axis = y2.col)
  mtext(y2.lab, side = 4, line = 3, col = y2.col)
  legend(
    "topright",
    col = c(y1.col, y2.col),
    lty = 1,
    bty = 'n',
    pch = c(y1.pch, y2.pch),
    legend = c(y1.legend, y2.legend),
    text.col = c(y1.col, y2.col)
  )
}
# Resetting parameters (Required when you messed up with plot parameters)---------
# `resetPar()`
resetPar <- function() {
  dev.new()
  par(no.readonly = TRUE)
  dev.off()
}
# Plotting error bars in a basic R plot ----------------------------------------
# `plotCI(x, y = NULL, uiw, liw = uiw, ...)`
# Plotting errorbars
plotCI <- function(
  x,
  y = NULL,
  uiw,
  liw = uiw,
  ylim = NULL,
  sfrac = 0.01,
  add = FALSE,
  col = par("col"),
  lwd = par("lwd"),
  slty = par("lty"),
  xlab = deparse(substitute(x)),
  ylab = deparse(substitute(y)),
  ...
) {
  # from Bill Venables, R-list
  if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (is.null(y)) {
    if (is.null(x)) {
      stop("both x and y NULL")
    }
    y <- as.numeric(x)
    x <- seq(along = x)
  }
  ui <- y + uiw
  li <- y - liw
  if (is.null(ylim)) {
    ylim <- range(c(y, ui, li), na.rm = TRUE)
  }
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
eq.reg <- function(reg) {
  rmse <- round(sqrt(mean(resid(reg)^2)), 2)
  coefs <- summary(reg)$coefficients
  if (dim(coefs)[1] == 2) {
    b0 <- round(coefs[1], 2)
    b1 <- round(coefs[2], 2)
    r2 <- round(summary(reg)$r.squared, 2)
    ar2 <- round(summary(reg)$adj.r.squared, 2)
    Pvalue.slp <- round(coefs[8], 3)
    Pvalue.int <- round(coefs[7], 3)
    eqn <- bquote(
      italic(bold(Y)) ==
        .(b0)["(" * p == .(Pvalue.int) * ")"] +
          .(b1) * italic(bold(X))["(" * p == .(Pvalue.slp) * ")"] * "," ~
        ~ r^2 == .(r2) * "," ~
        ~ adj_r^2 == .(ar2) * "," ~
        ~ RMSE == .(rmse)
    )
  } else if (dim(coefs)[1] == 3) {
    c0 <- round(coefs[1, 1], 2)
    c1 <- round(coefs[2, 1], 2)
    c2 <- round(coefs[3, 1], 2)
    cr2 <- round(summary(reg)$r.squared, 2)
    car2 <- round(summary(reg)$adj.r.squared, 2)
    Pvalue.c0 <- round(coefs[1, 4], 3)
    Pvalue.c1 <- round(coefs[2, 4], 3)
    Pvalue.c2 <- round(coefs[3, 4], 3)
    eqn <- bquote(
      italic(bold(Y)) ==
        .(c0)["(" * p == .(Pvalue.c0) * ")"] +
          .(c1) * italic(bold(X))["(" * p == .(Pvalue.c1) * ")"] +
          .(c2) * italic(bold(X^2))["(" * p == .(Pvalue.c2) * ")"] * "," ~
        ~ r^2 == .(cr2) * "," ~
        ~ adj_r^2 == .(car2) * "," ~
        ~ RMSE == .(rmse)
    )
  }
}

lm_eqn <- function(reg) {
  coefs <- summary(reg)$coefficients
  if (dim(coefs)[1] == 2) {
    eq <- substitute(
      y == a + b %.% x * "," ~ ~ italic(r)^2 ~ "=" ~ r2,
      list(
        a = format(coef(reg)[[1]], digits = 2),
        b = format(coef(reg)[[2]], digits = 2),
        r2 = format(summary(reg)$r.squared, digits = 3)
      )
    )
  } else if (dim(coefs)[1] == 3) {
    eq <- substitute(
      y == a + b %.% x + c %.% x^2 * "," ~ ~ r^2 ~ "=" ~ r2,
      list(
        a = format(coef(reg)[[1]], digits = 2),
        b = format(coef(reg)[[2]], digits = 2),
        c = format(coef(reg)[[3]], digits = 2),
        r2 = format(summary(reg)$r.squared, digits = 3)
      )
    )
  }
  #as.character(as.expression(eq));
  return(eq)
}
# Plotting grey confidence bars in a base R plot -------------------------------
# `conf.bar(x, reg, alpha, varname = "")`

# Plotting grey confidence bars
# Don't use subset inside lm
# Keep same variable names in varname as in fitting dataframe
conf.bar <- function(x, reg, alpha, varname = "") {
  coefs <- summary(reg)$coefficients
  newx <- seq(min(x), max(x), length.out = 12)
  newdata <- data.frame(newx)
  if (identical(varname, "") == 1) {
    names(newdata) <- names(reg$coefficients)[2]
  } else {
    names(newdata) <- varname
  }
  #        print(newdata)
  #       print(reg)
  preds <- predict(reg, I(newdata), interval = 'confidence')
  polygon(
    c(rev(newx), newx),
    c(rev(preds[, 3]), preds[, 2]),
    col = rgb(0.1, 0.1, 0.1, alpha),
    border = NA
  )
  # model
  if (dim(coefs)[1] == 2) {
    abline(reg)
  } else if (dim(coefs)[1] > 2) {
    x.temp = seq(min(x), max(x), length.out = 100)
    new = list()
    new[[names(reg$coefficients)[2]]] = x.temp
    lines(x.temp, predict(reg.T, newdata = new))
  }
  # intervals
  lines(newx, preds[, 3], lty = 'dashed', col = 'red')
  lines(newx, preds[, 2], lty = 'dashed', col = 'red')
}

# Export plots to pdf or png-----------------------------------------
# `export(filename = "test.pdf)`

export <- function(fname) {
  dev.copy(
    png,
    filename = paste0(fname, ".png"),
    width = 2000,
    height = 1000,
    antialias = "cleartype",
    pointsize = 16
  )
  dev.off()
  dev.copy(x11)
  dev.copy2pdf(
    file = paste0(fname, ".pdf"),
    width = 14,
    height = 9,
    paper = "a4r",
    out.type = "pdf"
  )
  dev.off()
}

# Open and closing of pdf device ------------------------------------------
# `openPdf(pdfname = "test")`
# `closePdf(pdfname = "test")`

openPdf <- function(
  pdfname = "test",
  width = 8.3,
  height = 11.7,
  subfolder = "04-Graphics",
  path = getwd()
) {
  closeFigs()
  # Opening "test.pdf"
  if (!file.exists(subfolder)) {
    dir.create(file.path(path, subfolder))
  }
  pdfname <- subfolder %/% pdfname
  {
    pdf(
      paste0(pdfname, " [R].pdf"),
      width = width,
      height = height, #paper = "a4r",
      onefile = T,
      bg = "white"
    )
    dev.control("enable")
  }
}
# closePdf(pdfname = "test")
closePdf <- function(pdfname = "test", subfolder = "04-Graphics") {
  # Closing Images
  dev.off()
  # opening pdf file
  system(paste0("open ", shQuote(subfolder %/% pdfname %+% " [R].pdf")))
}
# ggsave.adv ()---------------------------------------------------------------------------------------------
ggsave.adv <- function(
  filename = "temp",
  prefix = "99ZZ-99z-99",
  plot = last_plot(),
  asp.WbyH = 4 / 3,
  width = 8,
  height = NULL,
  ext = "svg"
) {
  install("svglite")
  subfolder = "04-Graphics"
  path = getwd()
  if (!file.exists(subfolder)) {
    dir.create(file.path(path, subfolder))
  }

  if ((!is.null(asp.WbyH)) + (!is.null(width)) + (!is.null(height)) == 3) {
    if (asp != height / width) {
      errorCondition("aspect ratio and height/weight are not equal")
    }
  } else if (is.null(height)) {
    height = width / asp.WbyH
  } else if (is.null(width)) {
    width = height * asp.WbyH
  } else {
    errorCondition(
      "Please provide atleast two variables among height,
                   width and aspect ratio"
    )
  }

  for (type in ext) {
    file <- prefix %+% "-" %+% filename %+% " [R]." %+% type
    ggsave(
      filename = file,
      plot = plot,
      width = width,
      height = height,
      unit = "cm",
      path = subfolder,
      device = type
    )
  }
  system(paste0("open ", shQuote(subfolder %/% file)))
}
# Clear data or screen -------------------------------------------
# `clr()`
# `clr("all")`
clr <- function(mode = "notall", except = NULL, env = globalenv()) {
  # env <- globalenv()
  ll <- ls(envir = env)

  if (is.null(except)) {
    except = c("clr", "clc")
  } else {
    except = c("clr", "clc", except)
  }

  ll <- ll[!ll %in% except]

  if ((mode == "all") || (mode == "All")) {
    rm(list = ll, envir = env)
  } else if (mode == "notall") {
    functions <- lsf.str(envir = env)
    rm(list = setdiff(ll, functions), envir = env)
  }
}
# clc()
clc <- function() {
  cat("\014")
}

# Close all open figures ------
# `closeFigs()`
closeFigs <- function() {
  graphics.off()
}

# Convert any object to a string -----------------------------------------------
# `to.chr(string = objName)`
to.chr <- function(string) {
  as.character(substitute(string))
}

# Advance function for converting columns to numeric ---------------------------
# `as.numeric.adv(x)`
as.numeric.adv <- function(x) {
  if (class(x) == "factor") {
    as.numeric(levels(x))[x]
  } else {
    as.numeric(x)
  }
}

# Converting a data frame column to numeric ------
col2num <- function(df, column, use.names = FALSE) {
  unlist(df[column], use.names = use.names)
}

# Converting classes of columns of data frame -----------------------------------
# `convertClass(obj = df, types = to.chr(fnin - fnic))`
convertClass <- function(obj, types, date.origin = "1970-01-01") {
  # Input data is pure dataframe
  # Check you give the classes in lowerclass
  # It works for all columns
  if (class(obj)[1] != "data.frame") {
    # in case obj is tbl_df or tbl
    obj <- as.data.frame(obj)
  }

  if (is.character(types)) {
    types <- types %>%
      gsub(., pattern = ",", replacement = "") %>%
      gsub(., pattern = " ", replacement = "") %>%
      gsub(., pattern = "-", replacement = "") %>%
      strsplit(., "") %>%
      unlist(.)
  }

  origin <- date.origin

  as.date <- function(x) {
    if (class(x) == "factor") {
      as.Date(x, origin = origin)
    } else {
      as.Date(as.numeric.adv(x), origin = origin)
    }
  }
  out <- lapply(1:length(obj), FUN = function(i) {
    FUN1 <- switch(
      tolower(types[i]),
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
      l = as.logical
    )
    FUN1(obj[, i])
  })
  names(out) <- colnames(obj)
  return(as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE))
}

# Generating unique file names -------------------------------------------------
# `uniqueFilename(filename = "test.pdf", default = FALSE)`
#tempfile
uniqueFilename <- function(filename, default = FALSE) {
  sysTime <- format(Sys.time(), format = "%y-%b-%d-%H%M%S")
  if (default == TRUE) {
    return(sysTime %+% '-' %+% filename)
  } else {
    return(filename %+% '-' %+% sysTime)
  }
}

# Sandeep: Borrowed Functions
# From Sandeep
# Extracting different summary of linear model ---------------------------------
# `summaryLM(reg)`

summaryLM <- function(reg) {
  out = {}
  out$Slope <- coef(reg)[2]
  out$Intercept <- coef(reg)[1]
  out$Slope25 <- confint(reg)[2, "2.5 %"]
  out$Slope95 <- confint(reg)[2, "97.5 %"]
  out$Intercept25 <- confint(reg)[1, "2.5 %"]
  out$Intercept95 <- confint(reg)[1, "97.5 %"]
  out$SlopePval <- summary(reg)$coefficients[2, "Pr(>|t|)"]
  out$InterceptPval <- summary(reg)$coefficients[1, "Pr(>|t|)"]
  out$RSquared <- summary(reg)$r.squared
  out$AdjRSquared <- summary(reg)$adj.r.squared
  return(out)
}

# Count occurrence of a character in a string -----------------------------
# `char.count (string = "string", Char = "i")`
char.count <- function(string, Char = ".") {
  return(length(unlist(strsplit(string, Char, fixed = TRUE))) - 1)
}

# Add Path to file name ----------------------------------------------------
# `getwd()`
"%/%" = function(path, file) {
  if (substr(file, 1, 1) == "/") {
    file = substr(file, 2, nchar(file))
  }
  if (substr(file, nchar(file), nchar(file)) == "/") {
    file = substr(file, 1, nchar(file) - 1)
  }
  if (substr(path, nchar(path), nchar(path)) != "/") {
    path = paste0(path, "/")
  }
  return(paste0(path, file))
}

# Not in the list ----------------------------------------------------
# `getwd()`
"%!in%" = function(x, y) !('%in%'(x, y))

# Save data as csv file (no need to worry about path and extension) ------------
# `write.csv.adv (data, file.name, path = getwd(), subfolder = "Output-Tables" )`
write_csv.adv <- function(
  data,
  file.name,
  path = getwd(),
  subfolder = "03-Tables",
  quote = "none",
  fun_family = "csv",
  ...
) {
  install("readr")
  # if subfolder doesn't exist then create it.
  if (!file.exists(subfolder)) {
    dir.create(file.path(path, subfolder))
  }
  # In case my file name already have .csv in it
  # we don't wan't to add another .csv
  if (substr(file.name, nchar(file.name) - 3, nchar(file.name)) != ".csv") {
    file = paste0(file.name, ".csv")
  } else {
    file = gsub("(.csv)+$", ".csv", file.name)
  }
  if (fun_family == "csv") {
    write_csv(x = data, file = path %/% subfolder %/% file, quote = quote, ...)
  } else if (fun_family == "csv2") {
    write_csv2(x = data, file = path %/% subfolder %/% file, quote = quote, ...)
  } else if (fun_family == "excel_csv") {
    write_excel_csv(
      x = data,
      file = path %/% subfolder %/% file,
      quote = quote,
      ...
    )
  } else if (fun_family == "excel_csv2") {
    write_excel_csv2(
      x = data,
      file = path %/% subfolder %/% file,
      quote = quote,
      ...
    )
  } else {
    stop(
      "Please provide fun_family parameter from 'csv', `csv2`, `excel_csv`, `excel_csv2` only!"
    )
  }
}

# Saving session info to a txt file --------------------------------------------
# `runInfo()`
runInfo <- function(time = FALSE) {
  closeAllConnections()
  fileName <- file(basename(getwd()) %+% "-Session Info.txt")
  sink(fileName)

  if (time == TRUE) {
    msg <- "Ran by " %+%
      getUser() %+%
      " on " %+%
      as.character(Sys.time()) %+%
      "\n"
  } else {
    msg <- "Ran by " %+%
      getUser() %+%
      " on " %+%
      as.character(Sys.Date()) %+%
      "\n"
  }
  catn(msg)
  catn(capture.output(sessionInfo(), split = TRUE))
  sink()
  closeAllConnections()
}

# Get User Name -----------------------------------------------------------
# `getUser()`
getUser <- function() {
  env <- if (.Platform$OS.type == "windows") "USERNAME" else "USER"
  unname(Sys.getenv(env))
}

# Publication Theme for ggplot2 -------------------------------------------
# `theme_publication(base_size=14, base_family="")`
# `scale_fill_Publication()`
# `scale_colour_Publication`

theme_Publication <- function(base_size = 14, base_family = "") {
  install("grid")
  install("ggthemes")
  (theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
      text = element_text(),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line.x = element_line(colour = "black", size = 1),
      axis.line.y = element_line(colour = "black", size = 1),
      axis.ticks = element_line(),
      panel.grid.major = element_line(colour = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(0.2, "cm"),
      legend.margin = unit(0, "cm"),
      legend.title = element_text(face = "bold"),
      plot.margin = unit(c(10, 5, 5, 5), "mm"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "bold")
    ))
}
# scale_fill_Publication()
scale_fill_Publication <- function(...) {
  install("scales")
  discrete_scale(
    "fill",
    "Publication",
    manual_pal(
      values = c(
        "#386cb0",
        "#fdb462",
        "#7fc97f",
        "#ef3b2c",
        "#662506",
        "#a6cee3",
        "#fb9a99",
        "#984ea3",
        "#ffff33"
      )
    ),
    ...
  )
}

# scale_colour_Publication()
scale_colour_Publication <- function(...) {
  install("scales")
  discrete_scale(
    "colour",
    "Publication",
    manual_pal(
      values = c(
        "#386cb0",
        "#fdb462",
        "#7fc97f",
        "#ef3b2c",
        "#662506",
        "#a6cee3",
        "#fb9a99",
        "#984ea3",
        "#ffff33"
      )
    ),
    ...
  )
}

# Reset Everything (Like a new R session) --------------------------------------
# `reset()`
reset <- function() {
  clr()
  clc()
  closeFigs()
  invisible(resetPar())
}

# Concatenate object as Text -------------------------------------------
# `"a" %+% "b"`
"%+%" = function(obj_1, obj_2) {
  paste(obj_1, obj_2, sep = "")
}
# cat() that appends a newline ---------------------------------------
# `catn(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)`
catn = function(
  ...,
  file = "",
  sep = " ",
  fill = FALSE,
  labels = NULL,
  append = FALSE,
  console = TRUE,
  color = NULL,
  newline = TRUE
) {
  if (newline) {
    n = '\n'
  } else {
    n = ""
  }

  if (console) {
    if (!is.null(color)) {
      install("crayon")
      eval(
        expr = parse(
          text = "cat(" %+%
            color %+%
            "(" %+%
            quote(...) %+%
            "),'" %+%
            n %+%
            "')"
        )
      )
    } else {
      (cat(
        ...,
        n,
        file = file,
        sep = sep,
        fill = fill,
        labels = labels,
        append = append
      ))
    }
  }
  if (file != "") {
    cat(
      ...,
      n,
      file = file,
      sep = sep,
      fill = fill,
      labels = labels,
      append = append
    )
  }
}
# List objects available in the environment-------------------------------------
# `list.objects(env = .GlobalEnv)`
list.objects <- function(env = .GlobalEnv) {
  if (!is.environment(env)) {
    env <- deparse(substitute(env))
    stop(sprintf('"%s" must be an environment', env))
  }
  obj.type <- function(x) class(get(x, envir = env))
  foo <- sapply(ls(envir = env), obj.type)
  object.name <- names(foo)
  names(foo) <- seq(length(foo))
  dd <- data.frame(CLASS = foo, OBJECT = object.name, stringsAsFactors = FALSE)
  dd[order(dd$CLASS), ]
}

# Check whether all the elements are same or identical -------------------------
# `all.same(vector)`
# `all.identical(x, warn = FALSE)`
#
# usage
# all.same(c(1,1,NA))
# all.same(c(1,1,NA), na.rm = TRUE)
# all.same(c(NA, NA, NA), na.rm = TRUE, all.na.as = TRUE)
# all.same(c(NA, NA ,NA), na.rm = FALSE)
all.same = function(vector, na.rm = FALSE, all.na.as = NA) {
  # Give a vector of values and it will tell whether all the values are same or not
  # Returns boolean results in terms of True or False
  if (!na.rm) {
    if (length(unique(vector)) == 1) {
      if (is.na(unique(vector))) {
        return(all.na.as)
      } else {
        return(TRUE)
      }
    } else {
      return(FALSE)
    }
  } else {
    vector <- vector[!is.na(vector)]
    if (length(vector) == 0) {
      return(all.na.as)
    } else if (length(unique(vector)) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
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
    TF <- vapply(
      1:(length(x) - 1),
      function(n) identical(x[[n]], x[[n + 1]]),
      logical(1)
    )
    if (all(TF)) TRUE else FALSE
  }
}

# Stitch (Collapse a vector of strings) ------------------------------------
# `stich(vec, collapse = " ")`
stitch <- function(vec, collapse = " ") {
  paste0(as.character(vec), collapse = collapse)
}

# Remove na from a vector -------------------------------------------------
# `rm.na(vec)`
rm.na <- function(vec) {
  return(vec[!is.na(vec)])
}

# Merging two data frame like mean +- sd -----------------------------------
# `fuse(df.prfx, df.sufx, merged.Cols , link = "±")`
fuse <- function(df.prfx, df.sufx, merged.Cols, link = "±") {
  # Checking no. of cols are identical
  if (dim(df.prfx)[2] != dim(df.sufx)[2]) {
    stop("Number of columns are not matching, please recheck")
  }
  if (dim(df.prfx)[1] != dim(df.sufx)[1]) {
    print.warn("Please note that number of rows are not matching!")
  }
  if (!identical(sort(names(df.prfx)), sort(names(df.sufx)))) {
    stop("Columns names are not matching, please fix")
  }
  # Merging matrix
  merged <- merge(
    df.prfx,
    df.sufx,
    by = merged.Cols,
    all = TRUE,
    suffixes = c(".prfx", ".sufx")
  )
  out <- as.data.frame(matrix(
    NA,
    nrow = dim(merged)[1],
    ncol = dim(df.prfx)[2]
  ))
  names(out) <- names(df.prfx)
  out[, merged.Cols] <- merged[, merged.Cols]
  to.fuse.cols <- setdiff(names(df.prfx), merged.Cols)

  for (cols in to.fuse.cols) {
    prfx.rows.blank <- merged[, cols %+% ".prfx"] %in% c("", " ")
    prfx.rows.Na <- is.na(merged[, cols %+% ".prfx"])
    prfx.rows.valid <- (!prfx.rows.blank & !prfx.rows.Na)
    sufx.rows.blank <- merged[, cols %+% ".sufx"] %in% c("", " ")
    sufx.rows.Na <- is.na(merged[, cols %+% ".sufx"])
    sufx.rows.valid <- (!sufx.rows.blank & !sufx.rows.Na)

    # Completely valid rows
    rows.valid <- prfx.rows.valid & sufx.rows.valid
    out[rows.valid, cols] <- paste(
      merged[rows.valid, cols %+% ".prfx"],
      merged[rows.valid, cols %+% ".sufx"],
      sep = " " %+% link %+% " "
    )

    # Only prefix valid rows
    only.prfx.rows.valid <- prfx.rows.valid & !sufx.rows.valid
    out[only.prfx.rows.valid, cols] <- merged[
      only.prfx.rows.valid,
      cols %+% ".prfx"
    ]

    # Only sufix valid rows
    only.sufx.rows.valid <- sufx.rows.valid & !prfx.rows.valid
    if (sum(only.sufx.rows.valid) > 0) {
      out[only.sufx.rows.valid, cols] <- merged[
        only.sufx.rows.valid,
        cols %+% ".sufx"
      ]
      print.warn("Watch out suffix df have valid rows while prefix df not!!")
    }

    rows.rest <- !(prfx.rows.valid | sufx.rows.valid)
    out[rows.rest, cols] <- ""
  }
  return(out)
}

# Moving Average & Standard Deviations ------------------------------------
# `mov.avg(x, width, align = "center", partial = FALSE, na.rm = FALSE )`
mov.avg <- function(
  x,
  width,
  align = "center",
  partial = FALSE,
  na.rm = FALSE
) {
  # Installing zoo
  install("zoo")
  out <- rollapply(
    data = x,
    width = width,
    FUN = mean,
    na.rm = na.rm,
    align = align,
    fill = NA,
    partial = partial
  )
  return(out)
}

# `mov.sd(x, width, align = "center", partial = FALSE, na.rm = FALSE )`
mov.sd <- function(x, width, align = "center", partial = FALSE) {
  install("zoo")
  out <- rollapply(
    data = x,
    width = width,
    FUN = sd,
    na.rm = na.rm,
    align = align,
    fill = NA,
    partial = partial
  )
  return(out)
}

# Open Current Directory directly from R ---------------------------------------
# 'openwd()'
openwd <- function(dir = getwd()) {
  if (.Platform['OS.type'] == "windows") {
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

# List objects by their size ----------------------------------------------
# `lsos()`
.ls.objects <- function(
  pos = 1,
  pattern,
  order.by,
  decreasing = FALSE,
  head = FALSE,
  n = 5
) {
  napply <- function(names, fn) {
    sapply(names, function(x) {
      fn(get(x, pos = pos))
    })
  }
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto"))
  })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x) {
    as.numeric(dim(x))[1:2]
  }))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by)) {
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  }
  if (head) {
    out <- head(out, n)
  }
  out
}

lsos <- function(..., n = 10) {
  .ls.objects(..., order.by = "Size", decreasing = TRUE, head = TRUE, n = n)
}

# function to see logs/ diary of work and project progress ------
diary <- function(
  dir = "C:/Users/Ankur/OneDrive - Indian Institute of Science/Work",
  filename = "[Diary].xlsm",
  highlight_proj = "Growth"
) {
  #Installing necessary plugins
  install(c("tidyverse", "DT", "readxl", "htmlwidgets"))
  #Name and path for the file
  table <- read_excel(dir %/% filename, sheet = "Work_Diary") %>%
    mutate(
      `#` = as.integer(`#`),
      Project = as.factor(Project),
      Section = as.factor(Section),
      File = as.factor(File),
      Task_Type = as.factor(Task_Type)
    )

  #  Creating html table
  widget <- datatable(
    table,
    class = 'display',
    filter = list(position = 'top', clear = TRUE, plain = FALSE),
    extensions = c('FixedHeader', "KeyTable", "Buttons"),
    plugins = "ellipsis",
    escape = TRUE,
    options = list(
      paging = TRUE,
      searchHighlight = TRUE,
      search = list(smart = TRUE),
      pageLength = 400,
      autoWidth = TRUE,
      fixedHeader = TRUE,
      keys = TRUE, # Keytable
      dom = 'lBfrtip',
      buttons = I('colvis'),
      columnDefs = list(
        list(width = '100%', targets = "_all", className = 'hower'),
        list(
          targets = "_all",
          render = JS("$.fn.dataTable.render.ellipsis(50, false )")
        )
      )
    )
  ) %>%
    DT::formatDate(2, "toLocaleString") %>%
    DT::formatStyle(
      columns = colnames(table),
      `font-size` = '100%',
      `width` = "100%"
    )
  #  Saving html table in the same folder as html file
  DT::saveWidget(widget, dir %/% filename %+% '.html', selfcontained = TRUE)

  #  opening the file
  browseURL(dir %/% filename %+% '.html')
  #https://datatables.net/manual/styling/classes
}

# blank as NA ------
#Usage: data %>% mutate_each(funs(blank2NA))
blank2NA <- function(x) {
  install("dplyr")
  if ("factor" %in% class(x)) {
    x <- as.character(x)
  } # since ifelse wont work with factors
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

# sum.adv ------
# Modified sum function to handle values like all NA
sum.adv <- function(x) {
  if (sum(!is.na(x)) == 0) {
    out <- NA
  } else {
    out <- sum(x, na.rm = TRUE)
  }
  return(out)
}

# max.adv ------
# Modified max function to handle values like all NA
max.adv <- function(x) {
  if (sum(!is.na(x)) == 0) {
    out <- NA
  } else {
    out <- max(x, na.rm = TRUE)
  }
  return(out)
}

# max.adv ------
# Modified min function to handle values like all NA
min.adv <- function(x) {
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
# Example usage
# data("iris")
# table <- data.frame(var = names(iris)[-dim(iris)[2]], filter = NA)
# stat.summary(colTable = table, group_var = "Species",df_in = iris)
# out_w <- stat.summary(colTable = table, group_var = "Species", df_in = iris, format = "wide")
# out_l <- stat.summary(colTable = names(iris)[1:4], group_var = "Species", df_in = iris, format = "long")
# out_list <- stat.summary(colTable = table, group_var = "Species", df_in = iris, format = "list")
#
stat.summary <- function(colTable, group_var, df_in, format = "wide") {
  list_df <- list()
  class = class(colTable)
  summary.vars <- c("mean", "sd", "cv", "max", "min", "n", "se")
  n.sum.vars <- length(summary.vars)
  n.gr.vars <- length(group_var)
  if ("data.frame" %!in% class) {
    if (class == "character") {
      colTable = data.frame(var = colTable, filter = NA)
    } else {
      stop(
        "Check the class of the colTable variable! \n
      It should be eithe character vector of variables names \n
      or \n
      a data.frame of variable names and associated filter columns."
      )
    }
  }

  for (f in (1:dim(colTable)[1])) {
    var.name <- colTable[f, 1]
    filter.var <- colTable[f, 2]
    filter.var.name <- filter.var

    if (is.na(filter.var) == TRUE) {
      filter.var = TRUE
    }

    if (format == "wide") {
      var.mean <- var.name %+% ".mean"
      var.sd <- var.name %+% ".sd"
      var.cv <- var.name %+% ".cv"
      var.max <- var.name %+% ".max"
      var.min <- var.name %+% ".min"
      var.n <- var.name %+% ".n"
      var.se <- var.name %+% ".se"
    } else if (format %in% c("long", "list")) {
      var.mean <- "mean"
      var.sd <- "sd"
      var.cv <- "cv"
      var.max <- "max"
      var.min <- "min"
      var.n <- "n"
      var.se <- "se"
    } else {
      print(format)
      stop("Please check the format! Default is `wide`, other is `long` ")
    }

    var <- colTable[f, 1] %+% "[" %+% filter.var %+% " == TRUE]"

    formula.mean <- "mean(" %+% var %+% ", na.rm = TRUE)"
    formula.sd <- "sd(" %+% var %+% ", na.rm = TRUE)"
    formula.cv <- var.sd %+% "*100/" %/% var.mean
    formula.max <- "max.adv(" %+% var %+% ")"
    formula.min <- "min.adv(" %+% var %+% ")"
    formula.n <- "sum.adv(!is.na(" %+% var %+% "))"
    formula.se <- var.sd %+% "/sqrt(" %+% var.n %+% ")"

    list_df[[var.name]] <- df_in %>%
      group_by_at(vars(group_var)) %>%
      summarise(
        !!var.mean := eval(parse(text = formula.mean)),
        !!var.sd := eval(parse(text = formula.sd)),
        !!var.cv := eval(parse(text = formula.cv)),
        !!var.min := eval(parse(text = formula.min)),
        !!var.max := eval(parse(text = formula.max)),
        !!var.n := eval(parse(text = formula.n)),
        !!var.se := eval(parse(text = formula.se))
      ) %>%
      mutate(var = var.name, filter.var = filter.var.name)
  }
  if (format == "wide") {
    out <- list_df %>%
      reduce(left_join, by = group_var) %>%
      as.data.frame() %>%
      select(-starts_with("var"), -starts_with("filter.var"))
  } else if (format == "long") {
    out <- list_df %>%
      reduce(bind_rows) %>%
      as.data.frame() %>%
      select(
        1:length(n.gr.vars),
        n.sum.vars + n.gr.vars + 1,
        n.sum.vars + n.gr.vars + 2,
        (n.gr.vars + 1):(n.gr.vars + n.sum.vars)
      )
  } else if (format == "list") {
    out = list_df
  } else {
    stop("Please check the format! Default is `wide`, other is `long` ")
  }
  return(out)
}

# pad.00------
# Pads a number or string with zeros
pad.00 <- function(string, width = 2, side = "left") {
  warning("pad.00 is deprecated. Please use pad_decimal() instead.")
  install("stringr")
  str_pad(string = string, width = width, side = side, pad = "0")
}
# pad_decimal------
# Pads a number or string with characters on left or right side.
# Usage
# pad_decimal(3.14, 3, 4, " ", "0")
pad_decimal <- function(
  x,
  width_left = 0,
  width_right = 2,
  char_left = " ",
  char_right = "0"
) {
  # x: input decimal number
  # width_left: desired width left side (excluding decimal point)
  # width_right: desired number of padding digits on the right side
  # char_left: padding character for the left side
  # char_right: padding character for the right side

  # split the number into integer and fractional parts
  int_part <- floor(x)
  frac_part <- x - int_part
  install("stringr")
  # convert the integer part to a character string and pad with specified character from left
  int_str <- stringr::str_pad(
    string = int_part,
    width = width_left,
    side = "left",
    pad = char_left
  )

  # convert the fractional part to a character string and pad with specified character from right

  frac_str <- formatC(
    round(frac_part, width_right),
    digits = width_right,
    format = "f"
  )

  # concatenate the integer and fractional parts with a decimal point in between
  paste(int_str, frac_str, sep = ".")
}
format_decimal <- function(x, n) {
  # Round the number to n decimal places
  x_rounded <- round(x, n)

  # Convert the rounded number to a character string with n decimal places
  x_formatted <- formatC(x_rounded, digits = n, format = "f")

  # Return the formatted string
  return(x_formatted)
}
# is.attrib.same ---------------
# Crosscheck whether two data sets are same in some columns
# Checks the internal consistency of the data by a particular column
is.attrib.same <- function(df, by_col, attrib_cols) {
  # Check that by_col and attrib_cols are mutually exclusive
  if (length(intersect(by_col, attrib_cols)) != 0) {
    stop(sprintf(
      "by_col and attrib_cols have operlapping column: %s",
      intersect(by_col, attrib_cols)
    ))
  } else {
    # Filtering rows where there are conflicting values for a tag
    # Removing columns which are consistent!
    # First term generates the string of TRUE by numbers of by_col
    # Second term checks whether all the entries in the attrib_cols columns are TRUE or not. If it is not then it filters it.
    df_check <- df %>%
      select(by_col, attrib_cols) %>%
      group_by_(by_col) %>%
      summarise_all(all.same) %>%
      filter_at(vars(attrib_cols), any_vars(. == FALSE)) %>%
      select_if(c(
        rep(TRUE, length(by_col)),
        sapply(.[attrib_cols], all) == FALSE
      ))
    return(df_check)
  }
}

# export2html -------------------------------------------------------------------------------------------
# export to html
# 'export2html(".R")'
export2html <- function(
  filename,
  folder = '05-Html',
  suppress_warnings = TRUE,
  browse = TRUE,
  output_file = NULL
) {
  env_new <- new.env()
  if (suppress_warnings) {
    suppressWarnings(rmarkdown::render(
      filename,
      output_dir = folder,
      clean = TRUE,
      quiet = TRUE,
      output_file = output_file,
      envir = env_new
    ))
  } else {
    rmarkdown::render(
      filename,
      output_dir = folder,
      clean = TRUE,
      quiet = TRUE,
      output_file = output_file,
      envir = env_new
    )
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
  rm(env_new)
}
# r2html -------------------------------------------------------------------------------------------------
# Funtion which converts normal R code into a rmarkdown code. There already exists some implementation, for example see <https://rmarkdown.rstudio.com/articles_report_from_r_script.html> for more details.
# This function however does an additional things.
# 1. Converts R-section to R markdown sections
# 2. Converts normal comments to roxygen comments automatically
# 3. Appends detailed output format
r2html <- function(numbered_section = TRUE) {
  invisible(install("dplyr"))
  file <- rstudioapi::getSourceEditorContext()$path
  flIn <- readLines(file)
  # open the file and read in all the lines
  head <- unlist(strsplit(flIn[2:4], split = '\n'))
  render <- unlist(strsplit(flIn[5:8], split = '\n'))
  time_now <- "# Last Rendered: " %+%
    format(
      Sys.time(),
      format = "%Y-%b-%d %H:%M:%S " %+%
        weekdays(as.Date(Sys.Date(), '%d-%m-%Y'))
    )
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
  if (!numbered_section) {
    text_block[10] <- "#'    number_sections: false"
  }
  # concatenate the old file with the new text
  flIn <- c(
    "#'---",
    head,
    text_block,
    render[1:2],
    time_now,
    render[3:4],
    "#'",
    "#'*****",
    flIn
  )
  secStrt <- which(grepl(flIn, pattern = "^#{1,4} ", perl = TRUE))
  secEnd <- which(grepl(flIn, pattern = "----", perl = TRUE))
  comLines <- which(grepl(flIn, pattern = "^+# "))
  secLines <- intersect(secStrt, secEnd)
  sketchLines <- which(grepl(flIn, pattern = "sketch\\(", perl = TRUE))
  for (i in 1:length(flIn)) {
    if (i %in% secLines) {
      flIn[i] <- flIn[i] %>%
        gsub(pattern = "[-]+$", replacement = "", x = .) %>%
        gsub(pattern = "^+# ", replacement = "#' # ", x = .) %>%
        gsub(pattern = "^+## ", replacement = "#' ## ", x = .) %>%
        gsub(pattern = "^+### ", replacement = "#' ### ", x = .) %>%
        gsub(pattern = "^+#### ", replacement = "#' #### ", x = .)
    } else if (i %in% comLines) {
      flIn[i] <- flIn[i] %>%
        gsub(pattern = "^+# ", replacement = "#' ", x = .) %+%
        "<br>"
    }
    if (i %in% sketchLines) {
      flIn[i] <- flIn[i] %>%
        gsub(pattern = ')$', replacement = ', export = FALSE)', x = .)
    }
  }
  filename = basename(file)
  fn = substr(filename, start = 1, nchar(filename) - 2)
  writeLines(flIn, con = "temp_rmd.R")
  export2html(
    "temp_rmd.R",
    folder = '05-Html',
    suppress_warnings = TRUE,
    browse = TRUE,
    output_file = fn
  )
  if (file.exists("temp_rmd.R")) {
    #Delete file if it exists
    file.remove("temp_rmd.R")
  }
}

# opendir -----------------------------------------------------------------------------------------------
# Function which opens the current working directory directly from the console
# Adapted from the stackoverflow user Dason <https://stackoverflow.com/users/1003565/dason>
# Stack overflow link <https://stackoverflow.com/questions/12135732/how-to-open-working-directory-directly-from-r-console>
opendir <- function(dir = getwd()) {
  if (.Platform['OS.type'] == "windows") {
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

# summary.adv -------------------------------------------------------------------------------------------
# Function to convert summary into a nice dataframe
summary.adv = function(data) {
  out <- data.frame(
    unclass(summary(data)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  return(out)
}

# summary.num -------------------------------------------------------------------------------------------
# Detailed summary of the numeric columns only
summary.num = function(data) {
  install("summarytools")
  out <- as.data.frame(summarytools::descr(
    data,
    transpose = TRUE,
    round.digits = 4
  ))
  rows <- rownames(out)
  out <- out %>% mutate(SE = Std.Dev / sqrt(N.Valid)) %>% as.data.frame()
  out$names <- rows
  out <- out[, c(
    17,
    15,
    14,
    3,
    1,
    7,
    17,
    2,
    8,
    9,
    16,
    10,
    17,
    4,
    5,
    6,
    11,
    12,
    13
  )]
  return(out)
}

# summary.non.num -------------------------------------------------------------------------------------------
# Function to convert summary into a nice dataframe
summary.non.num = function(data) {
  a <- sapply(data, class)
  cols <- names(a[!(a %in% c("numeric", "integer"))])
  out <- data.frame(
    unclass(summary(data %>% dplyr::select(all_of(cols)))),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  return(out)
}

# sketch.pptx -------------------------------------------------------------------------------------------
# Function to export the plot as editable powerpoint file
sketch.pptx <- function(figObj, prefix, figname, figObjName, figList = "") {
  install(c("officer"))
  subfolder = "04-Graphics"
  path = subfolder %/% prefix %+% "-" %+% figname %+% " [R]" %+% ".pptx"
  if (!file.exists(path)) {
    out <- read_pptx()
  } else {
    out <- read_pptx(path)
  }
  footer_text <- prefix %+%
    "-<...>.R: Figure: " %+%
    figList %+%
    "$" %+%
    figObjName
  out %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(
      value = figObj,
      location = ph_location_type(type = "body"),
      bg = "transparent"
    ) %>%
    ph_with(value = ".", location = ph_location_type(type = "title")) %>%
    ph_with(
      value = paste0(
        format(Sys.time(), format = "%Y-%b-%d %H:%M:%S "),
        weekdays(as.Date(Sys.Date(), '%d-%m-%Y'))
      ),
      location = ph_location_type(type = "dt")
    ) %>%
    ph_with(value = footer_text, location = ph_location_type(type = "ftr")) %>%
    ph_hyperlink(
      ph_label = slide_summary(.) %>%
        dplyr::filter(type == "ftr") %>%
        dplyr::select(ph_label),
      type = "ftr",
      href = figname %+% " [R]" %+% ".pdf"
    ) %>%
    print(target = path)
}

# print.figure ----------------------------------------------------------
# Printing a singleton figure by an object
print.figure <- function(
  figobj,
  prefix,
  figname,
  figObjName,
  figList = "",
  pdf = TRUE,
  pptx = FALSE
) {
  if (pdf) {
    print(figobj)
  }
  if (pptx) {
    sketch.pptx(
      figObj = figobj,
      prefix = prefix,
      figname = figname,
      figObjName = figObjName,
      figList = figList
    )
  }
}
# sketch ------------------------------------------------------------------------------------------------
# Function to save a singleton plot as pdf as well as ppt
sketch <- function(
  figObj,
  prefix = "99ZZ-99z-99",
  figname = "temp",
  ppt = FALSE,
  export = TRUE
) {
  subfolder = "04-Graphics"
  path = getwd()
  if (!file.exists(subfolder)) {
    dir.create(file.path(path, subfolder))
  }
  figname <- prefix %+% "-" %+% figname
  print(figObj)
  if (export == TRUE) {
    ggsave(
      plot = figObj,
      path = subfolder,
      figname %+% "[R].pdf",
      height = 210,
      width = 297,
      units = "mm"
    )
    system(paste0("open ", shQuote(subfolder %/% figname), "[R].pdf"))
    if (ppt == TRUE) {
      sketch.pptx(figObj = figObj, figname = figname)
    }
  }
}


# export.list.of.figures --------------------------------------------------
# Saves a list of figures into a joint pdf or pptx file.
export.list.of.figures <- function(
  figList,
  prefix = "99zz-99z-99",
  figname,
  pdf = TRUE,
  pptx = FALSE,
  ...
) {
  prefix.figname = prefix %+% "-" %+% figname
  if (pdf) {
    openPdf(pdfname = prefix.figname, ...)
  }
  figListName = substitute(figList)
  walk2(
    figList,
    names(figList),
    ~ print.figure(
      figobj = .x,
      prefix = prefix,
      figname = figname,
      figObjName = .y,
      figList = figListName,
      pptx = pptx
    )
  )
  if (pdf) {
    closePdf(pdfname = prefix.figname, ...)
  }
}
# interpolate -------------------------------------------------------------------------------------------
# Function to interpolate the values by fitting a smooth spline
interpolate <- function(x, y, df, y_per, graph = FALSE) {
  temp <- data.frame(x = x, y = y) %>%
    dplyr::filter(complete.cases(.)) %>%
    group_by(x) %>%
    summarise(y = mean(y, na.rm = TRUE)) %>%
    arrange(x)
  if (dim(temp)[1] < 4) {
    return(list(NA, NA))
  } else {
    x = temp$x
    y = temp$y
    df = floor(length(unique(x[!is.na(x)])) * .9)
    rm(temp)
    reg <- smooth.spline(x = x, y = y, df = df)
    xval <- approx(x = reg$y, y = reg$x, xout = y_per)$y
    if (graph) {
      plot(x, y)
      lines(reg)
      abline(h = y_per)
      abline(v = xval)
    }
    return(list(reg, xval))
  }
}

# create.dir.str ----------------------------------------------------------------------------------------
create.dir.str <- function() {
  dirs <- c(
    "01-Data",
    "01-Data/01-Raw",
    "01-Data/02-Processed",
    "02-Code",
    "03-Tables",
    "04-Graphics",
    "05-Html"
  )
  for (dir in dirs) {
    if (!file.exists(dir)) {
      dir.create(file.path(getwd(), dir))
      print("Created directory: " %+% dir)
    }
  }
}


# file.info.adv ---------------------------------------------------------------------------------------------
# given a file name this function gives the information of file creation, modified and accessed time-stamps along with the md5 check sum values.
file.info.adv <- function(file.name) {
  install("openssl")
  return(cbind(
    file.name,
    file.info(file.name),
    "md5" = as.character(md5(file.name))
  ))
}

# files.status() ----------------------------------------------------------------------------------------
# The purpose of this function is to track files which may not be under version control of the current repository.
# One use case is involves a sub-directory containing big data files which we don't want to keep under version control. A version control folder (ex. git) becomes very big after a while. However, we might be interested whether such files have been updated after an action. In such cases we can check the md5 checksums of the files or the changes in the last modification details of the files. This is exactly this function does.
#
# Usage
#
# files.status(folder_check = "01-Data, folder_out = "03-Tables")
#
# This will check whether the files in "01-Data" folder has been updated or not.
# if we choose to write the logs then we have to additionally provide the option "write = TRUE"
#
# Note:- Current directory has to be under git as function pulls the sha tag of the current git repository as well as the commit message.

files.status <- function(
  folder_check = "01-Data",
  folder_out = "03-Tables",
  write = FALSE
) {
  install(c("git2r", "tidyverse"))
  outfile <- folder_check %+% "_Logs.csv"
  file.names <- list.files(folder_check, recursive = TRUE, full.names = TRUE)
  if (!file.exists(folder_out %/% outfile)) {
    out_prev <- NULL
    out_prev_rows <- ""
  } else {
    out_prev <- read_csv(
      folder_out %/% outfile,
      col_types = cols(
        mtime = col_datetime(format = "%Y-%m-%d %T %Z"),
        ctime = col_datetime(format = "%Y-%m-%d %T %Z"),
        atime = col_datetime(format = "%Y-%m-%d %T %Z")
      )
    ) %>%
      mutate(
        mtime = format(mtime) %+% " IST",
        ctime = format(ctime) %+% " IST",
        atime = format(atime) %+% " IST"
      ) %>%
      as.data.frame()
    out_prev_rows <- do.call(paste0, out_prev)
  }
  git_mess = git2r::revparse_single(getwd(), "HEAD")
  suppressWarnings({
    out <- map_df(file.names, file.info.adv) %>%
      mutate(
        mtime = format(mtime, usetz = TRUE),
        ctime = format(ctime, usetz = TRUE),
        atime = format(atime, usetz = TRUE),
        sha = git_mess$sha,
        message = git_mess$message
      )
  })

  out_present <- do.call(paste0, out) %in% out_prev_rows
  if (all(out_present)) {
    message("All is well!")
  } else {
    out <- out
    temp <- bind_rows(out_prev, out)
    output <- temp[!duplicated(temp), ] %>%
      arrange(file.name, ctime, mtime, atime)
    message("Following files has been updated!")
    print(out[!out_present, "file.name"] %>% unlist())
    if (write) {
      write_csv(output, path = folder_out %/% outfile)
      message("Updated the information")
    }
  }
}


# paste.adv() -------------------------------------------------------------------------------------------
# paste which can ignore NA values
# taken from https://stackoverflow.com/questions/13673894/suppress-nas-in-paste
paste.adv <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F) {
    paste(..., sep = sep, collapse = collapse)
  } else if (na.rm == T) {
    paste.na <- function(x, sep) {
      x <- gsub("^\\s+|\\s+$", "", x)
      ret <- paste(na.omit(x), collapse = sep)
      is.na(ret) <- ret == ""
      return(ret)
    }
    df <- data.frame(..., stringsAsFactors = F)
    ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))

    if (is.null(collapse)) {
      ret
    } else {
      paste.na(ret, sep = collapse)
    }
  }
}


# hr() --------------------------------------------------------------------------------------------------
# to print horizontal bar on the console
hr <- function(width = 80) {
  catn(paste0(rep("\u2500", width), collapse = ""))
}
# get.empty.columns() -----------------------------------------------------------------------------------
# Function gets the empty columns of any stand, mortality or regeneration file
# usage
# get_empty.columns(data = stand.df, group_by = "census)
get.empty.columns <- function(data, group.cols) {
  install("knitr")
  text = deparse(substitute(group.cols))
  out <- data %>%
    group_by({{ group.cols }}) %>%
    summarise_all(
      ~ all(
        is.na(.) |
          {
            as.character(.) == ""
          }
      )
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(empty.any = sum(c_across(matches(setdiff(names(.), text))))) %>%
    ungroup() %>%
    dplyr::filter(empty.any > 0) %>%
    dplyr::select(-empty.any) %>%
    mutate(across(matches(setdiff(names(.), text)), ~ if_else(., "X", "")))
  if (dim(out)[1] > 0) {
    print(kable(out, format = "rst"))
  } else {
    catn("There are no empty columns")
  }
}

# drop_unfit_cols() ------------------------------------------------------------------------------------------
drop_unfit_cols <- function(data, ..., contains = "", na.rm = TRUE) {
  vars <- tidyselect::eval_select(expr(c(...)), data)
  if (!is_empty(vars)) {
    data.filtered = data %>% dplyr::select(all_of(vars))
  } else {
    data.filtered = data
  }
  if (any(is.na(contains))) {
    stop("contains can not be NA!")
  }
  if (na.rm) {
    cols <- names(data.filtered)[apply(data.filtered, 2, function(x) {
      all(is.na(x) | x %in% contains)
    })]
  } else {
    cols <- names(data.filtered)[apply(data.filtered, 2, function(x) {
      all(x %in% contains)
    })]
  }
  out <- data %>% select(-all_of(cols))
  return(out)
}

# drop_unfit_rows() -------------------------------------------------------------------------------------
drop_unfit_rows <- function(data, ..., contains = c(""), na.rm = TRUE) {
  vars <- tidyselect::eval_select(expr(c(...)), data)
  if (is_empty(vars)) {
    vars = vars(all_of(names(data)))
  }
  if (any(is.na(contains))) {
    stop("contains can not be NA!")
  }
  if (na.rm) {
    out <- data %>%
      filter_at(vars, any_vars(!(is.na(.) | . %in% contains)))
  } else {
    out <- data %>%
      filter_at(vars, any_vars(!(is.na(.))))
  }
  return(out)
}

# interpolate.y.vs.x() -----------------------------------------------------
# A function to interpolate the x values given y is know.
# Useful to extract values from a cumulative function, say 50%
# when the exact value at 50% doesn't exist.
interpolate.y.vs.x <- function(data, x.col, y.col, y.out = c(25, 50, 75)) {
  data = data |> dplyr::filter(!is.na(get(x.col)), !is.na(get(y.col)))
  x = data[, x.col] %>% unlist()
  y = data[, y.col] %>% unlist()

  # Creating an empty data.frame to store values
  out = data.frame(y = y.out, x = NA_real_)
  names(out) <- c(y.col, x.col)

  # Case when one of the column is all NA
  if (all(is.na(x)) | all(is.na(y)) | length(x) * length(y) == 0) {
    out[, x.col] = NA_real_
    return(out)
    exit()
  }

  # Removing

  # Check whether vector x and y are of same length
  if (length(x) != length(y)) {
    stop("x and y are not of same length!")
  }

  # Check whether both vectors (x, y) are sorted.
  if (is.unsorted(x)) {
    stop("x is not sorted!")
  }
  if (is.unsorted(y)) {
    stop("y is not sorted!")
  }

  # Loop to go through each y.out values
  for (i.y in c(1:length(y.out))) {
    y.int = y.out[i.y]

    # Finding closest value to y.int
    i = which(abs(y - y.int) == min(abs(y - y.int)))

    # Handling cases where multiple i corresponding to same y.int exists
    if (length(i) > 1) {
      k = length(i)
      i = round(median(i))
    } else {
      k = 1
    }

    # Considering range of i for the interpolation
    i.l = i - k
    i.h = i + k

    if (y[i] == y.int) {
      # Case where there is an exact match
      x.out = x[i]
    } else {
      if (i - k < 1) {
        # Case when i.l each zero or negative
        x.out = x[i]
        i.l = 1
      } else if (i + k == length(x)) {
        # Case when i.h is more than the vector length
        x.out = x[length(x)]
        i.h = length(x)
      } else {
        xx = x[seq(i.l, i.h, 1)]
        yy = y[seq(i.l, i.h, 1)]
        val = approx(x = yy, y = xx, xout = y.int)
        x.out = val$y
      }
    }
    out[i.y, x.col] = x.out
  }
  return(out)
}
# resize.ggplot.panel() ---------------------------------------------------
# Function to precisely size the actual grid panel of a ggplot, irrespective of
# size of axes labels and titles
# here p is the ggplot figure object
# Usage
# fig = ggplot(cars, aes(speed, dist)) +
#   geom_point()
# plot(fig)
# fig.resized = resize.ggplot.panel(p = fig, width = unit(10, "cm"), height = unit(10, "cm"))
# grid::grid.newpage()
# grid::grid.draw(fig.resized)

resize.ggplot.panel = function(
  p = NULL,
  g = ggplotGrob(p),
  width = unit(15, "cm"),
  height = unit(15, "cm")
) {
  install("grid")
  panel_index_w <- g$layout$l[g$layout$name == "panel"]
  panel_index_h <- g$layout$t[g$layout$name == "panel"]
  g$widths[[panel_index_w]] <- width
  g$heights[[panel_index_h]] <- height
  class(g) <- c("fixed", class(g), "ggplot")
  return(g)
}

# %<% ---------------------------------------------------------------------
# Adding a variable to a list with the same name
# Usage:
# t = list()
# df = data.frame(a = 1, b = 2)
# t %<% df
# View(t)
"%<%" <- function(t, v) {
  var = deparse(substitute(v, current_env()))
  t.name = deparse(substitute(t, current_env()))
  eval(
    parse(text = paste0(t.name, "[['", var, "']]", "=", var)),
    envir = parent.frame()
  )
  return(invisible())
}

# str.list() --------------------------------------------------------
# Function to see list structure (names only) in a form of a tree.
str.list <- function(
  X,
  prefix1 = "",
  prefix2 = "",
  prefix3 = "",
  prefix4 = "",
  a = 1
) {
  # Box symbols key
  # "\U2500" :  ─
  # "\U2502" :  │
  # "\U251C" :  ├
  # "\U2514" :  └
  if (a == 1) {
    var = deparse(substitute(X))
    cat(var) #,"\n")#, "\U2502", sep = "")
    Y = list()
    Y[[1]] = X
  } else {
    Y = X
  }

  if (is.list(Y)) {
    for (i in seq_along(Y)) {
      cat(if (i < length(Y)) prefix1 else prefix3, names(Y)[i], "\n", sep = "")
      prefix <- if (i < length(Y)) prefix2 else prefix4
      str.list(
        Y[[i]],
        paste0(prefix, "\U251C\U2500"), #"├─"
        paste0(prefix, "\U2502 "), #"│ "
        paste0(prefix, "\U2514\U2500"), #"└─"
        paste0(prefix, "  "), #"  "
        a = 2
      )
    }
  }
}

# get.file.prefix() -------------------------------------------------------
# Extracts prefix (ex.01aa) of custom file names such as "01aa-script.R"
get.file.prefix <- function(file = get.source.file.name(), suffix = "_") {
  nn = substr(file, 1, 2) |>
    as.numeric() |>
    is.na() |>
    isFALSE() |>
    suppressWarnings()

  tt = substr(file, 3, 4) |>
    as.numeric() |>
    is.na() |>
    isTRUE() |>
    suppressWarnings()

  if (nn & tt) {
    prefix = substr(file, 1, 4)
  } else {
    prefix = NA
  }
  if (is.na(prefix)) {
    prefix = ""
  } else {
    prefix = prefix %+% suffix
  }
  return(prefix)
}

# get.source.file.name() --------------------------------------------------
# get current active source file name
# Ex. get.source.file.name()
# [1] util_Global.R
get.source.file.name <- function() {
  rev(strsplit(rstudioapi::getSourceEditorContext()$path, split = "/")[[1]])[1]
}

# store.table() -----------------------------------------------------------
# This is custom table saving (with sequential file naming) only if the changes exists.
# Usage:
# l = list()
# df = data.frame(x = 1 , y = 1)
# store.table(filename = "Test", data = df, lt = l, console = T)
# store.table(filename = "Test", data = df, lt = l, console = T)
# str.list(l)
# df = data.frame(x = 2 , y = 1)
# store.table(filename = "Test", data = df, lt = l, console = T)
# str.list(l)
# df = data.frame(x = 2 , y = 2)
# store.table(filename = "Test-New", data = df, lt = l, console = T)
# str.list(l)

store.table <- function(
  filename,
  data,
  lt,
  check = T,
  subfolder = "03-Tables",
  console = FALSE,
  fun_family = "csv",
  envir = rlang::caller_env(),
  ...
) {
  install("rlang")

  # Extracting names of the data and the list
  data.name = rlang::enexpr(data)
  lt.name = deparse(substitute(lt))

  # Extracting source file name
  current.file.name = get.source.file.name()

  # Extracting the file prefix if exists
  prefix = get.file.prefix(current.file.name)

  # Generating the name of the .Rdata file
  stored.list.file.name = subfolder %/% current.file.name %+% "data"

  # Checking whethere .Rdata file already exists in the subfolder
  if (file.exists(stored.list.file.name)) {
    # If Yes, then load it in a new environment to reduce the name conflicts.
    env.stored = new.env()
    load(stored.list.file.name, envir = env.stored)

    # Printing various messages of this actions
    catn(
      "    Data is already saved in:",
      color = "blue",
      console = console,
      newline = F
    )
    catn(stored.list.file.name, color = "red", console = console)

    # Setting 'compare' to note if data saved in .Rdata file needs to be compared
    compare = TRUE
  } else {
    compare = FALSE
  }

  # Checking
  # 1. Whether a list exists if not create.
  # 2. Check whether we already have the data with the same name in the list l
  if (is.null(names(lt)) || any("tables" != names(lt))) {
    lt$tables = list()
    catn(
      "    Existing list doesn't contain the table!",
      color = "blue",
      console = console
    )
  }

  file.index = str_detect(string = names(lt$tables), pattern = filename)

  if (any(file.index)) {
    # Case when table name exists
    table.names = sort(names(lt$tables)[file.index])
    table.name = table.names[length(table.names)]
    filename.n = prefix %+% table.name

    catn(
      "    Tables of the name",
      color = "blue",
      newline = F,
      console = console
    )
    catn(filename, color = "red", newline = F, console = console)
    catn("' exists!", color = "blue", console = console)
    catn(
      "    Data need to rewritten?",
      color = "blue",
      newline = F,
      console = console
    )

    # Checking whether the data is identical as in the list
    # In case we need to overwrite
    if (!identical(lt$tables[[table.name]][[data.name]], data)) {
      catn(" Yes!", color = "green", console = console)
      write = TRUE
    } else {
      catn(" No!", color = "red", console = console)
      write = FALSE
    }
    # Case when table name doesn't exist!
  } else {
    index = pad.00(length(lt$tables) + 1)
    table.name = index %+% "_" %+% filename %+% "_[R].csv"
    filename.n = prefix %+% table.name
    write = TRUE
  }

  # Case when data needs to be written or overwritten
  if (write) {
    lt1 = paste0(lt.name, "$tables[['", table.name, "']]")

    eval(parse(text = paste0(lt1, "= list()")), envir = envir)
    eval(parse(text = paste0(lt1, " %<% ", data.name)), envir = envir)

    # Case when data needs to be overwritten
    if (compare) {
      data.same = identical(
        env.stored[[lt.name]]$tables[[table.name]][[data.name]],
        data
      )
    } else {
      data.same = FALSE
    }

    # Implementation
    # Case: No need to write!
    if (compare & data.same) {
      catn(
        "    Table data is identical to stored data!",
        color = "blue",
        console = console
      )
      # Case: Data needs to be freshly saved.
    } else {
      catn("    Saving:", color = "blue", newline = F, console = console)
      catn(filename.n, color = "green", console = console)
      write_csv.adv(
        data = data,
        file.name = filename.n,
        fun_family = fun_family,
        ...
      )
      eval(parse(
        text = "save(" %+%
          lt.name %+%
          ", file = '" %+%
          stored.list.file.name %+%
          "', envir = envir)"
      ))
    }
  }
  return(invisible())
}

# plot.colors() -----------------------------------------------------------
# There are more than 650 color names in R, plot.color() shows which color name appears how
# We can even filter out the colors which has some partial string
# usage
# plot.colors()
# plot.colors("dark")
# plot.colors("red)
plot.colors <- function(name = NULL) {
  # Adapted from
  # http://sape.inf.usi.ch/quick-reference/ggplot2/colour
  install("tidyverse")
  c = colors()[!stringr::str_detect(colors(), "grey")]
  if (is.null(name)) {
    c.sub = c
  } else {
    name = stringr::str_to_lower(name)
    if (name == "grey") {
      name = "gray"
      warning("Using American spelling as 'gray'!!")
    }
    c.loc = stringr::str_detect(c, name)
    if (any(c.loc)) {
      c.sub = c[c.loc]
    } else {
      stop("Please put a valid search color name!!")
    }
  }
  n.name = max(
    floor(
      length(c.sub) /
        dplyr::if_else(length(c.sub) > 100, 10, floor(length(c.sub) / 10))
    ),
    1
  )
  d = data.frame(
    c = c.sub,
    y = seq(0, length(c.sub) - 1) %% n.name,
    x = floor(seq(0, length(c.sub) - 1) / n.name)
  )
  ggplot2::ggplot() +
    scale_x_continuous(name = "", breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(name = "", breaks = NULL, expand = c(0, 0)) +
    scale_fill_identity() +
    geom_rect(
      data = d,
      mapping = aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1),
      fill = "white"
    ) +
    geom_rect(
      data = d,
      mapping = aes(
        xmin = x + 0.05,
        xmax = x + 0.95,
        ymin = y + 0.5,
        ymax = y + 1
      ),
      fill = c.sub
    ) +
    geom_text(
      data = d,
      mapping = aes(x = x + 0.5, y = y + 0.5, label = c.sub),
      colour = "black",
      hjust = 0.5,
      vjust = 1,
      size = 3
    ) +
    labs(title = name) +
    theme_minimal()
}


# format_num(3.14, width =2) ####
format_num <- function(
  x,
  int.digits = 4,
  dec.digits = 2,
  big.mark = ",",
  big.interval = 3L,
  small.mark = ",",
  small.interval = 5L,
  ...
) {
  # Round the number to n decimal places
  int_part <- floor(x)
  frac_part <- x - int_part

  if (frac_part == 0) {
    out = prettyNum(
      x,
      width = int.digits,
      big.mark = big.mark,
      big.interval = big.interval,
      small.mark = small.mark,
      small.interval = small.interval,
      ...
    )
  } else {
    int_str <- prettyNum(
      int_part,
      scientific = F,
      width = int.digits,
      big.mark = big.mark,
      big.interval = big.interval,
      small.mark = small.mark,
      small.interval = small.interval,
      ...
    )
    # Convert the rounded number to a character string with n decimal places
    frac_str <- prettyNum(
      round(frac_part, dec.digits),
      digits = dec.digits,
      format = "f",
      nsmall = dec.digits
    ) |>
      strsplit(split = "[.]") |>
      unlist() |>
      (\(x) x[[2]])()

    out = int_str %+% "." %+% frac_str
  }
  # Return the formatted string
  return(out)
}
