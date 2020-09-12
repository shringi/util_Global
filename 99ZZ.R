# Author: Ankur Shringi (ankurshringi@iisc.ac.in)
# title: 99ZZ.R
# subtitle: ...
# abstract: ...
# Project: util_Global
# Date created: 2020-May-05 16:08:32 Tuesday
# Enter following command to render the code as html
# `r2html()`
# Initialization ----------------------------------------------------------
# Loading custom made utility functions
source("util_Global.R")
# Deleting R-Environment Variables (Except utility functions)
clr()
# Loading required packages
Packages <-     c("tidyverse", "roxygen2Comment")
install(Packages); rm(Packages)

# Stat_lm Equation --------------------------------------------------------------------------------------
# .stat_lm <- function(formula, data, output.type = "expression"){
formula = formula
data = df
output.type = "expression"
res.lm <- stats::lm(formula, data)
coefs <- stats::coef(res.lm)

formula.rhs.chr <- as.character(formula)[3]
if (grepl("-1", formula.rhs.chr) || grepl("- 1", formula.rhs.chr)) {
  coefs <- c(0, coefs)
}

rr <- summary(res.lm)$r.squared %>% signif(2)
adj.rr <- summary(res.lm)$adj.r.squared %>% signif(2)
AIC <- stats::AIC(res.lm) %>% signif(2)
BIC <- stats::BIC(res.lm) %>% signif(2)

# Build model equation
eq.char.tmp <- as.character(signif(polynom::as.polynomial(coefs), 3))
eq.char.tmp2 <- gsub("e([+-]?[0-9]*)", "%*%10^\\1", eq.char.tmp)
if (output.type %in% c("latex", "tex", "tikz")) {
  eq.char.tmp2 <- gsub("*", " ", eq.char, fixed = TRUE)
}
# Add y
if (output.type == "expression") {
  lhs <- "italic(y)~`=`~"
} else if (output.type %in% c("latex", "tex", "tikz", "text")) {
  lhs <- "y = "
}
eq.char <- paste(lhs, eq.char.tmp2, sep = "")

# Build data frame with the output
if (output.type == "expression") {
  eq.x.rhs = "~italic(x)"
} else {
  eq.x.rhs = " x"
}

if (output.type == "expression") {
  z <- data.frame(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                  rr.label = paste("italic(R)^2", rr, sep = "~`=`~"),
                  adj.rr.label = paste("italic(R)[adj]^2",
                                       adj.rr, sep = "~`=`~"),
                  AIC.label = paste("AIC", AIC, sep = "~`=`~"),
                  BIC.label = paste("BIC", BIC, sep = "~`=`~"))
} else if (output.type %in% c("latex", "tex", "text")) {
  z <- data.frame(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                  rr.label = paste("R^2", rr, sep = " = "),
                  adj.rr.label = paste("R_{adj}^2",adj.rr, sep = " = "),
                  AIC.label = paste("AIC", AIC, sep = " = "),
                  BIC.label = paste("BIC", BIC, sep = " = "))
}

z <- z %>%
  mutate(rr = rr, adj.rr = adj.rr, AIC = AIC, BIC = BIC) %>%
  dplyr::select(rr, adj.rr, AIC, BIC, everything())

z
# }



# Backstitch --------------------------------------------------------------------------------------------
warning("You probably don't want to use this `backstitch()` function.",
        "\n  It's hacky and there's a much better option in knitr called `purl()`.",
        "\n  More info at: `?knitr::purl`")

#' Backstitch an Rmd file to an R script
#'
#' Takes an Rmd file -- that would be converted with knitr::knit() -- and
#' "backstitches" it into an R script suitable for knitr::purl(). The output
#' file is the just the backstitched R script when `output_type = 'script'`, or
#' just the code chunks when `output_type = 'code'` (note that all inline code
#' is dropped in this case). Or finally, output both with
#' `output_type = 'both'`.
#'
#' You can load this function by calling:
#' devtools::source_gist('284671997992aefe295bed34bb53fde6', filename = 'backstitch.R')
#'
#' @param infile Input file name
#' @param outfile Output file name (`.R` extension added if not included)
#' @param ouput_type One of `script`, `code` or `both`. If `both`, two files are
#'   created, with `_code` appended to the file name of the code chunks.
#' @param chunk_header Chunk header style, valid options are `"#-"`, `"#+"`, and `"# ----"`
backstitch <- function(
  infile,
  outfile = NULL,
  output_type = c('both'),
  chunk_header = "# ----"
) {
  requireNamespace('knitr', quietly = TRUE)
  requireNamespace('stringr', quietly = TRUE)
  stopifnot(output_type %in% c('script', 'code', 'both'))

  if (is.null(outfile) && output_type == 'both')
    stop("Please choose output_type of 'script' or 'code' when not outputting to a file.")

  knitr::knit_patterns$set(knitr::all_patterns[['md']])

  x <- readLines(infile)
  if (inherits(infile, 'connection')) close(infile)

  empty_lines <- which(stringr::str_detect(x, "^\\s?+$"))
  last_non_empty_line <- max(setdiff(seq_along(x), empty_lines))
  x <- x[1:last_non_empty_line]

  x_type <- rep('text', length(x))

  # Find YAML section
  yaml_markers <- which(stringr::str_detect(x, "^[-.]{3}\\s*$"))
  if (length(yaml_markers) > 2) {
    message("Input file may have multiple YAML chunks, only considering lines",
            paste(yaml_markers[1:2], collapse = '-'), 'as YAML header.')
  }
  if (length(yaml_markers) > 0) {
    i.yaml <- yaml_markers[1]:yaml_markers[2]
    x_type[i.yaml] <- 'yaml'
  }

  # Mark code chunk.begin, chunk.end and regular chunk codelines
  i.chunk.begin <- which(stringr::str_detect(x, knitr::knit_patterns$get('chunk.begin')))
  i.chunk.end   <- which(stringr::str_detect(x, knitr::knit_patterns$get('chunk.end')))
  x_type[i.chunk.end] <- 'chunk.end'
  for (j in i.chunk.begin) {
    j.chunk.end <- min(i.chunk.end[i.chunk.end > j]) - 1
    x_type[j:j.chunk.end] <- 'chunk'
  }
  x_type[i.chunk.begin] <- 'chunk.begin'

  # Check for inline code
  i.inline <- which(stringr::str_detect(x, knitr::knit_patterns$get('inline.code')))
  i.inline <- intersect(i.inline, which(x_type == 'text'))
  x_type[i.inline] <- 'inline'

  # Check empty lines
  i.empty <- which(stringr::str_detect(x, "^\\s*$"))
  i.empty <- intersect(i.empty, which(x_type == 'text'))
  x_type[i.empty] <- 'empty'

  really_empty <- function(x_type, j, n = -1) {
    if (grepl('(chunk|yaml)', x_type[j + n])) {
      return('empty')
    } else if (n < 0) {
      return(really_empty(x_type, j, 1))
    } else if (x_type[j + n] %in% c('text', 'inline')) {
      return('text')
    } else {
      return(really_empty(x_type, j, n + 1))
    }
  }

  for (j in i.empty) {
    x_type[j] <- really_empty(x_type, j)
  }

  # Rewrite lines helper functions
  comment <- function(x) paste("#'", x)
  make_chunk_header <- function(x, chunk_header) {
    stringr::str_replace(stringr::str_replace(x, knitr::knit_patterns$get('chunk.begin'), "\\1"),
                         "^r[, ]?", paste(chunk_header, ""))
  }
  # Rewrite lines
  y <- x
  regex_inline_grouped <- "`r[ ]?#?(([^`]+)\\s*)`"
  i.empty       <- which(x_type == 'empty')
  i.text        <- which(x_type == 'text')
  y[i.chunk.begin] <- make_chunk_header(x[i.chunk.begin], chunk_header)
  y[i.inline]      <- comment(stringr::str_replace_all(x[i.inline], regex_inline_grouped, "{{\\1}}"))
  y[i.text]        <- comment(x[i.text])
  if (length(yaml_markers) > 0) y[i.yaml] <- comment(x[i.yaml])
  y[i.empty]       <- ""
  y[i.chunk.end]   <- ""

  y_code <- y[which(stringr::str_detect(x_type, 'chunk'))]

  if (!is.null(outfile)) {
    outfile_name <- stringr::str_replace(outfile, "(.+)\\.R$", "\\1")
    if (output_type == "script") {
      cat(c(y, ""), file = paste0(outfile_name, ".R"), sep = '\n')
    } else if (output_type == "code") {
      cat(c(y_code, ""), file = paste0(outfile_name, ".R"), sep = '\n')
    } else {
      cat(c(y, ""), file = paste0(outfile_name, ".R"), sep = '\n')
      cat(c(y_code, ""), file = paste0(outfile_name, "_code.R"), sep = '\n')
    }
  } else {
    switch(
      output_type,
      'script' = unname(y),
      'code' = unname(y_code)
    )
  }
}


# lm2eq -------------------------------------------------------------------------------------------------
install("tidyverse")
model = res.lm
summary(model)
broom::tidy(model)
summary(model)$coefficients[,1] %>% round(2)
cbn = function(Estimate, low, c2, ...){
  paste0(a,"[bgroup('(', atop(", c1,"," ,c2,"),')')]")
}

model.stats <- function(model){
  t = format(terms(model))
  ty = strsplit(t, split = "~")[[1]][1]
  tx = strsplit(t, split = "~")[[1]][2]
  terms = strsplit(tx, split = "\\+")[[1]] %>% trimws()
  pos_wi = terms %>% map_lgl(grepl,pattern = " * ")
  terms_wi = terms[!pos_wi]
  terms_in = terms[pos_wi] %>% strsplit(.,split = "\\*") %>% unlist() %>% trimws() %>% unique()
  coef_names = names(coefficients(model)[-1])
  terms_un = union(terms_wi, terms_in)
  coef_class = model$model %>% select(names(.)) %>% select(-1) %>% map_chr(class)
  factors = coef_class[coef_class == "factor"] %>% names()
  coefs = summary(model)$coefficients
  cc = data.frame(signif(coefs, 3), signif(confint(res.lm),3), var = c("", coef_names)) %>%
    rename(P.value = Pr...t..,
           low = X2.5..,
           high = X97.5..) %>%
    mutate(stars = stars.pval(P.value))
  # ~a[scriptscriptstyle(bgroup("(",atop(scriptscriptstyle(a.high),a.low),")"))]^{a.pval} ~~ b[bgroup("(",atop(b.high,b.low),")")]^{b.pval} %.% bolditalic(X)*","~~textstyle(bgroup("(",atop(r[adj]^{2}~"="~adj.r2, r^2~"="~r2),")")),
  list(a = Estimate,
       b = format(coef(m)[[2]], digits = 2) %>% as.numeric() %>% with_sign(),
       a.pval = stars.pval(summary(m)$coefficients[1,4])[1],
       b.pval = stars.pval(summary(m)$coefficients[2,4])[1],
       adj.r2 = format(summary(m)$adj.r.squared, digits = 3),
       r2 = format(summary(m)$r.squared, digits = 3),
       a.low = format(confint(m)[1,1], digits = 2) %>% as.numeric() %>% with_sign(unicode = TRUE),
       a.high = format(confint(m)[1,2], digits = 2) %>% as.numeric() %>% with_sign(unicode = TRUE),
       b.low = format(confint(m)[2,1], digits = 2) %>% as.numeric() %>% with_sign(unicode = TRUE),
       b.high = format(confint(m)[2,2], digits = 2) %>% as.numeric() %>% with_sign(unicode = TRUE))
}
text_lhs = c(signif(coefs[1],3), paste0(as.character(signif(coefs[-1],3)),"*",coef_names))
eq.m <- paste0(ty, "== ", , " ", text_lhs) %>% gsub("[I(|)]", "", .)
eq.ci <- paste("bgroup('(', atop(", cf1, "),')')", collapse = "+")
as.character(as.expression(eq))
cf = signif(confint(res.lm),3) %>% as.data.frame()
cf1 <- map_chr(transpose(cf), paste, collapse = ",")
tmp <- paste("bgroup('(', atop(", cf1, "),')')", collapse = "+")
# tmp <- paste0(cc1[1,1],"[bgroup('(', atop(", cc1[1,3],"," ,cc1[1,4],"),')')]")
tmp <- cbn(1.1,1.2,1.3)
#plot
qplot(c(0,10),c(0,10)) + annotate("text", x = 5, y = 5, label = as.character(as.expression(tmp)), parse = TRUE)

# stat_smooth -------------------------------------------------------------------------------------------
install("gtools")
stat_smooth_func <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,

                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))

                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }

                            params
                          },

                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }

                            if (is.null(data$weight)) data$weight <- 1

                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }

                            if (is.character(method)) method <- match.fun(method)

                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))

                            m = model
                            # eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                            #                  list(a = format(coef(m)[1], digits = 3),
                            #                       b = format(coef(m)[2], digits = 3),
                            #                       r2 = format(summary(m)$r.squared, digits = 3)))
                            eq <- lm2eq(m)
                            func_string = as.character(as.expression(eq))

                            if(is.null(xpos)) xpos = min(data$x)*0.9
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x = xpos, y = ypos, label = func_string)

                          },

                          required_aes = c("x", "y")
)


with_sign <- function(x, unicode = FALSE, ...){
  if (unicode) {
    if (x >= 0)
    {paste0("\UFF0B ",sprintf(fmt = "%s", format(x, ...)))
    }
    else
    {paste0("\UFF0D ",sprintf(fmt = "%s", format(abs(x), ...)))
    }
  } else {
    if (x >= 0)
    {sprintf(fmt = "+ %s", format(x, ...))
    }
    else
    {sprintf(fmt = "- %s", format(abs(x), ...))
    }
  }

}
lm2eq <- function(m){
  substitute(bolditalic(Y) == ~a[scriptscriptstyle(bgroup("(",atop(scriptscriptstyle(a.high),a.low),")"))]^{a.pval} ~~ b[bgroup("(",atop(b.high,b.low),")")]^{b.pval} %.% bolditalic(X)*","~~textstyle(bgroup("(",atop(r[adj]^{2}~"="~adj.r2, r^2~"="~r2),")")),
             list(a = format(coef(m)[[1]], digits = 2) ,
                  b = format(coef(m)[[2]], digits = 2) %>% as.numeric() %>% with_sign(),
                  a.pval = stars.pval(summary(m)$coefficients[1,4])[1],
                  b.pval = stars.pval(summary(m)$coefficients[2,4])[1],
                  adj.r2 = format(summary(m)$adj.r.squared, digits = 3),
                  r2 = format(summary(m)$r.squared, digits = 3),
                  a.low = format(confint(m)[1,1], digits = 2) %>% as.numeric() %>% with_sign(unicode = TRUE),
                  a.high = format(confint(m)[1,2], digits = 2) %>% as.numeric() %>% with_sign(unicode = TRUE),
                  b.low = format(confint(m)[2,1], digits = 2) %>% as.numeric() %>% with_sign(unicode = TRUE),
                  b.high = format(confint(m)[2,2], digits = 2) %>% as.numeric() %>% with_sign(unicode = TRUE)))
}
# test_eq -----------------------------------------------------------------------------------------------
# https://gist.github.com/kdauria/524eade46135f6348140
stat_smooth_func <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,

                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))

                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }

                            params
                          },

                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }

                            if (is.null(data$weight)) data$weight <- 1

                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }

                            if (is.character(method)) method <- match.fun(method)

                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))

                            m = model
                            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                                             list(a = format(coef(m)[1], digits = 3),
                                                  b = format(coef(m)[2], digits = 3),
                                                  r2 = format(summary(m)$r.squared, digits = 3)))
                            func_string = as.character(as.expression(eq))

                            if (is.null(xpos)) xpos = min(data$x)*0.9
                            if (is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x = xpos, y = ypos, label = func_string)

                          },

                          required_aes = c("x", "y")
)
# stat_smooth -------------------------------------------------------------------------------------------
install("tidyverse")
source("stat_smooth.R")
df = data.frame(n = c(1:100))
df$ba = -1.01 - 2.02 * df$n + 3.03 * df$n*df$n #+ rnorm(100, sd = 4)
df$class = rep(1:2,50)
formula <- ba ~ n + I(n^2)
ggplot(data = df, aes(x = n, y = ba, label=ba)) +
  stat_smooth_func(geom ="text", method = "lm", formula = formula, hjust = 0,parse = TRUE) +
  geom_smooth(method = "lm", level=.95) +
  geom_point()# + facet_wrap(~class)

# stat_regline_equation ---------------------------------------------------------------------------------
# devtools::install_github("kassambara/ggpubr")
library("ggpubr")

# stat_smooth(aes(fill = group, color = group), method = "lm", formula = formula) +

p = ggplot(data = df, aes(x = n, y = ba, label = ba)) +
  stat_smooth(method = "lm", formula = formula) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula) + geom_point()
ggpar(p, palette = "jco")



# ggpmisc -----------------------------------------------------------------------------------------------
install("ggpmisc")
formula <- y ~ x + I(x^2)
ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_fit_deviations(method = "lm", formula = formula, colour = "red") +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
               formula = formula, parse = TRUE)

xx = runif(n = 100, min = 0, max = 3)
df = data.frame(x = xx, y = 0.01*exp(xx) + rnorm(100, 0.01*exp(xx), 0.01), z = rep(c("A","B","C","D")))
#df1 = data.frame(x = xx, y = exp(0.05*xx), z = rep(c("A","B","C","D")))
ggplot(df, aes(x = x, y = y)) + geom_point() + geom_smooth(method = "lm", formula = y~exp(x) )

mod1 <- lm(log(y)~ x, data = df)
df$fitted1 = exp(fitted(mod1))
mod2 <- lm(y ~ exp(x), data = df)
df$fitted2 = fitted(mod2)

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(x = x, y =  fitted1, col = "1")) +
  geom_line(aes(x = x, y =  fitted2, col = "2"))

dataset <- data.frame(Exp = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6), t = c(0, 0.33, 0.67, 1, 1.33, 1.67, 2, 4, 6, 8, 10, 0, 33, 0.67, 1, 1.33, 1.67, 2, 4, 6, 8, 10, 0, 0.33, 0.67, 1, 1.33, 1.67, 2, 4, 6, 8, 10), fold = c(1, 0.957066345654286, 1.24139015724819, 1.62889151698633, 1.72008539595879, 1.82725412314402, 1.93164365299958, 1.9722929538061, 2.15842019312484, 1.9200507796933, 1.95804730344453, 1, 0.836176542548747, 1.07077717914707, 1.45471712491441, 1.61069357875771, 1.75576377806756, 1.89280913889538, 2.00219054189937, 1.87795513639311, 1.85242493827193, 1.7409346372629, 1, 0.840498729335292, 0.904130905000499, 1.23116185602517, 1.41897551928886, 1.60167656534099, 1.72389226836308, 1.80635095956481, 1.76640786872057, 1.74327897001172, 1.63581509884482))
test <- subset(dataset,Exp == 4)
fit1 = nls(fold ~ 1+(Vmax*(1-exp(-t/tau))),
           data = test,
           start = c(tau = 0.2, Vmax = 2))
ggplot(test,aes(t, fold)) +
  stat_function(fun = function(t){1.01 + coef(fit1)[[2]]*(1-exp(-t/coef(fit1)[[1]]))})+
  geom_point() +
  geom_smooth(method = "nls",
              formula = y~1+Vmax*(1-exp(-x/tau)), # this is an nls argument
              method.args = list(start = c(tau = 0.2, Vmax = 2)), # this too
              se = FALSE)

# Automatic equation extraction -------------------------------------------------------------------------
install("tidyverse")
df <- data.frame(x = rnorm(100), y = rnorm(100), va = rep(c("A","B"),50)) %>%
  mutate(z = if_else(va == "A",
                     -1.01 - 1.05*x - 2.1*y - 3.13*y^2 - 8.99 - 3*x*y,
                     -1.01 - 1.15*x - 2.3*y - 3.13*y^2 + 12.01 - 4*x*y))

ggplot(df, aes(x, z, color = y)) + geom_point() + facet_wrap(~va)

equation <- z ~ x + y + x*y + x*va + y*va + x*va*y + I(y^2)
model <- lm(equation, data = df)
model
summary(model)
broom::tidy(model)
summary(model)$coefficients[,1] %>% round(2)

t = format(terms(model))
ty = strsplit(t, split = "~")[[1]][1]
tx = strsplit(t, split = "~")[[1]][2]
terms = strsplit(tx, split = "\\+")[[1]] %>% trimws()
pos_wi = terms %>% map_lgl(grepl,pattern = " * ")
terms_wi = terms[!pos_wi]
terms_in = terms[pos_wi] %>% strsplit(.,split = "\\*") %>% unlist() %>% trimws() %>% unique()
terms_un = union(terms_wi, terms_in)
coef_names = names(coefficients(model)[-1])
coef_class = model$model %>% select(names(.)) %>% select(-1) %>% map_chr(class)
factors = coef_class[coef_class == "factor"] %>% names()


paste0(coef(model),"\U22C5",terms, collapse = " + ")

paste0(ty, "~ ", round(coefficients(model)[1],2), "",
       paste(sprintf(" %+.2f\U22C5%s ",
                     coefficients(model)[-1],
                     names(coefficients(model)[-1])),
             collapse = ""))



# test_stat_poly_eq.R -----------------------------------------------------------------------------------
#' Add a curve from a fitted linear model and a label to a plot.
#'
#' \code{stat_poly_eq} fits a polynomial and generates a label with
#'   an equation and/or coefficient of determination (R^2).
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param formula a formula object
#' @param eq.with.lhs If \code{character} the string is pasted to the front
#'   of the equation label before parsing or a \code{logical} (see note).
#' @param eq.x.rhs \code{character} this string will be used as replacement
#'   for \code{"x"} in the model equation when generating the label before
#'   parsing it.
#' @param label.x,label.y \code{numeric} Coordinates to be used in output. If
#'   too short they will be recycled.
#'
#' @note For backward compatibility a logical is accepted as argument for
#'   \code{eq.with.lhs}, giving the same output than the current default
#'   character value. By default "x" is retained as independent variable as
#'   this is the name of the aesthetic. However, it can be substituted by
#'   providing a suitable replacement character string through \code{eq.x.rhs}.
#'
#' @details This stat can be used to automatically annotate a plot with R^2,
#' adjusted R^2 or the fitted model equation. It supports only linear models
#' fitted with function \code{lm()}. The R^2 and adjusted R^2 annotations can be
#' used with any linear model formula. The fitted equation label is correclty
#' generated for polynomials or quasi-polynomials through the origin. Model
#' formulas can use \code{poly()} or be defined algebraically with terms of
#' powers of increasing magnitude with no missing intermediate terms, except
#' possibly for the intercept indicated by "- 1" or "-1" in the formula. The
#' validity of the \code{formula} is not checked in the current implementation,
#' and for this reason the default aesthetics sets R^2 as label for the
#' annotation. This stat only generates the label, the predicted values need
#' to be sepearately added to the plot, so to make sure that the same model
#' formula is used in all steps it is best to save the formula as an object
#' and supply this object as argument to the different statistics.
#'
#' @section Computed variables:
#'   \describe{ \item{x}{x position for left edge}
#'   \item{y}{y position near upper edge}
#'   \item{eq.label}{equation for the
#'   fitted polynomial as a character string to be parsed}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{adj.rr.label}{Adjusted \eqn{R^2} of the fitted model as a character string
#'   to be parsed}
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{BIC.label}{BIC for the fitted model.}
#'   \item{hjust}{Set to zero to override the default of the "text" geom.}}
#'
#' @examples
#' library(ggplot2)
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y, group = c("A", "B"), y2 = y * c(0.5,2))
#' # give a name to a formula
#' formula <- y ~ poly(x, 3, raw = TRUE)
#' # plot
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, parse = TRUE)
#'
#' @export
#'
stat_poly_eq <- function(mapping = NULL, data = NULL, geom = "text",
                         formula = NULL,
                         eq.with.lhs = "italic(y)~`=`~",
                         eq.x.rhs = "~italic(x)",
                         label.x = NULL, label.y = NULL,
                         position = "identity",
                         na.rm = FALSE, show.legend = FALSE,
                         inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPolyEq, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(formula = formula,
                  eq.with.lhs = eq.with.lhs,
                  eq.x.rhs = eq.x.rhs,
                  label.x = label.x,
                  label.y = label.y,
                  na.rm = na.rm,
                  ...)
  )
}

# Define here to avoid a note in check as the import from 'polynom' is not seen
# when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
poly_eq_compute_group_fun <- function(data,
                                      scales,
                                      formula,
                                      eq.with.lhs,
                                      eq.x.rhs,
                                      label.x,
                                      label.y) {
  mf <- stats::lm(formula, data)
  coefs <- stats::coef(mf)
  formula.rhs.chr <- as.character(formula)[3]
  if (grepl("-1", formula.rhs.chr) || grepl("- 1", formula.rhs.chr)) {
    coefs <- c(0, coefs)
  }
  rr <- summary(mf)$r.squared
  AIC <- AIC(mf)
  BIC <- BIC(mf)
  adj.rr <- summary(mf)$adj.r.squared
  eq.char <- as.character(signif(polynom::as.polynomial(coefs), 3))
  eq.char <- gsub("e([+-]?[0-9]*)", "%*%10^\\1", eq.char)
  if (is.character(eq.with.lhs)) {
    lhs <- eq.with.lhs
    eq.with.lhs <- TRUE
  } else if (eq.with.lhs) {
    lhs <- "italic(y)~`=`~"
  }
  if (eq.with.lhs) {
    eq.char <- paste(lhs, eq.char, sep = "")
  }
  rr.char <- format(rr, digits = 2)
  adj.rr.char <- format(adj.rr, digits = 2)
  AIC.char <- sprintf("%.4g", AIC)
  BIC.char <- sprintf("%.4g", BIC)
  data.frame(x = ifelse(is.null(label.x),
                        min(data$x),
                        label.x),
             y = ifelse(is.null(label.y),
                        max(data$y) - 0.1 * diff(range(data$y)),
                        label.y),
             eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
             rr.label = paste("italic(R)^2", rr.char, sep = "~`=`~"),
             adj.rr.label = paste("italic(R)[adj]^2",
                                  adj.rr.char, sep = "~`=`~"),
             AIC.label = paste("AIC", AIC.char, sep = "~`=`~"),
             BIC.label = paste("BIC", BIC.char, sep = "~`=`~"),
             hjust = 0)
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPolyEq <-
  ggplot2::ggproto("StatPolyEq", ggplot2::Stat,
                   compute_group = poly_eq_compute_group_fun,
                   default_aes =
                     ggplot2::aes(label = ..rr.label.., hjust = ..hjust..),
                   required_aes = c("x", "y")
  )




# stat_regline_equation ---------------------------------------------------------------------------------
#' @include utilities.R utilities_label.R
#' @importFrom dplyr everything
#' @importFrom dplyr select
NULL
#'Add Regression Line Equation and R-Square to a GGPLOT.
#'@description Add regression line equation and R^2 to a ggplot. Regression
#'  model is fitted using the function \code{\link[stats]{lm}}.
#'@inheritParams ggplot2::layer
#'@param formula a formula object
#'@param label.x.npc,label.y.npc can be \code{numeric} or \code{character}
#'  vector of the same length as the number of groups and/or panels. If too
#'  short they will be recycled. \itemize{ \item If \code{numeric}, value should
#'  be between 0 and 1. Coordinates to be used for positioning the label,
#'  expressed in "normalized parent coordinates". \item If \code{character},
#'  allowed values include: i) one of c('right', 'left', 'center', 'centre',
#'  'middle') for x-axis; ii) and one of c( 'bottom', 'top', 'center', 'centre',
#'  'middle') for y-axis.}
#'
#'  If too short they will be recycled.
#'@param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'  for absolute positioning of the label. If too short they will be recycled.
#'@param output.type character One of "expression", "latex" or "text".
#'@param ... other arguments to pass to \code{\link[ggplot2]{geom_text}} or
#'  \code{\link[ggplot2]{geom_label}}.
#'@param na.rm If FALSE (the default), removes missing values with a warning. If
#'  TRUE silently removes missing values.
#'@seealso \code{\link{ggscatter}}
#'@references the source code of the function \code{stat_regline_equation()} is
#'  inspired from the code of the function \code{stat_poly_eq()} (in ggpmisc
#'  package).
#'
#' @section Computed variables:
#'   \describe{ \item{x}{x position for left edge}
#'   \item{y}{y position near upper edge}
#'   \item{eq.label}{equation for the
#'   fitted polynomial as a character string to be parsed}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{adj.rr.label}{Adjusted \eqn{R^2} of the fitted model as a character string
#'   to be parsed}
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{BIC.label}{BIC for the fitted model.}
#'   \item{hjust}{Set to zero to override the default of the "text" geom.}}
#' @examples
#'
#' # Simple scatter plot with correlation coefficient and
#' # regression line
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::
#' ggscatter(mtcars, x = "wt", y = "mpg", add = "reg.line") +
#'   stat_cor(label.x = 3, label.y = 34) +
#'   stat_regline_equation(label.x = 3, label.y = 32)
#'
#'
#' # Groupped scatter plot
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::
#' ggscatter(
#'   iris, x = "Sepal.Length", y = "Sepal.Width",
#'   color = "Species", palette = "jco",
#'   add = "reg.line"
#'   ) +
#'   facet_wrap(~Species) +
#'   stat_cor(label.y = 4.4) +
#'   stat_regline_equation(label.y = 4.2)
#'
#' # Polynomial equation
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::
#'
#' # Demo data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y, group = c("A", "B"),
#'                       y2 = y * c(0.5,2), block = c("a", "a", "b", "b"))
#'
#' # Fit polynomial regression line and add labels
#' formula <- y ~ poly(x, 3, raw = TRUE)
#' p <- ggplot(my.data, aes(x, y2, color = group)) +
#'   geom_point() +
#'   stat_smooth(aes(fill = group, color = group), method = "lm", formula = formula) +
#'   stat_regline_equation(
#'     aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#'     formula = formula
#'   ) +
#'   theme_bw()
#' ggpar(p, palette = "jco")
#'
#'@export
stat_regline_equation <- function (
  mapping = NULL, data = NULL, formula = y~x,
  label.x.npc = "left", label.y.npc = "top",
  label.x = NULL, label.y = NULL, output.type = "expression",
  geom = "text", position = "identity",  na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...
)
{

  parse <- ifelse(output.type == "expression", TRUE, FALSE)

  layer(
    stat = StatReglineEquation, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(formula = formula, label.x.npc  = label.x.npc , label.y.npc  = label.y.npc,
                  label.x = label.x, label.y = label.y,
                  output.type = output.type, parse = parse, na.rm = na.rm, ...)
  )
}


StatReglineEquation<- ggproto("StatReglineEquation", Stat,
                              required_aes = c("x", "y"),
                              default_aes = aes(label = ..eq.label.., hjust = ..hjust.., vjust = ..vjust..),

                              compute_group = function(data, scales, formula, label.x.npc, label.y.npc,
                                                       label.x, label.y, output.type)
                              {

                                force(data)

                                if (length(unique(data$x)) < 2) {
                                  return(data.frame()) # Not enough data to perform test
                                }

                                .test <- .stat_lm(formula, data, output.type = output.type)
                                # Returns a data frame with label: x, y, hjust, vjust
                                .label.pms <- .label_params(data = data, scales = scales,
                                                            label.x.npc = label.x.npc, label.y.npc = label.y.npc,
                                                            label.x = label.x, label.y = label.y ) %>%
                                  mutate(hjust = 0)
                                cbind(.test, .label.pms)
                              }
)



# Compute regression line equation
.stat_lm <- function(formula, data, output.type = "expression"){

  res.lm <- stats::lm(formula, data)
  coefs <- stats::coef(res.lm)

  formula.rhs.chr <- as.character(formula)[3]
  if (grepl("-1", formula.rhs.chr) || grepl("- 1", formula.rhs.chr)) {
    coefs <- c(0, coefs)
  }

  rr <- summary(res.lm)$r.squared %>% signif(2)
  adj.rr <- summary(res.lm)$adj.r.squared %>% signif(2)
  AIC <- stats::AIC(res.lm) %>% signif(2)
  BIC <- stats::BIC(res.lm) %>% signif(2)

  # Build model equation
  eq.char <- as.character(signif(polynom::as.polynomial(coefs), 2))
  eq.char <- gsub("e([+-]?[0-9]*)", "%*%10^\\1", eq.char)
  if (output.type %in% c("latex", "tex", "tikz")) {
    eq.char <- gsub("*", " ", eq.char, fixed = TRUE)
  }
  # Add y
  if (output.type == "expression") {
    lhs <- "italic(y)~`=`~"
  } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
    lhs <- "y = "
  }
  eq.char <- paste(lhs, eq.char, sep = "")

  # Build data frame with the output
  if (output.type == "expression") {
    eq.x.rhs = "~italic(x)"
  } else {
    eq.x.rhs = " x"
  }

  if (output.type == "expression") {
    z <- data.frame(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                    rr.label = paste("italic(R)^2", rr, sep = "~`=`~"),
                    adj.rr.label = paste("italic(R)[adj]^2",
                                         adj.rr, sep = "~`=`~"),
                    AIC.label = paste("AIC", AIC, sep = "~`=`~"),
                    BIC.label = paste("BIC", BIC, sep = "~`=`~"))
  } else if (output.type %in% c("latex", "tex", "text")) {
    z <- data.frame(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                    rr.label = paste("R^2", rr, sep = " = "),
                    adj.rr.label = paste("R_{adj}^2",adj.rr, sep = " = "),
                    AIC.label = paste("AIC", AIC, sep = " = "),
                    BIC.label = paste("BIC", BIC, sep = " = "))
  }

  z <- z %>%
    mutate(rr = rr, adj.rr = adj.rr, AIC = AIC, BIC = BIC) %>%
    dplyr::select(rr, adj.rr, AIC, BIC, everything())

  z
}


# pptx --------------------------------------------------------------------------------------------------
library(officer)
library(rvg)
library(magrittr)
library(partykit)
tree_model <- ctree(data = iris, Species~.)
read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = plot(tree_model), type = "body") %>%
  print("test.pptx")

fileout <- ("test.pptx")
doc <- read_pptx()
doc <- add_slide(doc)

if (require("ggplot2")) {
  doc <- add_slide(doc)
  gg_plot <- ggplot(data = iris ) +
    geom_point(mapping = aes(Sepal.Length, Petal.Length),
               size = 3) +
    theme_minimal() +
    labs(x = expression("Above Ground Biomass " ~ bgroup("(",frac(Mg, ha),")")))
  doc <- ph_with(x = doc, value = gg_plot,
                 location = ph_location_fullsize(),
                 bg = "transparent" )
  doc <- ph_with(x = doc, value = "graphic title",
                 location = ph_location_type(type="title") )
}
print(doc, target = fileout )

temp.pptx <- function(figObj, figname){
  install(c("officer"))
  subfolder = "Output-Graphics"
  path = subfolder %/% figname %+% "[R]" %+% ".pptx"
  if (!file.exists(path)) {
    out <- read_pptx()
    print(out, target = path)
    out <- read_pptx(path)
  } else {
    out <- read_pptx(path)

  }
  out %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = figObj, location = ph_location_type(type = "body"),
            bg = "transparent" ) %>%
    ph_with(value = ".", location = ph_location_type(type = "title")) %>%
    ph_with(value = paste0(format(Sys.time(), format = "%Y-%b-%d %H:%M:%S "), weekdays(as.Date(Sys.Date(), '%d-%m-%Y'))), location = ph_location_type(type = "dt")) %>%
    ph_with(value = figname, location = ph_location_type(type = "ftr")) %>%
    ph_hyperlink(ph_label = slide_summary(.) %>% dplyr::filter(type == "ftr") %>% dplyr::select(ph_label), type = "ftr", href = figname %+% "[R]" %+% ".pdf") %>%
    print(target = path)
}
temp.pptx(gg_plot,"test")

# Stat_summary_trial_1 ----------------------------------------------------------------------------------

out_w <- stat.summary(colTable = table, group_var = "Species", df_in = iris, format = "wide")
out_l <- stat.summary(colTable = table, group_var = "Species", df_in = iris, format = "long")
out_list <- stat.summary(colTable = table, group_var = "Species", df_in = iris, format = "list")
a <- iris[1:5,]
b <- iris[6:10,]
names(iris)[1:4]
