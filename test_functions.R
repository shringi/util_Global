# store.figure() -----------------------------------------------------------
# This is custom figure saving (with sequential file naming) only if the changes exists.
# Usage:
# l = list()
# df = data.frame(x = 1 , y = 1)
# store.figure(filename = "Test", data = df, lt = l, console = T)
# store.figure(filename = "Test", data = df, lt = l, console = T)
# str.list(l)
# df = data.frame(x = 2 , y = 1)
# store.figure(filename = "Test", data = df, lt = l, console = T)
# str.list(l)
# df = data.frame(x = 2 , y = 2)
# store.figure(filename = "Test-New", data = df, lt = l, console = T)
# str.list(l)

store.figure <- function(filename, data, lt, check = T,
                        subfolder = "04-Graphics",
                        console = FALSE,
                        fun_family = "csv",
                        envir = rlang::caller_env(),
                        ...) {
  install("rlang")

  # Extracting names of the data and the list
  data.name = rlang::enexpr(data)
  lt.name = deparse(substitute(lt))

  # Extracting source file name
  current.file.name = get.source.file.name()

  # Extracting the file prefix if exists
  prefix = get.file.prefix(current.file.name)

  # Generating the name of the .Rdata file
  stored.list.file.name = subfolder %/% current.file.name %+%
    "data"

  # Checking whethere .Rdata file already exists in the subfolder
  if (file.exists(stored.list.file.name)) {
    # If Yes, then load it in a new environment to reduce the name conflicts.
    env.stored = new.env()
    load(stored.list.file.name, envir = env.stored)

    # Printing various messages of this actions
    catn("    Data is already saved in:",
         color = "blue", console = console, newline = F)
    catn(stored.list.file.name, color = "red", console = console)

    # Setting 'compare' to note if data saved in .Rdata file needs to be compared
    compare = TRUE
  }  else {
    compare = FALSE
  }

  # Checking
  # 1. Whether a list exists if not create.
  # 2. Check whether we already have the data with the same name in the list l
  if (is.null(names(lt)) || any("figures" != names(lt))) {
    lt$figures = list()
    catn("    Existing list doesn't contain the figure!",
         color = "blue", console = console)
  }

  file.index = filename == names(lt$figures)

  if (any(file.index)) {
    # Case when figure name exists
    figure.name = names(lt$figures)[file.index]
    filename.n = prefix %+% figure.name

    catn("    figures of the name",
         color = "blue", newline = F, console = console)
    catn(filename, color = "red", newline = F, console = console)
    catn("' exists!", color = "blue", console = console)
    catn("    Data need to rewritten?",
         color = "blue", newline = F, console = console)

    # Checking whether the data is identical as in the list
    # In case we need to overwrite
    if (!identical(lt$figures[[figure.name]][[data.name]], data)) {
      catn(" Yes!", color = "green", console = console)
      write = TRUE
    } else {
      catn(" No!", color = "red", console = console)
      write = FALSE
    }
    # Case when figure name doesn't exist!
  } else {
    index = pad.00(length(lt$figures) + 1)
    figure.name = index %+% "_" %+% filename %+% "_[R].csv"
    filename.n = prefix %+% figure.name
    write = TRUE
  }

  # Case when data needs to be written or overwritten
  if (write) {
    lt1 = paste0(lt.name, "$figures[['", figure.name, "']]")

    eval(parse(text = paste0(lt1, "= list()")), envir = envir)
    eval(parse(text = paste0(lt1, " %<% ", data.name)), envir = envir)

    # Case when data needs to be overwritten
    if (compare) {
      data.same = identical(env.stored[[lt.name]]$figures[[figure.name]][[data.name]], data)
    } else {
      data.same = FALSE
    }

    # Implementation
    # Case: No need to write!
    if (compare & data.same) {
      catn("    figure data is identical to stored data!",
           color = "blue", console = console)
      # Case: Data needs to be freshly saved.
    } else {
      catn("    Saving:", color = "blue", newline = F, console = console)
      catn(filename.n, color = "green", console = console)
      write_csv.adv(data = data,
                    file.name = filename.n,
                    fun_family = fun_family,
                    ...)
      eval(parse(text = "save(" %+% lt.name %+% ", file = '" %+% stored.list.file.name %+% "', envir = envir)"))
    }
  }
  return(invisible())
}