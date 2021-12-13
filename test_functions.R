install("tidyverse")
store.table <- function(prefix, filename, data, lt, check = T,
                        subfolder = "03-Tables") {
  install("rlang")

  data.name = rlang::enexpr(data)
  lt.name = deparse(substitute(lt))

  current.file.name = rev(strsplit(rstudioapi::getSourceEditorContext()$path,
                                   split = "/")[[1]])[1]
  stored.list.file.name = subfolder %/% current.file.name %+%
    "data"

  if (file.exists(stored.list.file.name)) {
    env.stored = new.env()
    load(stored.list.file.name, envir = env.stored)
    catn("Data is already saved in: ", stored.list.file.name, "!")
    compare = TRUE
  }  else {
    compare = FALSE
  }

  if (is.null(names(lt)) || any("tables" != names(lt))) {
    lt$tables = list()
    catn("List tables are empty!")
  }

  file.index = grepl(filename, names(lt$tables))

  if (any(file.index)) {
    table.name = names(lt$tables)[file.index]
    filename.n = prefix %+% "_" %+% table.name

    catn("Tables of the name ", filename, "exists!")
    catn("Data need to rewritten?")
    if (!identical(lt$tables[[table.name]][[data.name]], data)) {
      catn("Yes!")
      write = TRUE
    } else {
      catn("No!")
      write = FALSE
    }
  } else {

    index = pad.00(length(lt$tables) + 1)
    table.name = index %+% "_" %+% filename %+% "_[R].csv"
    filename.n = prefix %+% "_" %+% table.name
    catn("Creating a list of the name:", filename.n, "!")
    write = TRUE
  }
  catn("write:", write)
  if (write) {
    lt1 = paste0(lt.name, "$tables[['", table.name, "']]")

    eval(parse(text = paste0(lt1, "= list()")))
    eval(parse(text = paste0(lt1, " %<% ", data.name)))
    if (compare) {
      identical(env.stored[[lt.name]]$tables[[table.name]][[data.name]], data)
      catn("Table data is identical to stored data!")
    } else {
      catn("Writing and saving, .csv and .Rdata!")
      write_csv.adv(data, file.name = filename.n)
      eval(parse(text = "save(" %+% lt.name %+% ", file = '" %+% stored.list.file.name %+% "')"))
    }
  } else {
    print("old")
  }
  return(invisible())
}



filename = "test"
data = df
data.name = "df"
lt = l
lt.name = "l"
subfolder = "03-Tables"
prefix = "01aa"


l = list()
df = data.frame(x = 1 , y = 1)
store.table(prefix = "01aa", filename = "test", data = df, lt = l)
str.list(l)
df = data.frame(x = 2 , y = 1)

store.table(prefix = "01aa", filename = "test", data = df, lt = l)
str.list(l)


store.table(prefix = "01aa", filename = "kachara", data = df, lt = l)
str.list(l)
t = function(x){
  attributes(x)$fille = "name"
}
t(x = df)
