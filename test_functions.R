install("tidyverse")
store.table <- function(prefix, filename, data, lt) {
  install("rlang")
  data.name = rlang::enexpr(data)
  lt.name = deparse(substitute(lt))

  if (is.null(names(lt)) || any("tables" != names(lt))) {
    lt$tables = list()
  }

  file.index = grepl(filename, names(lt$tables))

  if (any(file.index)) {
    table.name = names(lt$tables)[file.index]
    filename.n = prefix %+% "_" %+% table.name
    lt1 = paste0(lt.name, "$tables[['", table.name, "']]")
  } else {
    print("else")
    index = pad.00(length(lt$tables) + 1)
    table.name = index %+% "_" %+% filename %+% "_[R].csv"
    filename.n = prefix %+% "_" %+% table.name
    lt1 = paste0(lt.name, "$tables[['", table.name, "']]")
  }

  write_csv.adv(data, file.name = filename.n)
  print(data.name)

  eval(parse(text = paste0(lt1, "= list()")))
  print(paste0(lt1, " %<% ", data.name))
  eval(parse(text = paste0(lt1, " %<% ", data.name)))
  # !!(lt1) %<% data
  return(invisible())
}
l = list()
df = data.frame(x = 1 , y = 1)
store.table(prefix = "01aa", filename = "test", data = df, lt = l)
str.list(l)
store.table(prefix = "01aa", filename = "kachara", data = df, lt = l)
str.list(l)
t = function(x){
  attributes(x)$fille = "name"
}
t(x = df)
