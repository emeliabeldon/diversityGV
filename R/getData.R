getData <- function(x) {
  dataTable <<- read_csv(x, col_types =cols() )

  print("Your new table is now called: dataTable")

  View(dataTable)
}

