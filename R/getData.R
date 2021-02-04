getData <- function(x) {
  dataTable <- readr::read_csv(x)
  View(dataTable)
}
