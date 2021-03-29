#Set function with one argument, x, to receive file name in parenthesis
getData <- function(x) {

  #Suppress any warnings or messages about columns (auto and does not affect data) and assigns the file
  #to table "newData"
  newData <<- suppressWarnings(read_csv(x, col_types =cols()))

  #Remove extra column, "X32", that is added when read file
  newData <<- select(newData, -X32)

  #Print table name and next instructions
  print("Your data table is now called: newData")
  print("Use polishData() function with newData to clean up the data for calculations.")

  #Open and view table
  View(newData)
}


