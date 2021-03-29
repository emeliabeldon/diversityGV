#Set function with one argument, x, to receive table name
polishData <- function(x) {

  #Remove columns for gender, UnitID, Unknown Races, and Alien Races
  dataTable <<- select(x, -contains("men"), -contains("UnitID"), -contains("unknown"), -contains("alien"))

  #Rename columns for clarity and next function
  colnames(dataTable) <<- c('Institution_Name','Total_Number_Of_Students', 'American_Indian_Or_Alaska_Native_Total', 'Asian_Total',
                            'Black_Or_African_American_Total','Hispanic_Total', 'Native_Hawaiian_Or_Other_Pacific_Islander', 'White_Total',
                            'Two_Or_More_Races_Total')

  #Update the column with the total number of students to add the number of students per race in the table
  #since gender, UnitID, Unknown Races, and Alien Races were removed
  dataTable <<- mutate(dataTable, Total_Number_of_Students = rowSums(dataTable[,4:10]))

  #Removes rows with total number of students = 0 to prevent miscalculations
  dataTable <<- dataTable[!(dataTable$Total_Number_of_Students == 0), ]

  #Add new column for ID starting at 1 to x number of institutions
  dataTable <<- mutate(dataTable, "ID" = row_number())

  #Rearrange columns to have ID column at the beginning of the table
  dataTable <<-  dataTable[,c(10,1,2,3,4,5,6,7,8,9)]

  #Set a new table for the institutions that will be removed to add to a string for print
  nameTable <- dataTable[(dataTable$Total_Number_of_Students==0),] ['Institution_Name']

  #Add the institution names to a string to print and communicate to the user of elimination
  institutionName <- toString(nameTable$`Institution_Name`)

  #Print the institutions that will be removed from the table
  print("These institution(s):")
  print(institutionName)
  print("will be removed from the data set due to insufficent data.")

  #Print new data table name and next instructions
  print("Your new data table is now called: dataTable")
  print("Use measureData() function with dataTable to measure the GV value of each institution.")

  #Open and view table
  View(dataTable)
}


