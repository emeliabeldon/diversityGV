#Set function with one argument, x, to receive table name
measureData <- function(x) {

  #Set a function to format decimal numbers
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

  #Assign data table received in the function to dataTable
  dataTable <<- x

  #Set up a duplicate table to calculate the % of each race/ethnicity for GV formula
  testTable <- dataTable

  #Calculate the percentage of each race/ethnicity
  testTable <- mutate(testTable, American_Indian_Or_Alaska_Native_Total = as.numeric(American_Indian_Or_Alaska_Native_Total/Total_Number_of_Students))
  testTable <- mutate(testTable, Asian_Total = as.numeric(Asian_Total/Total_Number_of_Students))
  testTable <- mutate(testTable, Black_Or_African_American_Total = as.numeric(Black_Or_African_American_Total/Total_Number_of_Students))
  testTable <- mutate(testTable, Hispanic_Total = as.numeric(Hispanic_Total/Total_Number_of_Students))
  testTable <- mutate(testTable, Native_Hawaiian_Or_Other_Pacific_Islander = as.numeric(Native_Hawaiian_Or_Other_Pacific_Islander/Total_Number_of_Students))
  testTable <- mutate(testTable, White_Total = as.numeric(White_Total/Total_Number_of_Students))
  testTable <- mutate(testTable, Two_Or_More_Races_Total = as.numeric(Two_Or_More_Races_Total/Total_Number_of_Students))

  #Set another table with the percentages from testTable to calculate the GV values
  sampleTable <- select(testTable, American_Indian_Or_Alaska_Native_Total, Asian_Total, Black_Or_African_American_Total,
                Hispanic_Total,Native_Hawaiian_Or_Other_Pacific_Islander,White_Total,Two_Or_More_Races_Total)

  #Apply the GV formula to the percentages and have the solutions added as a new column
  #in dataTable named GV_Value
  #The line also sets the solutions to come as numbers instead of characters
  dataTable$GV_Value <<- as.numeric(apply(sampleTable, 1, function(t) specify_decimal((1-sum(t^2)), 3)))

  #Make a new table named rankedTable with ID, Institution_Name, and GV_Value to set rankings.
  rankedTable <<- select(dataTable, ID, `Institution_Name`, GV_Value)

  #Arrange the rankedTable from highest GV value to lowest GV value and add a new column for the ranks starting from 1 going down to x number of institutions.
  rankedTable <<- arrange(rankedTable, desc(GV_Value)) %>% mutate(Rank_of_GV_Value = 1:nrow(rankedTable))

  #Print new data table name
  print("The table to show you the rank of the Insitutions is: rankedTable")

  #Print statement for statistical summary of GV Values
  print("The data printed below is the statistical summary of GV_Value variable from rankedTable.")

  #Set the format to 2 digits
  options(digits=2)

  #Print the statistical summary of GV Values using pastecs
  print(stat.desc(rankedTable$GV_Value, norm = FALSE))

  #Print next instructions
  print("Use visualizeData() to show your targeted institution rankings in a lollipop graph. Insert rankedTable and your institution name in parenthesis.
  Ex. visuazlideData(rankedTable, 'Gallaudet University')")

  #Open and view rankedTable
  View(rankedTable)
}
