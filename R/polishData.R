polishData <- function(x) {

  dataTable1 <<- select(x, -contains("men"), -UnitID, -contains("unknown"), -contains("alien"))

  dataTable1 <<- rename(dataTable1, 'Total_Number_of_Students' = 'Grand total (EF2018A  All students total)', 'American_Indian_Or_Alaska_Native_Total' = 'American Indian or Alaska Native total (EF2018A  All students total)',
                        'Asian_Total' = 'Asian total (EF2018A  All students total)', 'Black_Or_African_American_Total' = 'Black or African American total (EF2018A  All students total)',
                        'Hispanic_Total' = 'Hispanic total (EF2018A  All students total)', 'Native_Hawaiian_Or_Other_Pacific_Islander'
                        = 'Native Hawaiian or Other Pacific Islander total (EF2018A  All students total)', 'White_Total' = 'White total (EF2018A  All students total)',
                        'Two_Or_More_Races_Total' = 'Two or more races total (EF2018A  All students total)')

  dataTable1 <<- mutate(dataTable1, "ID" = row_number())

  dataTable1 <<-  dataTable1[,c(10,1,2,3,4,5,6,7,8,9)]

  dataTable1 <<- mutate(dataTable1, Total_Number_of_Students = rowSums(dataTable1[,4:10]))

  nameTable <- dataTable1[(dataTable1$Total_Number_of_Students==0),] ['Institution Name']
  institutionName <- toString(nameTable$`Institution Name`)
  print("These institution(s):")
  print(institutionName)
  print("will be removed from the data set due insufficent data.")

  #removes rows with total number = 0
  dataTable1 <<- dataTable1[!(dataTable1$Total_Number_of_Students == 0), ]

  print("Your new table is now called: dataTable1")

  View(dataTable1)
}


