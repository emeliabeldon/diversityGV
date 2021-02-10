polishData <- function(x) {
  dataTable1 <<- select(x, -contains("men"), -UnitID, -contains("unknown"), -contains("alien"))

  dataTable1 <<- rename(dataTable1, 'Total Number of Students' = 'Grand total (EF2018A  All students total)', 'American Indian Or Alaska Native Total' = 'American Indian or Alaska Native total (EF2018A  All students total)',
                        'Asian Total' = 'Asian total (EF2018A  All students total)', 'Black Or African American Total' = 'Black or African American total (EF2018A  All students total)',
                        'Hispanic Total' = 'Hispanic total (EF2018A  All students total)', 'Native Hawaiian Or Other Pacific Islander'
                        = 'Native Hawaiian or Other Pacific Islander total (EF2018A  All students total)', 'White Total' = 'White total (EF2018A  All students total)',
                        'Two Or More Races Total' = 'Two or more races total (EF2018A  All students total)')

  dataTable1 <<- mutate(dataTable1, "ID" = row_number())

  dataTable1 <<-  dataTable1[,c(10,1,2,3,4,5,6,7,8,9)]

  print("Your new table is now called: dataTable1")

  View(dataTable1)
}


