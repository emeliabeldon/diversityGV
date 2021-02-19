measureData <- function(x) {

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

testTable <- dataTable1

testTable <- mutate(testTable, American_Indian_Or_Alaska_Native_Total = as.numeric(American_Indian_Or_Alaska_Native_Total/Total_Number_of_Students))
testTable <- mutate(testTable, Asian_Total = as.numeric(Asian_Total/Total_Number_of_Students))
testTable <- mutate(testTable, Black_Or_African_American_Total = as.numeric(Black_Or_African_American_Total/Total_Number_of_Students))
testTable <- mutate(testTable, Hispanic_Total = as.numeric(Hispanic_Total/Total_Number_of_Students))
testTable <- mutate(testTable, Native_Hawaiian_Or_Other_Pacific_Islander = as.numeric(Native_Hawaiian_Or_Other_Pacific_Islander/Total_Number_of_Students))
testTable <- mutate(testTable, White_Total = as.numeric(White_Total/Total_Number_of_Students))
testTable <- mutate(testTable, Two_Or_More_Races_Total = as.numeric(Two_Or_More_Races_Total/Total_Number_of_Students))

sampleTable <- select(testTable, American_Indian_Or_Alaska_Native_Total, Asian_Total, Black_Or_African_American_Total,
                Hispanic_Total,Native_Hawaiian_Or_Other_Pacific_Islander,White_Total,Two_Or_More_Races_Total)

dataTable1$GV_Index <<- apply(sampleTable, 1, function(t) specify_decimal((1-sum(t^2)), 3))

rankedTable <<- select(dataTable1, ID,`Institution Name`, GV_Index)
rankedTable <<- arrange(rankedTable, desc(GV_Index)) %>% mutate(Rank_of_GV_Index = 1:nrow(rankedTable))

print("The table to show you the rank of the Insitutions is: rankedTable")

View(rankedTable)
}

#dataTest <- dataTable1

#for (i in 4:ncol(dataTest)) {
  #print(colnames(dataTest[i]))
  #dataTest <- mutate(dataTest, colnames(dataTest[i]) = (colnames(dataTest[i])/Total_Number_of_Students))
#}
