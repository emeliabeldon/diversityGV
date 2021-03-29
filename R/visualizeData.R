#Set function with two argument, x and y, to receive table name and institution name in parenthesis
visualizeData <- function(x, y) {

  #Set a table with Institution_Name and Rank_of_GV_Value and arrange them by the rank of GV values
  data <- select(x, Institution_Name, Rank_of_GV_Value) %>% arrange(Rank_of_GV_Value) %>% mutate(Institution_Name = factor(Institution_Name, Institution_Name))

  #Plot a lollipop graph with x as Institution_Name,
  #and y as Rank_of_GV_Value
  ggplot(data, aes(x=Institution_Name, y=Rank_of_GV_Value)) +
  #Set points to be grey but orange and a few sizes bigger if the institution name
  #matches the institution name from the function argument
  geom_point( color=ifelse(data$Institution_Name %in% c(y), "orange", "grey"),
              size=ifelse(data$Institution_Name %in% c(y), 3, 1) ) +
  #Set lines to be grey but orange and a few sizes bigger if the institution name
  #matches the institution name from the function argument
  geom_segment( aes(x=Institution_Name, xend=Institution_Name, y=0, yend=Rank_of_GV_Value),
                color=ifelse(data$Institution_Name %in% c(y), "orange", "grey"),
                size=ifelse(data$Institution_Name %in% c(y), 1, 0.5) ) +
  #Flip the graph to horizontal
  coord_flip() +
  #Set the y axis text to be smaller so the institution names can be clear
  theme(axis.text.y = element_text(size = 5)) +
  #Set theme and no legends
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none") +
  #Set title and labels of the graph
  labs(x = "Instituition Names", y = "GV Rank", title = "The Rankings of Diversity in the Institutions of the Dataset") +
  #Add annotation for the rank of the institution from the function argument
  annotate("text", x = grep(y, data$Institution_Name),
           y = data$Rank_of_GV_Value[which(data$Institution_Name==y)*1.2],
           label = paste(y,"is ranked \n ",
                         data$Rank_of_GV_Value[which(data$Institution_Name==y)], "out of 122 institutions."),
                         color="orange", size=3, fontface="bold", hjust="inward", vjust="inward")
}






