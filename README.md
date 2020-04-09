# COVID-19-Tracking

library("readxl")
library(tidyverse)
library(xlsx)

#reading the csv file
daily = read.csv("C:\\Users\\student\\Documents\\Semester 4\\SYS 2202\\Final Project\\4.8 states daily.csv")



# CLEANING

# Counting the number of NA values in each column
na_count <-sapply(daily, function(y) sum(length(which(is.na(y)))))
na_count

# Converting the values in state from a factor to a character
daily$state = as.character(daily$state)

# Converting the date from a factor to a date
daily$date = as.Date(daily$date, format = "%m/%d/%Y")

# Checking that the conversion was successful
str(daily)

#attempting to group by state --> didn't work for some reason
states = group_by(daily, state)

# Removing unnecessary columns
daily = select(daily, date, state, positive, negative, pending, death, total, totalTestResults)

daily[is.na(daily)] <- 0


# Exporting as a csv
write.xlsx(daily, "C:\\Users\\student\\Documents\\Semester 4\\SYS 2202\\Final Project\\cleaned daily.xlsx")


# DATA VISUALIZATIONS

plot1 = ggplot(data=daily)+ geom_line(mapping = aes(x=date, y=positive, color = state))+ylab("Positive Tests")+
  xlab("Date")+ggtitle("Positive Tests by State")
plot1

plot2 = ggplot(data=daily)+ geom_line(mapping = aes(x=date, y=positive, color = state))+ylab("Positive Tests")+
  xlab("Date")+ggtitle("Positive Tests by State")+ylim(0,25000)
plot2
