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

# CREATING DATA TABLE
# Chose most recent date of data available
daily = subset(daily, date == "2020-04-07")

State <- daily$state
Positive <- daily$positive
Negative <- daily$negative
Pending <- daily$pending
Test_Results = rbind(Positive, Negative, Pending)
colnames(Test_Results) <- daily$state
Test_Results
library(gridExtra)
Test_Results_Table <- tableGrob(Test_Results)
grid.arrange(Test_Results_Table)

# SIDE-BY-SIDE TEST RESULTS GRAPH
barplot(Test_Results, main="COVID Test Results by State (04/07/20)", col=c("blue","red","green"), beside=TRUE, legend=rownames(Test_Results), cex.names=0.5)

# STACKED TEST RESULTS GRAPH
barplot(Test_Results, main="COVID Test Results", col=c("red","blue","yellow"), legend=rownames(Test_Results), position=stack, cex.names=0.5)

# POPULATION DATA
library(readxl)
state_population <- read_excel("State Population Data.xlsx")
View(state_population) 

# Clean data
state_population = select(state_population, -SUMLEV, -REGION, -DIVISION, -STATE, -POPEST18PLUS2019, -PCNT_POPEST18PLUS)
state_population = state_population[-c(1, 10, 53), ]

# Convert state names to abbreviations and sort alphabetically
state_population$NAME = state.abb[which(state.name == state_population$NAME)]
state_population = arrange (state_population, NAME)

# Add new population columns to daily
daily = mutate(daily, population = state_population$POPESTIMATE2019/1000000)
daily = mutate(daily, percent_tested = (daily$totalTestResults/daily$population)*100)

# GRAPH NUM OF TESTS / STATE POPULATION
options(scipen=999)
ggplot(daily) + geom_col(aes(x=State, y=Percent_Tested, fill=population)) + scale_fill_gradient(low="blue", high="red") + ggtitle("COVID-19 Tests Compared to State Population") + theme(plot.title = element_text(hjust = 0.5)) + xlab("US States") + ylab("Percentage of State Population Tested") + labs(fill = "Population\n(in millions)")
?geom_col

# Add new percent positive columns to daily
daily = mutate(daily, percent_p = (positive/totalTestResults)*100)
daily = mutate(daily, positive_thous = positive/1000)

# GRAPH NUM OF POSITIVE / TOTAL TESTS
options(scipen=999)
ggplot(daily) + geom_col(aes(x=State, y=percent_p, fill=positive_thous)) + scale_fill_gradient(low="red", high="yellow") + ggtitle("COVID-19 Positive Tests Compared to Total Tests") + theme(plot.title = element_text(hjust = 0.5)) + xlab("US States") + ylab("Percentage of Positive Tests") + labs(fill = "# of Positive Tests\n(in thousands)")
