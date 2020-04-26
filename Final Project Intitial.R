library("readxl")
library(tidyverse)
library(xlsx)
library(ggplot2)

#reading the csv file
daily = read.csv("https://covidtracking.com/api/v1/states/daily.csv")

current = read.csv("C:\\Users\\student\\Documents\\Semester 4\\SYS 2202\\Final Project\\4.23 states_current.csv")

state_pop = read.csv("C:\\Users\\student\\Documents\\Semester 4\\SYS 2202\\Final Project\\State Population Data 2019.csv")

# CLEANING

# Counting the number of NA values in each column
na_count <-sapply(daily, function(y) sum(length(which(is.na(y)))))
na_count

# Converting the values in state from a factor to a character
daily$state = as.character(daily$state)
current$state = as.character(current$state)
state_pop$NAME = as.character(state_pop$NAME)

# Converting the date from a factor to a date
#daily$date = as.Date(daily$date, format = "%m/%d/%Y")
daily <- transform(daily, date = as.Date(as.character(date), "%Y%m%d"))

# Checking that the conversion was successful
str(daily)
str(state_pop)


# Removing unnecessary columns
daily = select(daily, date, state, positive, negative, pending, death, total, totalTestResults)
current = select(current,state, positive, negative, pending, death, total, totalTestResults)
state_pop = select(state_pop, NAME, POPESTIMATE2019)

#removing unecessary rows
state_pop = subset(state_pop, NAME != 'United States' & NAME !='Puerto Rico Commonwealth'
                   & NAME != 'District of Columbia')

state_pop$NAME = state.abb[which(state_pop$NAME == state.name)] 
state_pop = arrange (state_pop, NAME)
names(state_pop)[1]<- 'state'

current = subset(current, state != 'DC')

current = left_join(current, state_pop)
names(current)[8] <- 'pop'

#creating a column for percentage of population tested
current$percent_tests = current$totalTestResults / current$pop

#creating a new dataframe by highest percentage of population tested
max_test_pop = current[order(current$percent_tests, decreasing = TRUE),]
max_test_pop = select(max_test_pop, state, percent_tests)

# Exporting as a csv
#write.xlsx(max_test_pop, "C:\\Users\\student\\Documents\\Semester 4\\SYS 2202\\Final Project\\Percent Population Tested.xlsx")


#trying to refactor
max_test_pop$percent_tests <- as.vector(max_test_pop$percent_tests) #get rid of factors
max_test_pop$percent_tests = factor(max_test_pop$percent_tests,max_test_pop$percent_tests) #add ordered factors back


#graphing percent tests
test_pop_plot = ggplot(data=max_test_pop) + geom_point(mapping = aes(x=state, y=percent_tests))
test_pop_plot

# Replacing null values with zero
daily[is.na(daily)] <- 0
current[is.na(current)] <- 0

# Exporting as a csv
#write.xlsx(daily, "C:\\Users\\student\\Documents\\Semester 4\\SYS 2202\\Final Project\\cleaned daily.xlsx")

# INDIVIDUAL STATES
# Subsetting into state dataframes
ny <- daily[ which(daily$state=='NY'), ]
nj <- daily[ which(daily$state=='NJ'), ]
ma <- daily[ which(daily$state=='MA'), ]
mi <- daily[ which(daily$state=='MI'), ]
va <- daily[ which(daily$state=='VA'), ]


#Removing US Territories
daily <- subset(daily, state != 'MP' & state!='AS' & state!='PR' & state!='GU' & state!='VI',)
current <- subset(current, state != 'MP' & state!='AS' & state!='PR' & state!='GU' & state!='VI',)

# FEATURE EXTRACTION: percent of positive tests
current$percent_pos <- current$positive / current$totalTestResults

# Sorting current by number of positive cases
max_pos <- current[order(current$positive, decreasing = TRUE),]

#Sorting by largest percent of positive test
max_ratio <- current[order(current$percent_pos, decreasing = TRUE),]


# TOP STATES
#Subsetting by the top positive states
daily_top_states <- subset(daily, state == 'NY' | state == 'NJ' | state == 'CT' | state == 'LA' |
                             state == 'MA'| state=='MI' | state == 'MI' |
                             state == 'CA' | state == 'PA' | state == 'GA' | state == 'FL',)


# DATA VISUALIZATIONS


nyplot = options(scipen=999)ggplot(data = ny) + geom_line(mapping = aes(x=date, y=positive), colour = 'red')+
  geom_line(mapping = aes(x=date,y=negative), colour='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), colour='black')+
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for New York')+
  scale_color_discrete(name = "Test Results", labels = c("total", "negative","positive"))
nyplot

njplot = ggplot(data = nj) + geom_line(mapping = aes(x=date, y=positive), color = 'red')+
  geom_line(mapping = aes(x=date,y=negative), color='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), color='black')+ylim(0,600000)+
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for New Jersey')
njplot

maplot = ggplot(data = ma) + geom_line(mapping = aes(x=date, y=positive), color = 'red')+
  geom_line(mapping = aes(x=date,y=negative), color='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), color='black')+ ylim(0,600000)+
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for Massachusets')
maplot

miplot = ggplot(data = mi) + geom_line(mapping = aes(x=date, y=positive), color = 'red')+
  geom_line(mapping = aes(x=date,y=negative), color='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), color='black')+ ylim(0,600000) +
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for Michigan')
miplot

vaplot = ggplot(data = va) + geom_line(mapping = aes(x=date, y=positive), color = 'red')+
  geom_line(mapping = aes(x=date,y=negative), color='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), color='black')+ ylim(0,600000)+
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for Virginia')
vaplot







plot1 = ggplot(data=daily)+ geom_line(mapping = aes(x=date, y=positive, color = state))+ylab("Positive Tests")+
  xlab("Date")+ggtitle("Positive Tests by State")
plot1

plot2 = ggplot(data=daily)+ geom_line(mapping = aes(x=date, y=positive, color = state))+ylab("Positive Tests")+
  xlab("Date")+ggtitle("Positive Tests by State")+ylim(0,25000)+geom_smooth(mapping = aes(x=date, y=positive))
plot2

daily_top_plot = ggplot(data=daily_top_states)+ geom_line(mapping = aes(x=date, y=positive, color = state))+ylab("Positive Tests")+
  xlab("Date")+ggtitle("Positive Tests by Top States")
daily_top_plot

daily_top_plot2 =  ggplot(data=daily_top_states)+ geom_line(mapping = aes(x=date, y=positive, color = state))+ylab("Positive Tests")+
  xlab("Date")+ggtitle("Positive Tests by Top States")+ylim(0,25000)
daily_top_plot2
