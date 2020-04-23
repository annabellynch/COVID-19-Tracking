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


# -------------------------------------------------------- #
# -------------------- CODE FOR MAP ---------------------- #
# -------------------------------------------------------- #

library(plotly)
library(dplyr)

# Import data file
df <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-22-2020.csv")

# --------------------- Cleaning Data ------------------------- #
# Alternate NA values to 0
df[is.na(df)] <- 0
# Remove provinces/states that are not part of the main 50
df <- df[!(df$Province_State == "Diamond Princess"),]
df <- df[!(df$Province_State == "Grand Princess"),]
df <- df[!(df$Province_State == "Guam"),]
df <- df[!(df$Province_State == "Northern Mariana Islands"),]
df <- df[!(df$Province_State == "Puerto Rico"),]
df <- df[!(df$Province_State == "Recovered"),]
df <- df[!(df$Province_State == "Virgin Islands"),]
df <- df[!(df$Province_State == "American Samoa"),]
df <- df[!(df$Province_State == "District of Columbia"),]
# Reset index after removing rows
row.names(df) <- NULL
# Create a column for information displayed when hovering over state
df$hover <- with(df, paste(Province_State))
# Add state abbreviations to data frame
Abbreviations <- factor(c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
                   "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
                   "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
                   "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
                   "WI", "WY"))
df <- cbind(df, Abbreviations)
df <- select(df, Abbreviations, Province_State:hover)
# --------------------------------------------------------------#

# --------------------- Creating Map -------------------------- #
# Give state boundaries a white color
bound_color <- list(color = toRGB("white"), width = 2)
# Specify map details
map_details <- list(
  scope = "usa",
  projection = list(type = "albers usa"),
  showlakes = TRUE,
  lakecolor = toRGB("white")
)
# Plot map
tracker <- plot_geo(df, locationmode = "USA-states")
tracker <- tracker %>% add_trace(
  z = ~Confirmed, text = ~hover, locations = Abbreviations,
  color = ~Confirmed, colors = "Reds"
)
tracker <- tracker %>% colorbar(title = "Thousands of Cases")
tracker <- tracker %>% layout(
  title = "COVID-19 US Confirmed Cases",
  geo = map_details
)

tracker
