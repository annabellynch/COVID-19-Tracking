# COVID-19-Tracking

library("readxl")
library(tidyverse)
library(xlsx)
library(ggplot2)
library(data.table)


#reading the csv file
daily <- read.csv("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv")
View(daily)



# CLEANING

# Counting the number of NA values in each column
na_count <-sapply(daily, function(y) sum(length(which(is.na(y)))))
na_count

# Converting the values in state from a factor to a character
daily$state = as.character(daily$state)

# Converting the date from a factor to a date
daily <- transform(daily, date = as.Date(as.character(date), "%Y%m%d"))


# Removing unnecessary columns
daily = select(daily, date, state, positive, negative, pending, death, total, totalTestResults)

#Converts "date" column to date format
daily <- transform(daily, date = as.Date(as.character(date), "%Y%m%d"))

# Chose most recent date of data available
daily = subset(daily, date == "2020-04-26")
# Removing unwanted US territoires (kept 50 states)
daily = daily[-c(4, 9, 13, 28, 43, 51), ]

# Replace NA values in pending column with zeros
daily[is.na(daily)] <- 0


# DATA VISUALIZATIONS

plot1 = ggplot(data=daily)+ geom_line(mapping = aes(x=date, y=positive, color = state))+ylab("Positive Tests")+
  xlab("Date")+ggtitle("Positive Tests by State")
plot1

plot2 = ggplot(data=daily)+ geom_line(mapping = aes(x=date, y=positive, color = state))+ylab("Positive Tests")+
  xlab("Date")+ggtitle("Positive Tests by State")+ylim(0,25000)
plot2

# CREATING DATA TABLE
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

# Feature Extraction
daily = mutate(daily, percent_p = Positive/Total_Tests, percent_n = Negative/Total_Tests)

# SIDE-BY-SIDE TEST RESULTS GRAPH
barplot(Test_Results, main="COVID Test Results by State (04/07/20)", col=c("blue","red","green"), beside=TRUE, legend=rownames(Test_Results), cex.names=0.5)

# STACKED TEST RESULTS GRAPH
barplot(Test_Results, main="COVID Test Results", col=c("red","blue","yellow"), legend=rownames(Test_Results), position=stack, cex.names=0.5)

# POPULATION DATA
library(readxl)
state_population = read.csv("State Population Data 2019.csv")
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
ggplot(daily) + geom_col(aes(x=reorder(state,-percent_tested), y=percent_tested, fill=population)) + scale_fill_gradient(low="blue", high="red") + ggtitle("COVID-19 Tests Compared to State Population") + theme(plot.title = element_text(hjust = 0.5)) + xlab("US States") + ylab("Percentage of State Population Tested") + labs(fill = "Population\n(in millions)")

# Add new percent positive columns to daily
daily = mutate(daily, percent_p = (positive/totalTestResults)*100)
daily = mutate(daily, positive_thous = positive/1000)

# GRAPH NUM OF POSITIVE / TOTAL TESTS
options(scipen=999)
ggplot(daily) + geom_col(aes(x=reorder(state,-percent_p), y=percent_p, fill=positive_thous)) + scale_fill_gradient(low="red", high="yellow") + ggtitle("COVID-19 Positive Tests Compared to Total Tests") + theme(plot.title = element_text(hjust = 0.5)) + xlab("US States") + ylab("Percentage of Positive Tests") + labs(fill = "# of Positive Tests\n(in thousands)")


# TOP STATES DATA AND GRAPHS

# INDIVIDUAL STATES
# Subsetting into state dataframes
ny <- daily[ which(daily$state=='NY'), ]
nj <- daily[ which(daily$state=='NJ'), ]
ma <- daily[ which(daily$state=='MA'), ]
il <- daily[ which(daily$state=='IL'), ]
ca <- daily[ which(daily$state=='CA'), ]

# DATA VISUALIZATIONS

options(scipen=999)
nyplot = ggplot(data = ny) + geom_line(mapping = aes(x=date, y=positive), colour = 'red')+
  geom_line(mapping = aes(x=date,y=negative), colour='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), colour='black')+
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for New York')+ theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Test Results", labels = c("total", "negative","positive"))
nyplot

njplot = ggplot(data = nj) + geom_line(mapping = aes(x=date, y=positive), color = 'red')+
  geom_line(mapping = aes(x=date,y=negative), color='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), color='black')+ylim(0,800000)+
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for New Jersey')+theme(plot.title = element_text(hjust = 0.5))
njplot

maplot = ggplot(data = ma) + geom_line(mapping = aes(x=date, y=positive), color = 'red')+
  geom_line(mapping = aes(x=date,y=negative), color='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), color='black')+ ylim(0,800000)+
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for Massachusets')+theme(plot.title = element_text(hjust = 0.5))
maplot

ilplot = ggplot(data = il) + geom_line(mapping = aes(x=date, y=positive), color = 'red')+
  geom_line(mapping = aes(x=date,y=negative), color='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), color='black')+ ylim(0,800000) +
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for Illinois')+theme(plot.title = element_text(hjust = 0.5))
ilplot

caplot = ggplot(data = ca) + geom_line(mapping = aes(x=date, y=positive), color = 'red')+
  geom_line(mapping = aes(x=date,y=negative), color='blue')+ 
  geom_line(mapping = aes(x=date, y=totalTestResults), color='black')+ ylim(0,800000)+
  xlab('Date')+ylab('Number of Tests')+ggtitle('Number of Tests for California')+theme(plot.title = element_text(hjust = 0.5))
caplot




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

## RSHINY APPLICATION 

#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
#import data
map_daily = read.csv("https://covidtracking.com/api/v1/states/daily.csv")

# Converting the values in state from a factor to a character
map_daily$state = as.character(map_daily$state)

# Converting the date from a factor to a date
#map_daily$date = as.Date(map_daily$date, format = "%m/%d/%Y")
map_daily <- transform(map_daily, date = as.Date(as.character(date), "%Y%m%d"))

# Removing unnecessary columns
map_daily = select(map_daily, date, state, positive, negative, pending, death, total, totalTestResults)

#https://www.kaggle.com/washimahmed/usa-latlong-for-state-abbreviations/data#statelatlong.csv
# State Latitude and Longitude 
state_lat_long = read.csv("C:\\Users\\student\\Downloads\\statelatlong.csv")

# Removing unnecessary columns
state_lat_long = select(state_lat_long, State, Latitude, Longitude)
names(state_lat_long)[1]<- 'state'

# Joining the two dataframes
map_daily = left_join(map_daily, state_lat_long)
names(map_daily)[9]<- 'latitude'
names(map_daily)[10]<- 'longitude'

cv_min_date = min(map_daily$date)
current_date = max(map_daily$date)
# creat variable for today's data
cv_today = subset(map_daily, date==current_date) 

#creating the ui
ui <- fluidPage(
  mainPanel( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap"), 
    #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
    absolutePanel(top = 60, left = 20, 
                  sliderInput("plot_date",
                              label = h5("Select mapping date"),
                              min = as.Date(cv_min_date,"%Y-%m-%d"),
                              max = as.Date(current_date,"%Y-%m-%d"),
                              value = as.Date(current_date),
                              timeFormat = "%d %b", 
                              animate=animationOptions(interval = 3000, loop = FALSE))
    )
  ))

server <- function(input, output, session) {
  #define the color pallate for the magnitidue of the earthquake
  pal <- colorNumeric(
    palette = c('red'),
    domain = map_daily$positive)
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(map_daily) %>% 
      setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~(cv_today$positive)^(1/4), popup = ~as.character(cv_today$positive), 
                 label = ~as.character(paste0("Positive Cases: ", sep = " ", cv_today$positive)), 
                 color = ~pal(cv_today$positive), fillOpacity = 0.5)
  })
  
  #reactive_db = reactive({
    #map_daily %>% filter(date == input$plot_date)
    # reactive = cv_cases %>% filter(date == "2020-04-25")
 # })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addCircles(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~sqrt((cv_today$positive)), popup = ~as.character(cv_today$positive), 
                 label = ~as.character(paste0("Positive Cases: ", sep = " ", cv_today$positive)), 
                 color = ~pal(cv_today$positive), fillOpacity = 0.5)
  })
  
  
  
  #observe({
    #date <- as.Date(paste0("2020-04-", input$n))
    #updateDateInput(session, "inDate",
                    #label = paste("Date label", input$n),
                    #value = date,
                    #min   = date - 3,
                    #max   = date + 3
    #)
  #})
  
}

shinyApp(ui, server)
